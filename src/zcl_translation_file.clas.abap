CLASS zcl_translation_file DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING languages TYPE REF TO zcl_translation_languages,
      export_to_excel IMPORTING texts TYPE zif_translatable=>tt_text full_file_path TYPE string,
      import_from_excel IMPORTING full_file_path TYPE string RETURNING VALUE(texts) TYPE zif_translatable=>tt_text raising zcx_translation.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_lang_col,
        sap TYPE sy-langu,
        col TYPE i,
      END OF t_lang_col,
      tt_lang_col TYPE STANDARD TABLE OF t_lang_col WITH EMPTY KEY,
      BEGIN OF t_col_map,
        object_type  TYPE i,
        object_name  TYPE i,
        text_id      TYPE i,
        translations TYPE tt_lang_col,
      END OF t_col_map.

    DATA:
        languages TYPE REF TO zcl_translation_languages.

    METHODS:
      build_export_table IMPORTING texts TYPE zif_translatable=>tt_text RETURNING VALUE(export_table) TYPE REF TO data,
      create_file_table EXPORTING file_table TYPE REF TO data file_table_line TYPE REF TO data,
      create_file_structures EXPORTING struct TYPE REF TO cl_abap_structdescr  table TYPE REF TO cl_abap_tabledescr,
      export_table_to_excel IMPORTING full_file_path TYPE string CHANGING export_table TYPE table,
      import_table_from_excel IMPORTING full_file_path TYPE string RETURNING VALUE(import_table) TYPE REF TO data,
      get_column_mapping IMPORTING headers_row TYPE any RETURNING VALUE(mapping) TYPE t_col_map.
ENDCLASS.



CLASS ZCL_TRANSLATION_FILE IMPLEMENTATION.


  METHOD build_export_table.
    create_file_table( IMPORTING file_table = export_table file_table_line = DATA(file_table_line) ).
    assign_to_table export_table->* <table>.
    ASSIGN file_table_line->* TO FIELD-SYMBOL(<line>).

    LOOP AT texts REFERENCE INTO DATA(single_text).
      <line> = CORRESPONDING #( single_text->* ).
      LOOP AT languages->languages REFERENCE INTO DATA(lang).
        ASSIGN COMPONENT lang->iso OF STRUCTURE <line> TO FIELD-SYMBOL(<line_lang_content>).
        <line_lang_content> = VALUE #( single_text->translations[ sap_lang = lang->sap ]-content OPTIONAL ).
      ENDLOOP.
      APPEND <line> TO <table>.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    me->languages = languages.
  ENDMETHOD.


  METHOD create_file_structures.
    DATA(dummy_text) = VALUE zif_translatable=>t_text( ).

    DATA(components) = VALUE cl_abap_structdescr=>component_table( ).
    APPEND VALUE #( name = 'OBJECT_TYPE' type = CAST #( cl_abap_datadescr=>describe_by_data( dummy_text-object_type ) ) ) TO components.
    APPEND VALUE #( name = 'OBJECT_NAME' type = CAST #( cl_abap_datadescr=>describe_by_data( dummy_text-object_name ) ) ) TO components.
    APPEND VALUE #( name = 'TEXT_ID' type = CAST #( cl_abap_datadescr=>describe_by_data( dummy_text-text_id ) ) ) TO components.

    LOOP AT languages->languages REFERENCE INTO DATA(lang).
      APPEND VALUE #( name = lang->iso type = CAST #( cl_abap_datadescr=>describe_by_name( 'TEXTPOOL-ENTRY' ) ) ) TO components.
    ENDLOOP.

    struct = cl_abap_structdescr=>get( components ).
    table = cl_abap_tabledescr=>get( p_line_type = struct p_key_kind = cl_abap_tabledescr=>keydefkind_empty ).
  ENDMETHOD.


  METHOD create_file_table.
    create_file_structures( IMPORTING struct = DATA(struct) table = DATA(table) ).
    CREATE DATA file_table TYPE HANDLE table.
    CREATE DATA file_table_line TYPE HANDLE struct.
  ENDMETHOD.


  METHOD export_table_to_excel.
    cl_salv_table=>factory( IMPORTING r_salv_table = DATA(salv) CHANGING t_table = export_table ).

    "Longest(?) text is visible after export
    LOOP AT salv->get_columns( )->get( ) REFERENCE INTO DATA(col).
      col->r_column->set_long_text( CONV #( col->columnname ) ).
      col->r_column->set_short_text( space ).
      col->r_column->set_f1_rollname( space ).
      col->r_column->set_medium_text( space ).
    ENDLOOP.

    "Export
    DATA(xml_bytes) = salv->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx ).
    cl_scp_change_db=>xstr_to_xtab( EXPORTING im_xstring = xml_bytes IMPORTING ex_size = DATA(size) ex_xtab = DATA(raw_data) ).
    cl_gui_frontend_services=>gui_download( EXPORTING filename = full_file_path filetype = 'BIN' bin_filesize = size CHANGING data_tab = raw_data ).
  ENDMETHOD.


  METHOD export_to_excel.
    DATA(export_table) = build_export_table( texts = texts ).
    assign_to_table export_table->* <table>.
    export_table_to_excel( EXPORTING full_file_path = full_file_path CHANGING export_table = <table> ).
  ENDMETHOD.


  METHOD get_column_mapping.
    DATA(col_index) = 1.
    DO.
      ASSIGN COMPONENT col_index OF STRUCTURE headers_row TO FIELD-SYMBOL(<header_field>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      CASE <header_field>.
        WHEN 'OBJECT_TYPE'. mapping-object_type = col_index.
        WHEN 'OBJECT_NAME'. mapping-object_name = col_index.
        WHEN 'TEXT_ID'. mapping-text_id = col_index.
        WHEN OTHERS.
          IF line_exists( languages->languages[ KEY iso iso = <header_field> ] ).
            APPEND VALUE #( sap = languages->languages[ KEY iso iso = <header_field> ]-sap col = col_index ) TO mapping-translations.
          ENDIF.
      ENDCASE.

      col_index = col_index + 1.
    ENDDO.
  ENDMETHOD.


  METHOD import_from_excel.
    DATA(import_table) = import_table_from_excel( full_file_path ).
    assign_to_table import_table->* <table>.

    "Assume first row has headers with ISO languages as translations
    DATA(mapping) = get_column_mapping( <table>[ 1 ] ).
    IF mapping-object_name = 0 OR mapping-object_type = 0 OR mapping-text_id = 0 OR lines( mapping-translations ) = 0.
      RAISE EXCEPTION TYPE zcx_translation EXPORTING custom_message = |Incorrect file structure or no languages. Check documentation|.
    ENDIF.

    DELETE <table> INDEX 1.
    LOOP AT <table> ASSIGNING FIELD-SYMBOL(<row>).
      ASSIGN COMPONENT mapping-object_type OF STRUCTURE <row> TO FIELD-SYMBOL(<object_type>).
      IF strlen( condense( <object_type> ) ) = 0.
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT mapping-object_name OF STRUCTURE <row> TO FIELD-SYMBOL(<object_name>).
      ASSIGN COMPONENT mapping-text_id OF STRUCTURE <row> TO FIELD-SYMBOL(<text_id>).

      DATA(text) = VALUE zif_translatable=>t_text( object_name = CONV #( <object_name> )
                                                   object_type = CONV #( <object_type> ) text_id = CONV #( <text_id> ) ).

      LOOP AT mapping-translations REFERENCE INTO DATA(translation).
        ASSIGN COMPONENT translation->col OF STRUCTURE <row> TO FIELD-SYMBOL(<translation>).
        INSERT VALUE #( sap_lang = translation->sap content = <translation> ) INTO TABLE text-translations.
      ENDLOOP.

      APPEND text TO texts.
    ENDLOOP.
  ENDMETHOD.


  METHOD import_table_from_excel.
    "Upload and parse excel file
    DATA it_bin_data TYPE w3mimetabtype.

    cl_gui_frontend_services=>gui_upload( EXPORTING filename = full_file_path filetype = 'BIN' CHANGING data_tab = it_bin_data ).
    DATA(file_as_xstring) = cl_bcs_convert=>solix_to_xstring( it_bin_data ).
    DATA(excel) = NEW cl_fdt_xl_spreadsheet( document_name = full_file_path xdocument = file_as_xstring ).

    "Read first worksheet to table
    excel->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = DATA(worksheets) ).
    LOOP AT worksheets REFERENCE INTO DATA(wks).
      import_table = excel->if_fdt_doc_spreadsheet~get_itab_from_worksheet( wks->* ).
      RETURN.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
