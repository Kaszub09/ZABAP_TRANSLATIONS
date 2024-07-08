CLASS zcl_translatable_data_element DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_translation_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_translatable.

    METHODS:
      constructor IMPORTING data_element TYPE sobj_name.

  PRIVATE SECTION.
    TYPES:
      t_text_type TYPE c LENGTH 3,
      BEGIN OF t_text_id_parsed,
        text_type TYPE t_text_type,
        as4local  TYPE as4local,
        as4vers   TYPE as4vers,
      END OF t_text_id_parsed,
      tt_dd04t       TYPE SORTED TABLE OF dd04t WITH UNIQUE KEY rollname ddlanguage as4local as4vers.

    METHODS:
     get_text_id IMPORTING parsed TYPE t_text_id_parsed RETURNING VALUE(text_id) TYPE string,
     parse_text_id IMPORTING text_id TYPE string RETURNING VALUE(parsed) TYPE t_text_id_parsed,
     get_text IMPORTING text_id TYPE string RETURNING VALUE(text) TYPE REF TO zif_translatable=>t_text,
     modify_translation IMPORTING sap_lang TYPE syst_langu content TYPE textpooltx
          CHANGING translations TYPE zif_translatable=>tt_translation,
     update_dd04l IMPORTING dd04t TYPE tt_dd04t.

    CONSTANTS:
      c_lxe_type TYPE lxeobjtype VALUE 'DTEL',
      BEGIN OF c_text_type,
        description TYPE t_text_type VALUE 'D60',
        short       TYPE t_text_type VALUE 'S10',
        medium      TYPE t_text_type VALUE 'M20',
        long        TYPE t_text_type VALUE 'L40',
        header      TYPE t_text_type VALUE 'H55',
      END OF c_text_type.

    DATA:
     texts TYPE zif_translatable=>tt_text.
ENDCLASS.

CLASS zcl_translatable_data_element IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = data_element.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-data_element.
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    SELECT as4local, as4vers, ddtext, reptext, scrtext_s, scrtext_m, scrtext_l
    FROM dd04t WHERE rollname = @zif_translatable~object_name AND ddlanguage = @sap_lang
    INTO TABLE @DATA(data_element_texts).

    LOOP AT data_element_texts REFERENCE INTO DATA(text).
      DATA(program_text) = get_text( get_text_id( VALUE #( BASE CORRESPONDING #( text->* ) text_type = c_text_type-description ) ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( text->ddtext ) CHANGING translations = program_text->translations ).

      program_text = get_text( get_text_id( VALUE #( BASE CORRESPONDING #( text->* ) text_type = c_text_type-short ) ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( text->scrtext_s ) CHANGING translations = program_text->translations ).

      program_text = get_text( get_text_id( VALUE #( BASE CORRESPONDING #( text->* ) text_type = c_text_type-medium ) ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( text->scrtext_m ) CHANGING translations = program_text->translations ).

      program_text = get_text( get_text_id( VALUE #( BASE CORRESPONDING #( text->* ) text_type = c_text_type-long ) ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( text->scrtext_l ) CHANGING translations = program_text->translations ).

      program_text = get_text( get_text_id( VALUE #( BASE CORRESPONDING #( text->* ) text_type = c_text_type-header ) ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( text->reptext ) CHANGING translations = program_text->translations ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    texts = me->texts.
  ENDMETHOD.

  METHOD get_text_id.
    text_id = |{ parsed-text_type }\|{ parsed-as4local }\|{ parsed-as4vers }|.
  ENDMETHOD.

  METHOD parse_text_id.
    SPLIT text_id AT '|' INTO parsed-text_type parsed-as4local parsed-as4vers.
  ENDMETHOD.

  METHOD zif_translatable~modify_texts.
    LOOP AT new_texts REFERENCE INTO DATA(new_text) USING KEY text_id
    WHERE object_type = zif_translatable~object_type AND object_name = zif_translatable~object_name.
      DATA(program_text) = get_text( new_text->text_id ).
      LOOP AT new_text->translations REFERENCE INTO DATA(new_translation).
        modify_translation( EXPORTING sap_lang = new_translation->sap_lang content = new_translation->content
             CHANGING translations = program_text->translations ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~save_modified_texts.
    DATA dd04t_table TYPE tt_dd04t.

    LOOP AT texts REFERENCE INTO DATA(text).
      DATA(parsed) = parse_text_id( text->text_id ).
      LOOP AT text->translations REFERENCE INTO DATA(translation) WHERE sap_lang = sap_lang.
        DATA(dd04t_row) = REF #( dd04t_table[ rollname = zif_translatable~object_name ddlanguage = sap_lang
                                              as4local = parsed-as4local as4vers = parsed-as4local ] OPTIONAL ).
        IF NOT dd04t_row IS BOUND.
          "Get original from DB, in case not all text are overwritten
          SELECT SINGLE * FROM dd04t
          WHERE rollname = @zif_translatable~object_name AND ddlanguage = @sap_lang AND as4local = @parsed-as4local AND as4vers = @parsed-as4vers
          INTO @DATA(dd04t_db_row).
          IF sy-subrc <> 0.
            "Not found, create new
            dd04t_db_row = VALUE #( rollname = zif_translatable~object_name ddlanguage = sap_lang
                                     as4local = parsed-as4local as4vers = parsed-as4local ).
          ENDIF.

          INSERT dd04t_db_row INTO TABLE dd04t_table REFERENCE INTO dd04t_row.
        ENDIF.

        CASE parsed-text_type.
          WHEN c_text_type-description. dd04t_row->ddtext = translation->content(60).
          WHEN c_text_type-short. dd04t_row->scrtext_s = translation->content(10).
          WHEN c_text_type-medium. dd04t_row->scrtext_m = translation->content(20).
          WHEN c_text_type-long. dd04t_row->scrtext_l = translation->content(40).
          WHEN c_text_type-header. dd04t_row->reptext = translation->content(55).
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    MODIFY dd04t FROM TABLE @dd04t_table.
    update_dd04l( dd04t_table ).

    zcl_translation_factory=>get_lxe_log( )->update_lxe_log( VALUE #( (
        objname = zif_translatable~object_name objtype = c_lxe_type targlng = sap_lang ) ) ).
  ENDMETHOD.

  METHOD get_text.
    text = REF #( texts[ KEY id_only text_id = text_id ] OPTIONAL ).
    IF NOT text IS BOUND.
      INSERT VALUE #( object_type = zif_translatable~object_type object_name = zif_translatable~object_name
      text_id = text_id ) INTO TABLE texts REFERENCE INTO text.
    ENDIF.
  ENDMETHOD.

  METHOD modify_translation.
    DATA(translation) = REF #( translations[ sap_lang = sap_lang ] OPTIONAL ).
    IF NOT translation IS BOUND.
      INSERT VALUE #( sap_lang = sap_lang ) INTO TABLE translations REFERENCE INTO translation.
    ENDIF.
    translation->content = content.
  ENDMETHOD.

  METHOD update_dd04l.
    "Update texts lengths, in case some new text are longer
    SELECT * FROM dd04l
    FOR ALL ENTRIES IN @dd04t
    WHERE rollname = @dd04t-rollname AND as4local = @dd04t-as4local AND as4vers = @dd04t-as4vers
    INTO TABLE @DATA(dd04l_tab).

    LOOP AT dd04l_tab REFERENCE INTO DATA(dd04l_row).
      LOOP AT dd04t REFERENCE INTO DATA(dd04t_row)
      WHERE rollname = dd04l_row->rollname AND as4local = dd04l_row->as4local AND as4vers = dd04l_row->as4vers.
        IF strlen( dd04t_row->reptext ) > dd04l_row->headlen.
          dd04l_row->headlen = strlen( dd04t_row->reptext ).
        ENDIF.
        IF strlen( dd04t_row->scrtext_s ) > dd04l_row->scrlen1.
          dd04l_row->scrlen1 = strlen( dd04t_row->scrtext_s ).
        ENDIF.
        IF strlen( dd04t_row->scrtext_m ) > dd04l_row->scrlen2.
          dd04l_row->scrlen2 = strlen( dd04t_row->scrtext_m ).
        ENDIF.
        IF strlen( dd04t_row->scrtext_l ) > dd04l_row->scrlen3.
          dd04l_row->scrlen3 = strlen( dd04t_row->scrtext_l ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    MODIFY dd04l FROM TABLE @dd04l_tab.
  ENDMETHOD.

ENDCLASS.
