CLASS zcl_translatable_table DEFINITION
 PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_translatable.
    METHODS:
      constructor IMPORTING program TYPE sobj_name.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      t_table TYPE c LENGTH 5,
      BEGIN OF  t_text_id_parsed,
        tab        TYPE t_table,
        tabname    TYPE tabname,
        fieldname  TYPE fieldname,
        ddlanguage TYPE ddlanguage,
        as4local   TYPE as4local,
        as4vers    TYPE as4vers,
      END OF  t_text_id_parsed.

    METHODS:
      get_text_id IMPORTING parsed TYPE  t_text_id_parsed RETURNING VALUE(text_id) TYPE string,
      parse_text_id IMPORTING text_id TYPE string EXPORTING parsed TYPE  t_text_id_parsed,
      get_text IMPORTING text_id TYPE string RETURNING VALUE(text) TYPE REF TO zif_translatable=>t_text,
      modify_translation IMPORTING sap_lang TYPE syst_langu content TYPE textpooltx
                         CHANGING  translations TYPE zif_translatable=>tt_translation,
      update_lxe_log IMPORTING sap_lang TYPE sy-langu descriptions TYPE abap_bool keys TYPE abap_bool.

    CONSTANTS:
      BEGIN OF c_lxe_type,
        descriptions TYPE lxeobjtype VALUE 'TABT',
        keys         TYPE lxeobjtype VALUE 'BEZD',
      END OF c_lxe_type,
      BEGIN OF c_table,
        dd02t TYPE t_table VALUE 'DD02T',
        dd03t TYPE t_table VALUE 'DD03T',
        dd08t TYPE t_table VALUE 'DD08T',
      END OF c_table.

    DATA:
      texts    TYPE zif_translatable=>tt_text.
ENDCLASS.



CLASS zcl_translatable_table IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = program.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-program.
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    DATA(empty_as4vers) = VALUE as4vers( ).
    SELECT @c_table-dd08t AS tab, tabname, fieldname, ddlanguage, as4local, as4vers, ddtext
    FROM dd08t WHERE tabname = @zif_translatable~object_name AND ddlanguage = @sap_lang
    UNION
    SELECT @c_table-dd02t AS tab, tabname,@space AS fieldname, ddlanguage, as4local, as4vers, ddtext
    FROM dd02t WHERE tabname = @zif_translatable~object_name AND ddlanguage = @sap_lang
    UNION
    SELECT @c_table-dd02t AS tab,tabname, fieldname, ddlanguage, as4local, @empty_as4vers AS as4vers, ddtext
    FROM dd03t WHERE tabname = @zif_translatable~object_name AND ddlanguage = @sap_lang
    INTO TABLE @DATA(table_texts).

    LOOP AT table_texts REFERENCE INTO DATA(text).
      DATA(program_text) = get_text( get_text_id( VALUE #( tab = text->tab tabname = text->tabname fieldname = text->fieldname
                                                  ddlanguage = text->ddlanguage as4local = text->as4local as4vers = text->as4vers ) ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( text->ddtext ) CHANGING translations = program_text->translations ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    texts = me->texts.
  ENDMETHOD.

  METHOD get_text_id.
    text_id = |{ parsed-tab }\|{ parsed-tabname }\|{ parsed-fieldname }\|{ parsed-ddlanguage }\|{ parsed-as4local }\|{ parsed-as4vers }|.
  ENDMETHOD.

  METHOD parse_text_id.
    SPLIT text_id AT '|' INTO parsed-tab parsed-tabname parsed-fieldname parsed-ddlanguage parsed-as4local parsed-as4vers.
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
    DATA dd02t_table TYPE STANDARD TABLE OF dd02t WITH EMPTY KEY.
    DATA dd03t_table TYPE STANDARD TABLE OF dd03t WITH EMPTY KEY.
    DATA dd08t_table TYPE STANDARD TABLE OF dd08t WITH EMPTY KEY.

    DATA(lxe_descriptions) = abap_false.
    DATA(lxe_keys) = abap_false.

    LOOP AT texts REFERENCE INTO DATA(text).
      parse_text_id( EXPORTING text_id = text->text_id IMPORTING parsed = DATA(parsed_text_id) ).
      LOOP AT text->translations REFERENCE INTO DATA(translation) WHERE sap_lang = sap_lang.
        CASE parsed_text_id-tab.
          WHEN c_table-dd02t.
            APPEND VALUE #( BASE CORRESPONDING dd02t( parsed_text_id ) ddlanguage = sap_lang ddtext  = translation->content ) TO dd02t_table.
            lxe_descriptions = abap_true.
          WHEN c_table-dd03t.
            APPEND VALUE #( BASE CORRESPONDING dd03t( parsed_text_id ) ddlanguage = sap_lang ddtext  = translation->content ) TO dd03t_table.
            lxe_descriptions = abap_true.
          WHEN c_table-dd08t.
            APPEND VALUE #( BASE CORRESPONDING dd08t( parsed_text_id ) ddlanguage = sap_lang ddtext  = translation->content ) TO dd08t_table.
            lxe_keys = abap_true.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    MODIFY dd02t FROM TABLE @dd02t_table.
    MODIFY dd03t FROM TABLE @dd03t_table.
    MODIFY dd08t FROM TABLE @dd08t_table.

    update_lxe_log( sap_lang = sap_lang descriptions = lxe_descriptions keys = lxe_keys ).
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

  METHOD update_lxe_log.
    DATA lxe_log_table TYPE STANDARD TABLE OF lxe_log WITH EMPTY KEY.

    SELECT SINGLE custmnr FROM lxe_custmnr INTO @DATA(custmnr).
    GET TIME.

    IF descriptions = abap_true.
      APPEND VALUE #( custmnr = custmnr objname = zif_translatable~object_name objtype = c_lxe_type-descriptions targlng = sap_lang
          uname = sy-uname udate = sy-datum utime = sy-uzeit ) TO lxe_log_table.
    ENDIF.
    IF keys = abap_true.
      APPEND VALUE #( custmnr = custmnr objname = zif_translatable~object_name objtype = c_lxe_type-keys targlng = sap_lang
          uname = sy-uname udate = sy-datum utime = sy-uzeit ) TO lxe_log_table.
    ENDIF.


    MODIFY lxe_log FROM TABLE @lxe_log_table.
  ENDMETHOD.

ENDCLASS.
