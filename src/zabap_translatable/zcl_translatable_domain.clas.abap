CLASS zcl_translatable_domain DEFINITION
 PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_translatable.
    METHODS:
      constructor IMPORTING domain TYPE sobj_name.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      t_table TYPE c LENGTH 5,
      BEGIN OF  t_text_id_parsed,
        tab      TYPE t_table,
        as4local TYPE as4local,
        as4vers  TYPE as4vers,
      END OF  t_text_id_parsed.

    METHODS:
      get_text_id IMPORTING parsed TYPE  t_text_id_parsed RETURNING VALUE(text_id) TYPE string,
      parse_text_id IMPORTING text_id TYPE string EXPORTING parsed TYPE  t_text_id_parsed,
      get_text IMPORTING text_id TYPE string RETURNING VALUE(text) TYPE REF TO zif_translatable=>t_text,
      modify_translation IMPORTING sap_lang TYPE syst_langu content TYPE textpooltx
                         CHANGING translations TYPE zif_translatable=>tt_translation,
      update_lxe_log IMPORTING sap_lang TYPE sy-langu descriptions TYPE abap_bool constants TYPE abap_bool.

    CONSTANTS:
      BEGIN OF c_lxe_type,
        descriptions TYPE lxeobjtype VALUE 'DOMA',
        constants    TYPE lxeobjtype VALUE 'VALU',
      END OF c_lxe_type,
      BEGIN OF c_table,
        dd01t TYPE t_table VALUE 'DD01T',
        dd07t TYPE t_table VALUE 'DD07T',
      END OF c_table.

    DATA:
      texts    TYPE zif_translatable=>tt_text.
ENDCLASS.



CLASS zcl_translatable_domain IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = domain.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-program.
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    DATA(valpos_empty) = VALUE valpos( ).

    SELECT @c_table-dd01t AS tab, @valpos_empty AS valpos, as4local, as4vers, ddtext
    FROM dd01t WHERE domname = @zif_translatable~object_name AND ddlanguage = @sap_lang
    UNION
    SELECT @c_table-dd07t AS tab, valpos, as4local, as4vers, ddtext
    FROM dd07t WHERE domname = @zif_translatable~object_name AND ddlanguage = @sap_lang
    INTO TABLE @DATA(table_texts).

    LOOP AT table_texts REFERENCE INTO DATA(text).
      DATA(program_text) = get_text( get_text_id( VALUE #( tab = text->tab as4local = text->as4local as4vers = text->as4vers ) ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( text->ddtext ) CHANGING translations = program_text->translations ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    texts = me->texts.
  ENDMETHOD.

  METHOD get_text_id.
    text_id = |{ parsed-tab }\|{ parsed-as4local }\|{ parsed-as4vers }|.
  ENDMETHOD.

  METHOD parse_text_id.
    SPLIT text_id AT '|' INTO parsed-tab parsed-as4local parsed-as4vers.
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
    DATA dd01t_table TYPE STANDARD TABLE OF dd01t WITH EMPTY KEY.
    DATA dd07t_table TYPE STANDARD TABLE OF dd07t WITH EMPTY KEY.

    DATA(lxe_descriptions) = abap_false.
    DATA(lxe_constants) = abap_false.

    LOOP AT texts REFERENCE INTO DATA(text).
      parse_text_id( EXPORTING text_id = text->text_id IMPORTING parsed = DATA(parsed_text_id) ).
      LOOP AT text->translations REFERENCE INTO DATA(translation) WHERE sap_lang = sap_lang.
        CASE parsed_text_id-tab.
          WHEN c_table-dd01t.
            APPEND VALUE #( BASE CORRESPONDING dd02t( parsed_text_id ) ddlanguage = sap_lang ddtext  = translation->content ) TO dd01t_table.
            lxe_descriptions = abap_true.
          WHEN c_table-dd07t.
            APPEND VALUE #( BASE CORRESPONDING dd03t( parsed_text_id ) ddlanguage = sap_lang ddtext  = translation->content ) TO dd07t_table.
            lxe_constants = abap_true.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    MODIFY dd01t FROM TABLE @dd01t_table.
    MODIFY dd07t FROM TABLE @dd07t_table.

    update_lxe_log( sap_lang = sap_lang descriptions = lxe_descriptions constants = lxe_constants ).
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
    IF constants = abap_true.
      APPEND VALUE #( custmnr = custmnr objname = zif_translatable~object_name objtype = c_lxe_type-constants targlng = sap_lang
          uname = sy-uname udate = sy-datum utime = sy-uzeit ) TO lxe_log_table.
    ENDIF.


    MODIFY lxe_log FROM TABLE @lxe_log_table.
  ENDMETHOD.

ENDCLASS.
