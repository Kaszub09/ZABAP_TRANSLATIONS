CLASS zcl_translatable_data_element DEFINITION
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
      t_text_type TYPE c LENGTH 3,
      BEGIN OF t_text_id_parsed,
        text_type TYPE t_text_type,
        as4local  TYPE as4local,
        as4vers   TYPE as4vers,
      END OF t_text_id_parsed.

    METHODS:
     get_text_id IMPORTING parsed TYPE t_text_id_parsed RETURNING VALUE(text_id) TYPE string,
     parse_text_id IMPORTING text_id TYPE string EXPORTING parsed TYPE t_text_id_parsed,
     get_text IMPORTING text_id TYPE string RETURNING VALUE(text) TYPE REF TO zif_translatable=>t_text,
     modify_translation IMPORTING sap_lang TYPE syst_langu content TYPE textpooltx
          CHANGING translations TYPE zif_translatable=>tt_translation,
     update_lxe_log IMPORTING sap_lang TYPE sy-langu .

    CONSTANTS:
      c_lxe_type TYPE lxeobjtype VALUE 'DTEL',
      BEGIN OF c_text_type,
        description TYPE t_text_type VALUE 'D60',
        short       TYPE t_text_type VALUE 'S10',
        medium      TYPE t_text_type VALUE 'M20',
        long        TYPE t_text_type VALUE 'L40',
        header      TYPE t_text_type VALUE 'H40',
      END OF c_text_type.

    DATA:
     texts TYPE zif_translatable=>tt_text.
ENDCLASS.



CLASS zcl_translatable_data_element IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = program.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-program.
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    DATA(empty_as4vers) = VALUE as4vers( ).
    SELECT as4local, as4vers, ddtext, reptext , scrtext_s , scrtext_m , scrtext_l
    FROM dd04t WHERE rollname = @zif_translatable~object_name AND ddlanguage = @sap_lang
    INTO TABLE @DATA(data_element_texts).

    LOOP AT data_element_texts REFERENCE INTO DATA(text).
      DATA(program_text) = get_text( get_text_id( VALUE #( text_type = c_text_type-description as4local = text->as4local as4vers = text->as4vers ) ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( text->ddtext ) CHANGING translations = program_text->translations ).

      program_text = get_text( get_text_id( VALUE #( text_type = c_text_type-short as4local = text->as4local as4vers = text->as4vers ) ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( text->scrtext_s ) CHANGING translations = program_text->translations ).

      program_text = get_text( get_text_id( VALUE #( text_type = c_text_type-medium as4local = text->as4local as4vers = text->as4vers ) ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( text->scrtext_m ) CHANGING translations = program_text->translations ).

      program_text = get_text( get_text_id( VALUE #( text_type = c_text_type-long as4local = text->as4local as4vers = text->as4vers ) ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( text->scrtext_l ) CHANGING translations = program_text->translations ).

      program_text = get_text( get_text_id( VALUE #( text_type = c_text_type-header as4local = text->as4local as4vers = text->as4vers ) ) ).
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
    DATA dd04t_table TYPE STANDARD TABLE OF dd04t WITH EMPTY KEY.

    LOOP AT texts REFERENCE INTO DATA(text).
      parse_text_id( EXPORTING text_id = text->text_id IMPORTING parsed = DATA(parsed_text_id) ).
      LOOP AT text->translations REFERENCE INTO DATA(translation) WHERE sap_lang = sap_lang.
        DATA(dd04t_row) = REF #( dd04t_table[ rollname = zif_translatable~object_name ddlanguage = sap_lang
            as4local = parsed_text_id-as4local as4vers = parsed_text_id-as4local ] OPTIONAL ).
        IF NOT dd04t_row IS BOUND.
          INSERT VALUE #( rollname = zif_translatable~object_name ddlanguage = sap_lang as4local = parsed_text_id-as4local
              as4vers = parsed_text_id-as4local ) INTO TABLE dd04t_table REFERENCE INTO dd04t_row.
        ENDIF.
        CASE parsed_text_id-text_type.
          WHEN c_text_type-description. dd04t_row->ddtext = translation->content.
          WHEN c_text_type-short. dd04t_row->scrtext_s = translation->content.
          WHEN c_text_type-medium. dd04t_row->scrtext_m = translation->content.
          WHEN c_text_type-long. dd04t_row->scrtext_l = translation->content.
          WHEN c_text_type-header. dd04t_row->reptext = translation->content.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    MODIFY dd04t FROM TABLE @dd04t_table.
    "TODO DD04L lengths

    update_lxe_log( sap_lang = sap_lang ).
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

    APPEND VALUE #( custmnr = custmnr objname = zif_translatable~object_name objtype = c_lxe_type targlng = sap_lang
     uname = sy-uname udate = sy-datum utime = sy-uzeit ) TO lxe_log_table.


    MODIFY lxe_log FROM TABLE @lxe_log_table.
  ENDMETHOD.

ENDCLASS.
