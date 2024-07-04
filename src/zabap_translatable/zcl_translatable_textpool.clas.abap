CLASS zcl_translatable_textpool DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_translatable.

    METHODS:
      constructor IMPORTING program TYPE sobj_name.

  PRIVATE SECTION.
    METHODS:
      get_text_id IMPORTING id TYPE textpoolid key TYPE textpoolky RETURNING VALUE(text_id) TYPE string,
      parse_text_id IMPORTING text_id TYPE string EXPORTING sub_type TYPE string id TYPE textpoolid key TYPE textpoolky,
      get_text IMPORTING text_id TYPE string RETURNING VALUE(text) TYPE REF TO zif_translatable=>t_text,
      modify_translation IMPORTING sap_lang TYPE syst_langu content TYPE textpooltx
                         CHANGING  translations TYPE zif_translatable=>tt_translation,
      update_translation_log IMPORTING sap_lang TYPE syst_langu.

    CONSTANTS:
      BEGIN OF c_sel_text,
        no_ref_prefix TYPE c LENGTH 8 VALUE '        ',
        ref_whole     TYPE c LENGTH 9 VALUE 'D       .',
      END OF c_sel_text,
      c_lxe_type TYPE lxeobjtype VALUE 'RPT4'.

    DATA:
      texts    TYPE zif_translatable=>tt_text,
      sub_type TYPE string.
ENDCLASS.

CLASS zcl_translatable_textpool IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = program.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-program.
    sub_type = zcl_translation_globals=>c_subcomponent-textpool.
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    DATA textpool TYPE STANDARD TABLE OF textpool WITH EMPTY KEY.

    READ TEXTPOOL zif_translatable~object_name INTO textpool LANGUAGE sap_lang.

    LOOP AT textpool REFERENCE INTO DATA(textpool_text).
      DATA(program_text) = get_text( get_text_id( id = textpool_text->id key = textpool_text->key ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = textpool_text->entry CHANGING translations = program_text->translations ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    texts = me->texts.
  ENDMETHOD.

  METHOD get_text_id.
    text_id = |{ sub_type }\|{ id }\|{ key }|.
  ENDMETHOD.

  METHOD parse_text_id.
    SPLIT text_id AT '|' INTO sub_type id key.
  ENDMETHOD.

  METHOD zif_translatable~modify_texts.
    LOOP AT new_texts REFERENCE INTO DATA(new_text) USING KEY text_id
    WHERE object_type = zif_translatable~object_type AND object_name = zif_translatable~object_name.
      parse_text_id( EXPORTING text_id = new_text->text_id IMPORTING sub_type = DATA(text_sub_type) id = DATA(id) ).
      IF text_sub_type <> sub_type.
        CONTINUE.
      ENDIF.

      DATA(program_text) = get_text( new_text->text_id ).
      LOOP AT new_text->translations REFERENCE INTO DATA(new_translation).
        DATA(content) = COND textpooltx( WHEN id = 'S' AND new_translation->content(9) <> c_sel_text-ref_whole
            THEN |{ c_sel_text-no_ref_prefix WIDTH = 8 }{ new_translation->content }| ELSE new_translation->content ).
        modify_translation( EXPORTING sap_lang = new_translation->sap_lang content = content
                            CHANGING translations = program_text->translations ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~save_modified_texts.
    DATA textpool TYPE STANDARD TABLE OF textpool WITH EMPTY KEY.

    LOOP AT texts REFERENCE INTO DATA(text).
      parse_text_id( EXPORTING text_id = text->text_id IMPORTING id = DATA(id) key = DATA(key) ).
      LOOP AT text->translations REFERENCE INTO DATA(translation) WHERE sap_lang = sap_lang.
        APPEND VALUE #( id = id key = key entry = translation->content ) TO textpool.
      ENDLOOP.
    ENDLOOP.

    INSERT TEXTPOOL zif_translatable~object_name FROM textpool LANGUAGE sap_lang.

    update_translation_log( sap_lang ).
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

  METHOD update_translation_log.
    SELECT SINGLE custmnr FROM lxe_custmnr INTO @DATA(custmnr).
    GET TIME.

    DATA(lxe_log_entry) = VALUE lxe_log( custmnr = custmnr objtype = c_lxe_type objname = zif_translatable~object_name
        targlng = sap_lang uname = sy-uname udate = sy-datum utime = sy-uzeit ).
    MODIFY lxe_log FROM @lxe_log_entry.
  ENDMETHOD.
ENDCLASS.
