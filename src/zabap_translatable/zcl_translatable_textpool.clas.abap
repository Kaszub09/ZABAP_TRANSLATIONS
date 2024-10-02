CLASS zcl_translatable_textpool DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_translation_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_translatable.

    METHODS:
      constructor IMPORTING program TYPE sobj_name.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF  t_text_id_parsed,
        sub_type TYPE string,
        id       TYPE textpoolid,
        key      TYPE textpoolky,
      END OF  t_text_id_parsed,
      tt_sap_lang TYPE SORTED TABLE OF spras WITH UNIQUE KEY table_line.

    METHODS:
      get_text_id IMPORTING parsed TYPE  t_text_id_parsed RETURNING VALUE(text_id) TYPE string,
      parse_text_id IMPORTING text_id TYPE string RETURNING VALUE(parsed) TYPE t_text_id_parsed,
      get_text IMPORTING text_id TYPE string RETURNING VALUE(text) TYPE REF TO zif_translatable=>t_text,
      modify_translation IMPORTING sap_lang TYPE syst_langu content TYPE textpooltx
                         CHANGING  translations TYPE zif_translatable=>tt_translation.

    CONSTANTS:
      BEGIN OF c_sel_text,
        no_ref_prefix TYPE c LENGTH 8 VALUE '        ',
        ref_whole     TYPE c LENGTH 9 VALUE 'D       .',
      END OF c_sel_text,
      c_lxe_type TYPE lxeobjtype VALUE 'RPT4'.

    DATA:
      texts          TYPE zif_translatable=>tt_text,
      sub_type       TYPE string,
      read_sap_langs TYPE tt_sap_lang.
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
      DATA(program_text) = get_text( get_text_id( VALUE #( sub_type = sub_type id = textpool_text->id key = textpool_text->key ) ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = textpool_text->entry CHANGING translations = program_text->translations ).
    ENDLOOP.

    IF NOT line_exists( read_sap_langs[ table_line = sap_lang ] ).
      INSERT sap_lang INTO TABLE read_sap_langs.
    ENDIF.
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    texts = me->texts.
  ENDMETHOD.

  METHOD get_text_id.
    text_id = |{ parsed-sub_type }\|{ parsed-id }\|{ parsed-key }|.
  ENDMETHOD.

  METHOD parse_text_id.
    SPLIT text_id AT '|' INTO parsed-sub_type parsed-id parsed-key.
  ENDMETHOD.

  METHOD zif_translatable~modify_texts.
    LOOP AT new_texts REFERENCE INTO DATA(new_text) USING KEY text_id
    WHERE object_type = zif_translatable~object_type AND object_name = zif_translatable~object_name.
      DATA(parsed) = parse_text_id( new_text->text_id ).
      IF parsed-sub_type <> sub_type.
        CONTINUE.
      ENDIF.

      DATA(program_text) = get_text( new_text->text_id ).
      LOOP AT new_text->translations REFERENCE INTO DATA(new_translation).
        "If you try to save translation and miss some text, INSERT TEXTPOOL will delete translations for this language.
        "To avoid this we first read all texts from language if it wasn't already read
        IF NOT line_exists( read_sap_langs[ table_line = new_translation->sap_lang ] ).
          zif_translatable~read_language( new_translation->sap_lang ).
        ENDIF.

        DATA(content) = COND textpooltx(
        "Non-edited excel still has empty prefixes, even though they are not visible after opening
        WHEN parsed-id = 'S' AND new_translation->content(9) <> c_sel_text-ref_whole AND new_translation->content(8) <> c_sel_text-no_ref_prefix
            THEN |{ c_sel_text-no_ref_prefix WIDTH = 8 }{ new_translation->content }|
            ELSE new_translation->content ).

        modify_translation( EXPORTING sap_lang = new_translation->sap_lang content = content
                            CHANGING translations = program_text->translations ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~save_modified_texts.
    DATA textpool TYPE STANDARD TABLE OF textpool WITH EMPTY KEY.

    LOOP AT texts REFERENCE INTO DATA(text).
      DATA(parsed) = parse_text_id( text->text_id ).
      LOOP AT text->translations REFERENCE INTO DATA(translation) WHERE sap_lang = sap_lang.
        APPEND VALUE #( id = parsed-id key = parsed-key entry = translation->content ) TO textpool.
      ENDLOOP.
    ENDLOOP.

    INSERT TEXTPOOL zif_translatable~object_name FROM textpool LANGUAGE sap_lang.

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
ENDCLASS.
