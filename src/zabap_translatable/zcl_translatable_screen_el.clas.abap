CLASS zcl_translatable_screen_el DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_translation_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_translatable.
    METHODS:
      constructor IMPORTING program TYPE sobj_name.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      tt_d020t TYPE STANDARD TABLE OF d020t WITH EMPTY KEY,
      tt_d021t TYPE STANDARD TABLE OF d021t WITH EMPTY KEY,
      BEGIN OF t_text_id_parsed,
        sub_type TYPE string,
        dynr     TYPE dynpronr,
        lxe_type TYPE lxeobjtype,
        fldn     TYPE dynfnam,
      END OF t_text_id_parsed.

    METHODS:
      get_text_id IMPORTING parsed TYPE t_text_id_parsed RETURNING VALUE(text_id) TYPE string,
      parse_text_id IMPORTING text_id TYPE string RETURNING VALUE(parsed) TYPE t_text_id_parsed,
      get_text IMPORTING text_id TYPE string RETURNING VALUE(text) TYPE REF TO zif_translatable=>t_text,
      modify_translation IMPORTING sap_lang TYPE syst_langu content TYPE textpooltx
                         CHANGING  translations TYPE zif_translatable=>tt_translation.
    CONSTANTS:
      BEGIN OF c_lxe_type,
        header TYPE lxeobjtype VALUE 'SRH4',
        text   TYPE lxeobjtype VALUE 'SRT4',
      END OF c_lxe_type.

    DATA:
      texts    TYPE zif_translatable=>tt_text,
      sub_type TYPE string.
ENDCLASS.



CLASS zcl_translatable_screen_el IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = program.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-program.
    sub_type = zcl_translation_globals=>c_subcomponent-screen_texts.
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    SELECT dynr, @c_lxe_type-text AS lxe_type, fldn, dtxt FROM d021t
    WHERE prog = @zif_translatable~object_name AND lang = @sap_lang
    UNION
    SELECT dynr, @c_lxe_type-header AS lxe_type, @space AS fldn, dtxt FROM d020t
    WHERE prog = @zif_translatable~object_name AND lang = @sap_lang
    INTO TABLE @DATA(screen_texts).

    LOOP AT screen_texts REFERENCE INTO DATA(screen_text).
      DATA(program_text) = get_text( get_text_id( VALUE #( sub_type = sub_type dynr = screen_text->dynr
                                                           lxe_type = screen_text->lxe_type fldn = screen_text->fldn ) ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( screen_text->dtxt ) CHANGING translations = program_text->translations ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    texts = me->texts.
  ENDMETHOD.

  METHOD get_text_id.
    text_id = |{ parsed-sub_type }\|{ parsed-dynr }\|{ parsed-lxe_type }\|{ parsed-fldn }|.
  ENDMETHOD.

  METHOD parse_text_id.
    SPLIT text_id AT '|' INTO parsed-sub_type parsed-dynr parsed-lxe_type parsed-fldn.
  ENDMETHOD.

  METHOD zif_translatable~modify_texts.
    LOOP AT new_texts REFERENCE INTO DATA(new_text) USING KEY text_id
    WHERE object_type = zif_translatable~object_type AND object_name = zif_translatable~object_name.
      DATA(parsed) = parse_text_id( EXPORTING text_id = new_text->text_id ).
      IF parsed-sub_type <> sub_type.
        CONTINUE.
      ENDIF.

      DATA(program_text) = get_text( new_text->text_id ).
      LOOP AT new_text->translations REFERENCE INTO DATA(new_translation).
        modify_translation( EXPORTING sap_lang = new_translation->sap_lang content = new_translation->content
                            CHANGING translations = program_text->translations ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~save_modified_texts.
    DATA(lxe_log) = zcl_translation_factory=>get_lxe_log( ).
    DATA lxe_log_table TYPE lxe_log->tt_lxe_log.
    DATA d020t_table TYPE tt_d020t.
    DATA d021t_table TYPE tt_d021t.

    LOOP AT texts REFERENCE INTO DATA(text).
      DATA(parsed) = parse_text_id( text->text_id ).
      LOOP AT text->translations REFERENCE INTO DATA(translation) WHERE sap_lang = sap_lang.
        CASE parsed-lxe_type.
          WHEN c_lxe_type-header.
            APPEND VALUE #( prog = zif_translatable~object_name dynr = parsed-dynr lang = sap_lang
                            dtxt = CONV #( translation->content ) ) TO d020t_table.
          WHEN c_lxe_type-text.
            APPEND VALUE #( prog = zif_translatable~object_name dynr = parsed-dynr lang = sap_lang fldn = parsed-fldn
                            dtxt = CONV #( translation->content ) ) TO d021t_table.
        ENDCASE.

        APPEND VALUE #( objname = |{ zif_translatable~object_name WIDTH = 40 }{ parsed-dynr }|
                        objtype = parsed-lxe_type targlng = sap_lang ) TO lxe_log_table.
      ENDLOOP.
    ENDLOOP.

    MODIFY d020t FROM TABLE @d020t_table.
    MODIFY d021t FROM TABLE @d021t_table.

    lxe_log->update_lxe_log( lxe_log_table ).
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
