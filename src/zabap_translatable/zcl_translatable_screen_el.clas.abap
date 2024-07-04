CLASS zcl_translatable_screen_el DEFINITION
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
      tt_d020t TYPE STANDARD TABLE OF d020t WITH EMPTY KEY,
      tt_d021t TYPE STANDARD TABLE OF d021t WITH EMPTY KEY.

    METHODS:
      get_text_id IMPORTING dynr TYPE dynpronr lxe_type TYPE lxeobjtype fldn TYPE dynfnam RETURNING VALUE(text_id) TYPE string,
      parse_text_id IMPORTING text_id TYPE string EXPORTING sub_type TYPE string lxe_type TYPE lxeobjtype dynr TYPE dynpronr fldn TYPE dynfnam,
      get_text IMPORTING text_id TYPE string RETURNING VALUE(text) TYPE REF TO zif_translatable=>t_text,
      modify_translation IMPORTING sap_lang TYPE syst_langu content TYPE textpooltx
                         CHANGING  translations TYPE zif_translatable=>tt_translation,
      update_lxe_log IMPORTING d020t_table TYPE tt_d020t d021t_table TYPE tt_d021t.
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
    SELECT dynr, @c_lxe_type-header AS lxe_type, fldn, dtxt FROM d021t
    WHERE prog = @zif_translatable~object_name AND lang = @sap_lang
    UNION
    SELECT dynr, @c_lxe_type-header AS lxe_type, @space AS fldn, dtxt FROM d020t
    WHERE prog = @zif_translatable~object_name AND lang = @sap_lang
    INTO TABLE @DATA(screen_texts).

    LOOP AT screen_texts REFERENCE INTO DATA(screen_text).
      DATA(program_text) = get_text( get_text_id( dynr = screen_text->dynr lxe_type = screen_text->lxe_type fldn = screen_text->fldn ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( screen_text->dtxt ) CHANGING translations = program_text->translations ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    texts = me->texts.
  ENDMETHOD.

  METHOD get_text_id.
    text_id = |{ sub_type }\|{ dynr }\|{ lxe_type }\|{ fldn }|.
  ENDMETHOD.

  METHOD parse_text_id.
    SPLIT text_id AT '|' INTO sub_type dynr lxe_type fldn.
  ENDMETHOD.

  METHOD zif_translatable~modify_texts.
    LOOP AT new_texts REFERENCE INTO DATA(new_text) USING KEY text_id
    WHERE object_type = zif_translatable~object_type AND object_name = zif_translatable~object_name.
      parse_text_id( EXPORTING text_id = new_text->text_id IMPORTING sub_type = DATA(text_sub_type) ).
      IF text_sub_type <> sub_type.
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
    DATA d020t_table TYPE tt_d020t.
    DATA d021t_table TYPE tt_d021t.

    LOOP AT texts REFERENCE INTO DATA(text).
      parse_text_id( EXPORTING text_id = text->text_id IMPORTING dynr = DATA(dynr) lxe_type = DATA(lxe_type) fldn = DATA(fldn) ).
      LOOP AT text->translations REFERENCE INTO DATA(translation) WHERE sap_lang = sap_lang.
        CASE lxe_type.
          WHEN c_lxe_type-header.
            APPEND VALUE #( prog = zif_translatable~object_name dynr = dynr lang = sap_lang
                dtxt = CONV #( translation->content ) ) TO d020t_table.
          WHEN c_lxe_type-text.
            APPEND VALUE #( prog = zif_translatable~object_name dynr = dynr lang = sap_lang fldn = fldn
                dtxt = CONV #( translation->content ) ) TO d021t_table.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    MODIFY d020t FROM TABLE @d020t_table.
    MODIFY d021t FROM TABLE @d021t_table.

    update_lxe_log( d020t_table = d020t_table d021t_table = d021t_table ).
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

    LOOP AT d020t_table REFERENCE INTO DATA(d020t).
      APPEND VALUE #( custmnr = custmnr objname = |{ zif_translatable~object_name WIDTH = 40 }{ d020t->dynr }|
        objtype = c_lxe_type-header targlng = d020t->lang uname = sy-uname udate = sy-datum utime = sy-uzeit ) TO lxe_log_table.
    ENDLOOP.
    LOOP AT d021t_table REFERENCE INTO DATA(d021t).
      DATA(lxe_log_row) = VALUE lxe_log( custmnr = custmnr objname = |{ zif_translatable~object_name WIDTH = 40 }{ d021t->dynr }|
        objtype = c_lxe_type-text targlng = d021t->lang uname = sy-uname udate = sy-datum utime = sy-uzeit ).
    ENDLOOP.

    SORT lxe_log_table  BY custmnr targlng objtype objname tabkey.
    DELETE ADJACENT DUPLICATES FROM lxe_log_table COMPARING custmnr targlng objtype objname tabkey.

    MODIFY lxe_log FROM TABLE @lxe_log_table.
  ENDMETHOD.

ENDCLASS.
