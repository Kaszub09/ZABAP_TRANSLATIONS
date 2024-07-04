CLASS zcl_translatable_menu_el DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES:
      zif_translatable.

    METHODS:
      constructor IMPORTING program TYPE sobj_name.

  PRIVATE SECTION.
    METHODS:
      get_text_id IMPORTING obj_type TYPE mp_o_type obj_code TYPE gui_func sub_code TYPE gui_bcode texttype TYPE mp_txttype
                  RETURNING VALUE(text_id) TYPE string,
      parse_text_id IMPORTING text_id TYPE string
                    EXPORTING sub_type TYPE string obj_type TYPE mp_o_type obj_code TYPE gui_func sub_code TYPE gui_bcode texttype TYPE mp_txttype,
      get_text IMPORTING text_id TYPE string RETURNING VALUE(text) TYPE REF TO zif_translatable=>t_text,
      modify_translation IMPORTING sap_lang TYPE syst_langu content TYPE textpooltx
                         CHANGING  translations TYPE zif_translatable=>tt_translation,
      update_lxe_log IMPORTING sap_lang TYPE sy-langu lxe_type_texts TYPE abap_bool lxe_type_tech TYPE abap_bool.

    CONSTANTS:
      BEGIN OF c_lxe_type,
        texts TYPE lxeobjtype VALUE 'CA4',
        tech  TYPE lxeobjtype VALUE 'CAD4',
      END OF c_lxe_type.

    DATA:
      texts    TYPE zif_translatable=>tt_text,
      sub_type TYPE string.
ENDCLASS.

CLASS zcl_translatable_menu_el IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = program.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-program.
    sub_type = zcl_translation_globals=>c_subcomponent-menu_texts.
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    SELECT obj_type, obj_code, sub_code, texttype, text FROM rsmptexts
    WHERE progname = @zif_translatable~object_name AND sprsl = @sap_lang
    INTO TABLE @DATA(menu_texts).

    LOOP AT menu_texts REFERENCE INTO DATA(menu_text).
      DATA(program_text) = get_text( get_text_id( obj_type = menu_text->obj_type obj_code = menu_text->obj_code
                                                  sub_code = menu_text->sub_code texttype = menu_text->texttype ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( menu_text->text ) CHANGING translations = program_text->translations ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    texts = me->texts.
  ENDMETHOD.

  METHOD get_text_id.
    text_id = |{ sub_type }\|{ obj_type }\|{ obj_code }\|{ sub_code }\|{ texttype }|.
  ENDMETHOD.

  METHOD parse_text_id.
    SPLIT text_id AT '|' INTO sub_type obj_type obj_code sub_code texttype.
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
    DATA rsmptexts_table TYPE STANDARD TABLE OF rsmptexts WITH EMPTY KEY.
    DATA(lxe_type_texts) = abap_false.
    DATA(lxe_type_tech) = abap_false.

    LOOP AT texts REFERENCE INTO DATA(text).
      parse_text_id( EXPORTING text_id = text->text_id IMPORTING obj_type = DATA(obj_type) obj_code = DATA(obj_code)
                                                                 sub_code = DATA(sub_code) texttype = DATA(texttype) ).
      LOOP AT text->translations REFERENCE INTO DATA(translation) WHERE sap_lang = sap_lang.
        APPEND VALUE #( progname = zif_translatable~object_name sprsl = sap_lang obj_type = obj_type obj_code = obj_code
            sub_code = sub_code texttype = texttype text = CONV #( translation->content ) ) TO rsmptexts_table.
      ENDLOOP.

      "Got this logic from ST05 trace when translating menu elements
      lxe_type_tech = xsdbool( ( obj_type <> 'T' AND texttype = 'T' ) OR lxe_type_tech = abap_true ).
      lxe_type_texts = xsdbool( ( obj_type = 'T' OR texttype <> 'T' ) OR lxe_type_texts = abap_true ).
    ENDLOOP.

    MODIFY rsmptexts FROM TABLE @rsmptexts_table.
    update_lxe_log( sap_lang = sap_lang lxe_type_texts = lxe_type_texts lxe_type_tech = lxe_type_tech ).
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

    IF lxe_type_texts = abap_true.
      APPEND VALUE #( custmnr = custmnr objname = zif_translatable~object_name objtype = c_lxe_type-texts targlng = sap_lang
          uname = sy-uname udate = sy-datum utime = sy-uzeit ) TO lxe_log_table.
    ENDIF.
    IF lxe_type_tech = abap_true.
      APPEND VALUE #( custmnr = custmnr objname = zif_translatable~object_name objtype = c_lxe_type-tech targlng = sap_lang
          uname = sy-uname udate = sy-datum utime = sy-uzeit ) TO lxe_log_table.
    ENDIF.

    MODIFY lxe_log FROM TABLE @lxe_log_table.
  ENDMETHOD.
ENDCLASS.
