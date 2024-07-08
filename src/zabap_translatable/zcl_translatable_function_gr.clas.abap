CLASS zcl_translatable_function_gr DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_translation_factory.
  PUBLIC SECTION.
    INTERFACES:
      zif_translatable.

    METHODS:
      constructor IMPORTING function_group TYPE sobj_name RAISING zcx_translation.

  PRIVATE SECTION.
    METHODS:
      texts_fg_to_program IMPORTING texts TYPE zif_translatable=>tt_text RETURNING VALUE(translated_texts) TYPE zif_translatable=>tt_text,
      texts_program_to_fg IMPORTING texts TYPE zif_translatable=>tt_text RETURNING VALUE(translated_texts) TYPE zif_translatable=>tt_text.

    DATA:
      fg_program TYPE REF TO zif_translatable.
ENDCLASS.

CLASS zcl_translatable_function_gr IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = function_group.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-function_group.

    fg_program = zcl_translation_factory=>create_translatable(
        object_type = zcl_translation_globals=>c_object_type-program
        object_name = |SAPL{ to_upper( function_group ) }| ).
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    fg_program->read_language( sap_lang ).
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    texts = texts_program_to_fg( fg_program->get_all_texts( ) ).
  ENDMETHOD.

  METHOD zif_translatable~modify_texts.
    fg_program->modify_texts( texts_fg_to_program( new_texts ) ).
  ENDMETHOD.

  METHOD zif_translatable~save_modified_texts.
    fg_program->save_modified_texts( sap_lang ).
  ENDMETHOD.

  METHOD texts_fg_to_program.
    LOOP AT texts REFERENCE INTO DATA(new_text) USING KEY text_id
    WHERE object_type = zif_translatable~object_type AND object_name = zif_translatable~object_name.
      APPEND VALUE #( BASE new_text->* object_type = fg_program->object_type object_name = fg_program->object_name ) TO translated_texts.
    ENDLOOP.
  ENDMETHOD.

  METHOD texts_program_to_fg.
    LOOP AT texts REFERENCE INTO DATA(new_text) USING KEY text_id
    WHERE object_type = fg_program->object_type AND object_name = fg_program->object_name.
      APPEND VALUE #( BASE new_text->* object_type = zif_translatable~object_type object_name = zif_translatable~object_name ) TO translated_texts.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
