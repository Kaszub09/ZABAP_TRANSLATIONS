CLASS zcl_translatable_function_gr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_translatable.
    METHODS:
      constructor IMPORTING function_group TYPE sobj_name.
  PROTECTED SECTION.
  PRIVATE SECTION.
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
    texts = fg_program->get_all_texts( ).
  ENDMETHOD.

  METHOD zif_translatable~modify_texts.
    fg_program->modify_texts( new_texts ).
  ENDMETHOD.

  METHOD zif_translatable~save_modified_texts.
    fg_program->save_modified_texts( sap_lang ).
  ENDMETHOD.

ENDCLASS.
