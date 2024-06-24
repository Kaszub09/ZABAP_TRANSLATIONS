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
      class_program TYPE REF TO zif_translatable.
ENDCLASS.

CLASS zcl_translatable_function_gr IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = function_group.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-function_group.

    class_program = zcl_translation_factory=>create_translatable(
        object_type = zcl_translation_globals=>c_object_type-program
        object_name = cl_oo_include_naming=>get_instance_by_name( to_upper( function_group ) )->pool ).
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    class_program->read_language( sap_lang ).
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    texts = class_program->get_all_texts( ).
  ENDMETHOD.

  METHOD zif_translatable~modify_texts.
    class_program->modify_texts( new_texts ).
  ENDMETHOD.

  METHOD zif_translatable~save_modified_texts.
    class_program->save_modified_texts( sap_lang ).
  ENDMETHOD.

ENDCLASS.
