CLASS zcl_translatable_class DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_translation_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_translatable.

    METHODS:
      constructor IMPORTING class_name TYPE sobj_name RAISING zcx_translation.

  PRIVATE SECTION.
    METHODS:
      texts_class_to_program IMPORTING texts TYPE zif_translatable=>tt_text RETURNING VALUE(translated_texts) TYPE zif_translatable=>tt_text,
      texts_program_to_class IMPORTING texts TYPE zif_translatable=>tt_text RETURNING VALUE(translated_texts) TYPE zif_translatable=>tt_text.

    DATA:
      class_program TYPE REF TO zif_translatable.
ENDCLASS.

CLASS zcl_translatable_class IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = class_name.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-class.

    class_program = zcl_translation_factory=>create_translatable(
        object_type = zcl_translation_globals=>c_object_type-program
        object_name = cl_oo_include_naming=>get_instance_by_name( to_upper( class_name ) )->pool ).
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    class_program->read_language( sap_lang ).
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    texts = texts_program_to_class( class_program->get_all_texts( ) ).
  ENDMETHOD.

  METHOD zif_translatable~modify_texts.
    class_program->modify_texts( texts_class_to_program( new_texts ) ).
  ENDMETHOD.

  METHOD zif_translatable~save_modified_texts.
    class_program->save_modified_texts( sap_lang ).
  ENDMETHOD.

  METHOD texts_class_to_program.
    LOOP AT texts REFERENCE INTO DATA(new_text) USING KEY text_id
    WHERE object_type = zif_translatable~object_type AND object_name = zif_translatable~object_name.
      APPEND VALUE #( BASE new_text->* object_type = class_program->object_type object_name = class_program->object_name ) TO translated_texts.
    ENDLOOP.
  ENDMETHOD.

  METHOD texts_program_to_class.
    LOOP AT texts REFERENCE INTO DATA(new_text) USING KEY text_id
    WHERE object_type = class_program->object_type AND object_name = class_program->object_name.
      APPEND VALUE #( BASE new_text->* object_type = zif_translatable~object_type object_name = zif_translatable~object_name ) TO translated_texts.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
