CLASS ltcl_translatable_class DEFINITION DEFERRED.
CLASS zcl_translatable_class DEFINITION LOCAL FRIENDS ltcl_translatable_class.
CLASS ltcl_translatable_class DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      check_program_name FOR TESTING,
      check_read_texts FOR TESTING.

    DATA:
        cut TYPE REF TO zcl_translatable_class.
ENDCLASS.


CLASS ltcl_translatable_class IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( class_name = 'ZCL_TRANSLATIONS_TEST_CLASS' ).
  ENDMETHOD.

  METHOD check_program_name.
    cl_abap_unit_assert=>assert_equals( act = cut->class_program->object_name exp = 'ZCL_TRANSLATIONS_TEST_CLASS===CP' ).
  ENDMETHOD.

  METHOD check_read_texts.
    cut->zif_translatable~read_language( 'E' ).
    DATA(texts) = cut->zif_translatable~get_all_texts( ).

    cl_abap_unit_assert=>assert_equals( act = texts[ 1 ]-translations[ sap_lang = 'E' ]-content exp = 'Sample text 1' ).
  ENDMETHOD.

ENDCLASS.
