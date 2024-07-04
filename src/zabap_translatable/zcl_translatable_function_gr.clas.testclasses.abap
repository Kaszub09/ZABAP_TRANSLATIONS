CLASS ltcl_translatable_function_gr DEFINITION DEFERRED.
CLASS zcl_translatable_function_gr DEFINITION LOCAL FRIENDS ltcl_translatable_function_gr.
CLASS ltcl_translatable_function_gr DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      check_program_name FOR TESTING,
      check_read_texts FOR TESTING.

    DATA:
        cut TYPE REF TO zcl_translatable_function_gr.
ENDCLASS.


CLASS ltcl_translatable_function_gr IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( function_group = 'ZABAP_TRANSLATABLE_FG_TEST' ).
  ENDMETHOD.

  METHOD check_program_name.
    cl_abap_unit_assert=>assert_equals( act = cut->fg_program->object_name exp = 'SAPLZABAP_TRANSLATABLE_FG_TEST' ).
  ENDMETHOD.

  METHOD check_read_texts.
    cut->zif_translatable~read_language( 'E' ).
    DATA(texts) = cut->zif_translatable~get_all_texts( ).

    cl_abap_unit_assert=>assert_equals( act = texts[ 1 ]-translations[ sap_lang = 'E' ]-content exp = 'Sample text 1' ).
  ENDMETHOD.

ENDCLASS.
