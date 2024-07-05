CLASS ltcl_translatable_function_gr DEFINITION DEFERRED.
CLASS zcl_translatable_function_gr DEFINITION LOCAL FRIENDS ltcl_translatable_function_gr.
CLASS ltcl_translatable_function_gr DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      check_program_name FOR TESTING,
      check_read_texts FOR TESTING,
      modify_texts FOR TESTING.

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

    cl_abap_unit_assert=>assert_equals( act = texts[ object_type = zcl_translation_globals=>c_object_type-function_group
                                                     object_name = 'ZABAP_TRANSLATABLE_FG_TEST'
                                                    ]-translations[ sap_lang = 'E' ]-content exp = 'Sample text 1' ).
  ENDMETHOD.

  METHOD modify_texts.
    cut->zif_translatable~read_language( 'E' ).
    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->zif_translatable~object_name
                                                       object_type = cut->zif_translatable~object_type
        ( text_id = 'TEXTPOOL|I|001' translations = VALUE #( ( sap_lang = 'E' content = 'Modified' ) ) ) ).

    cut->zif_translatable~modify_texts( new_texts ).
    "--------------------------------------------------
    DATA(current_texts) = cut->zif_translatable~get_all_texts( ).

    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 1 ] table = current_texts msg = |Text 001 not modified| ).
  ENDMETHOD.
ENDCLASS.
