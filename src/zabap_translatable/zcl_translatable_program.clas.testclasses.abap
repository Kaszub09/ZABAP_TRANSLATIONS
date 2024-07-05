CLASS ltcl_translatable_program DEFINITION DEFERRED.
CLASS zcl_translatable_program DEFINITION LOCAL FRIENDS ltcl_translatable_program.
CLASS ltcl_translatable_program DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      setup,
      check_read_texts FOR TESTING.

    DATA:
        cut TYPE REF TO zif_translatable.
ENDCLASS.

CLASS ltcl_translatable_program IMPLEMENTATION.
  METHOD setup.
    cut = NEW zcl_translatable_program( 'ZABAP_TRANSLATIONS_TEST_PROG' ).
  ENDMETHOD.

  METHOD check_read_texts.
    cut->read_language( 'E' ).
    DATA(texts) = cut->get_all_texts( ).

    cl_abap_unit_assert=>assert_equals( act = texts[ KEY id_only text_id = 'TEXTPOOL|I|001' ]-translations[ sap_lang = 'E' ]-content
                                        exp = 'Message 1 EN' ).
    cl_abap_unit_assert=>assert_equals( act = texts[ KEY id_only text_id = 'SCREEN_TEXTS|0001|SRT4|LABEL' ]-translations[ sap_lang = 'E' ]-content
                                        exp = 'LABEL' ).
    cl_abap_unit_assert=>assert_equals( act = texts[ KEY id_only text_id = 'MENU_TEXTS|F|FUNC1|001|Q' ]-translations[ sap_lang = 'E' ]-content
                                        exp = 'Info text function 1' ).
  ENDMETHOD.
ENDCLASS.
