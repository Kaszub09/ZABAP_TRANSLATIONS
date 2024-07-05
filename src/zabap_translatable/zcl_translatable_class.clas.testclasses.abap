CLASS ltcl_translatable_class DEFINITION DEFERRED.
CLASS zcl_translatable_class DEFINITION LOCAL FRIENDS ltcl_translatable_class.
CLASS ltcl_translatable_class DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS:
      setup,
      check_program_name FOR TESTING,
      check_read_texts FOR TESTING,
      modify_texts FOR TESTING.

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

    cl_abap_unit_assert=>assert_equals( act = texts[ object_type = zcl_translation_globals=>c_object_type-class
                                                     object_name = 'ZCL_TRANSLATIONS_TEST_CLASS'
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
