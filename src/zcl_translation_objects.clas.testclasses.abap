CLASS ltcl_translation_objects DEFINITION DEFERRED.
CLASS zcl_translation_objects DEFINITION LOCAL FRIENDS ltcl_translation_objects.
CLASS ltcl_translation_objects DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      read_all_texts FOR TESTING.

    DATA:
        cut TYPE REF TO zcl_translation_objects.
ENDCLASS.


CLASS ltcl_translation_objects IMPLEMENTATION.
  METHOD setup.
    cut = NEW #( NEW zcl_translation_languages( VALUE #( ( sign = 'I' option = 'EQ' low = 'E' ) ) ) ).
  ENDMETHOD.

  METHOD read_all_texts.
     cut->add_translatable( zcl_translation_factory=>create_translatable(
        object_type = zcl_translation_globals=>c_object_type-class object_name = 'ZCL_TRANSLATIONS_TEST_CLASS' ) ).
     cut->add_translatable( zcl_translation_factory=>create_translatable(
        object_type = zcl_translation_globals=>c_object_type-message_class object_name = 'ZTRAN_TEST_MSG_CLASS' ) ).

  data(texts) = cut->get_all_texts( ).

    cl_abap_unit_assert=>assert_equals( act = texts[ object_name = 'ZCL_TRANSLATIONS_TEST_CLASS' ]-translations[ sap_lang = 'E' ]-content
        exp = 'Sample text 1' ).
      cl_abap_unit_assert=>assert_equals( act = texts[ object_name = 'ZTRAN_TEST_MSG_CLASS' text_id = '001' ]-translations[ sap_lang = 'E' ]-content
        exp = 'Message 1 EN' ).
  ENDMETHOD.

ENDCLASS.
