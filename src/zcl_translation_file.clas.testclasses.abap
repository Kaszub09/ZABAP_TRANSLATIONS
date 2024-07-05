*"* use this source file for your ABAP unit test classes
CLASS ltcl_translation_file DEFINITION DEFERRED.
CLASS zcl_translation_file DEFINITION LOCAL FRIENDS ltcl_translation_file.
CLASS ltcl_translation_file DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_expected_export_tab,
        object_type TYPE trobjtype,
        object_name TYPE sobj_name,
        text_id     TYPE string,
        en          TYPE textpooltx,
        pl          TYPE textpooltx,
      END OF t_expected_export_tab,
      tt_expected_export_tab TYPE STANDARD TABLE OF t_expected_export_tab WITH EMPTY KEY.
    METHODS:
      setup,
      build_export_table FOR TESTING.
    DATA:
      cut TYPE REF TO zcl_translation_file.
ENDCLASS.


CLASS ltcl_translation_file IMPLEMENTATION.


  METHOD setup.
    cut = NEW #( NEW zcl_translation_languages( VALUE #( sign = 'I' option = 'EQ' ( low = 'E' ) ( low = 'L' ) ) ) ).
  ENDMETHOD.

  METHOD build_export_table.
    DATA(texts) = VALUE zif_translatable=>tt_text(
      ( object_type = 'MSAG' object_name = 'ZTRAN_TEST_MSG_CLASS' text_id = '001'
        translations = VALUE #( ( sap_lang = 'E' content = 'TEXT1 E' ) ( sap_lang = 'L' content = 'TEXT1 L' )
                                ( sap_lang = 'c' content = 'TEXT1 C' ) ) )
      ( object_type = 'MSAG' object_name = 'ZTRAN_TEST_MSG_CLASS' text_id = '002'
        translations = VALUE #( ( sap_lang = 'E' content = 'TEXT2 E' ) ( sap_lang = 'c' content = 'TEXT2 C' ) ) ) ).

    DATA(table_ref) = cut->build_export_table( texts ).
    FIELD-SYMBOLS <export_table> TYPE table.
    ASSIGN table_ref->* TO <export_table>.
    SORT <export_table> BY ('TEXT_ID').

    DATA(expected) = VALUE tt_expected_export_tab( object_type = 'MSAG' object_name = 'ZTRAN_TEST_MSG_CLASS'
            ( text_id = '001' en = 'TEXT1 E' pl = 'TEXT1 L' )
            ( text_id = '002' en = 'TEXT2 E' pl = space ) ).
    SORT expected BY text_id.

    cl_abap_unit_assert=>assert_equals( act = <export_table> exp = expected ).
  ENDMETHOD.

ENDCLASS.
