CLASS ltcl_translation_factory DEFINITION DEFERRED.
CLASS zcl_translation_factory DEFINITION LOCAL FRIENDS ltcl_translation_factory.
CLASS ltcl_translation_factory DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      setup,
      can_create FOR TESTING,
      error_on_create FOR TESTING,
      verify_can_create_translatable IMPORTING object_type TYPE trobjtype object_name TYPE sobj_name,
      verify_can_create_transla_sub IMPORTING sub_type TYPE string object_name TYPE sobj_name.

    DATA:
        cut TYPE REF TO zcl_im_mm_fill_del_inflog.
ENDCLASS.


CLASS ltcl_translation_factory IMPLEMENTATION.


  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.


  METHOD can_create.
    verify_can_create_translatable( object_type = zcl_translation_globals=>c_object_type-class object_name = 'ZCL_TRANSLATIONS_TEST_CLASS' ).
    verify_can_create_translatable( object_type = zcl_translation_globals=>c_object_type-data_element object_name = 'ZABAP_TRANSLATION_TEST_DE' ).
    verify_can_create_translatable( object_type = zcl_translation_globals=>c_object_type-domain object_name = 'ZABAP_TRANSLATIONS_TEST_DOMAIN' ).
    verify_can_create_translatable( object_type = zcl_translation_globals=>c_object_type-function_group object_name = 'ZABAP_TRANSLATABLE_FG_TEST' ).
    verify_can_create_translatable( object_type = zcl_translation_globals=>c_object_type-message_class object_name = 'ZTRAN_TEST_MSG_CLASS' ).
    verify_can_create_translatable( object_type = zcl_translation_globals=>c_object_type-program object_name = 'ZABAP_TRANSLATIONS_TEST_PROG' ).
    verify_can_create_translatable( object_type = zcl_translation_globals=>c_object_type-table object_name = 'ZABAP_TRA_TEST_T' ).
    verify_can_create_translatable( object_type = zcl_translation_globals=>c_object_type-transaction object_name = 'ZABAP_TRANS_L_A_TEST' ).

    verify_can_create_transla_sub( sub_type = zcl_translation_globals=>c_subcomponent-menu_texts object_name = 'ZABAP_TRANSLATIONS_TEST_PROG' ).
    verify_can_create_transla_sub( sub_type = zcl_translation_globals=>c_subcomponent-screen_texts object_name = 'ZABAP_TRANSLATIONS_TEST_PROG' ).
    verify_can_create_transla_sub( sub_type = zcl_translation_globals=>c_subcomponent-textpool object_name = 'ZABAP_TRANSLATIONS_TEST_PROG' ).
  ENDMETHOD.

  METHOD error_on_create.
    TRY.
        zcl_translation_factory=>create_translatable( object_type = space object_name = space ).
        cl_abap_unit_assert=>fail( |Exception for object_type not raised| ).
      CATCH zcx_translation.
    ENDTRY.

    TRY.
        zcl_translation_factory=>create_translatable_sub( sub_type = space object_name = space ).
        cl_abap_unit_assert=>fail( |Exception for sub_type not raised| ).
      CATCH zcx_translation.
    ENDTRY.
  ENDMETHOD.

  METHOD verify_can_create_translatable.
    DATA(translatable) = zcl_translation_factory=>create_translatable( object_type = object_type object_name = object_name ).
    cl_abap_unit_assert=>assert_bound( act = translatable msg = |{ object_type }-{ object_name }| ).
    cl_abap_unit_assert=>assert_equals( act = translatable->object_name exp = object_name ).
    cl_abap_unit_assert=>assert_equals( act = translatable->object_type exp = object_type ).
  ENDMETHOD.

  METHOD verify_can_create_transla_sub.
    DATA(translatable) = zcl_translation_factory=>create_translatable_sub( sub_type = sub_type object_name = object_name ).
    cl_abap_unit_assert=>assert_bound( act = translatable msg = |{ sub_type }-{ object_name }| ).
    cl_abap_unit_assert=>assert_equals( act = translatable->object_name exp = object_name ).
    cl_abap_unit_assert=>assert_equals( act = translatable->object_type exp = zcl_translation_globals=>c_object_type-program ).
  ENDMETHOD.

ENDCLASS.
