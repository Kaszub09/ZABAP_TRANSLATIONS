*"* use this source file for your ABAP unit test classes
CLASS ltcl_translations_lxe_log DEFINITION DEFERRED.
CLASS zcl_translations_lxe_log DEFINITION LOCAL FRIENDS ltcl_translations_lxe_log.
CLASS ltcl_translations_lxe_log DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-METHODS:
      class_teardown.

    METHODS:
      setup,
      update_correct_fields FOR TESTING,
      dont_overwrite_lang_if_lx FOR TESTING.

    DATA:
        cut TYPE REF TO zcl_translations_lxe_log.
ENDCLASS.


CLASS ltcl_translations_lxe_log IMPLEMENTATION.

  METHOD class_teardown.
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD update_correct_fields.
    DATA(lxe_log) = VALUE lxe_log( targlng = 'E'  objtype = 'OBJT' objname = 'OBJNAME' ).
    cut->update_lxe_log( VALUE #( ( lxe_log ) ) ).

    "Verify
    DATA(lxe_lang) = cut->lxe_languages[ sap = 'E' ]-lxe_lang.
    SELECT SINGLE * FROM lxe_log WHERE targlng = @lxe_lang AND objtype = 'OBJT' AND objname = 'OBJNAME' INTO @DATA(lxe_log_db).
    SELECT SINGLE custmnr FROM lxe_custmnr INTO @DATA(custmnr).

    cl_abap_unit_assert=>assert_equals( act = lxe_log_db-custmnr exp = custmnr ).
    cl_abap_unit_assert=>assert_equals( act = lxe_log_db-udate exp = sy-datum ).
    cl_abap_unit_assert=>assert_equals( act = lxe_log_db-uname exp = sy-uname ).
    cl_abap_unit_assert=>assert_equals( act = lxe_log_db-targlng exp = lxe_lang ).
    cl_abap_unit_assert=>assert_equals( act = ( lxe_log_db-utime - sy-uzeit ) exp = 0 tol = 5 ).
  ENDMETHOD.

  METHOD dont_overwrite_lang_if_lx.
    "(SAP-LX)
    "(D-deDE) shouldn't be overwritten to (d-shYU) - possible error if only first letter is checked against SAP-LX dictionary.
    DATA(lxe_lang) = cut->lxe_languages[ sap = 'D' ]-lxe_lang.
    DATA(lxe_log) = VALUE lxe_log( targlng = lxe_lang  objtype = 'OBJT' objname = 'OBJNAME' ).
    cut->update_lxe_log( VALUE #( ( lxe_log ) ) ).

    "Verify
    SELECT SINGLE * FROM lxe_log WHERE objtype = 'OBJT' AND objname = 'OBJNAME' INTO @DATA(lxe_log_db).

    cl_abap_unit_assert=>assert_equals( act = lxe_log_db-targlng exp = lxe_lang ).
  ENDMETHOD.

ENDCLASS.
