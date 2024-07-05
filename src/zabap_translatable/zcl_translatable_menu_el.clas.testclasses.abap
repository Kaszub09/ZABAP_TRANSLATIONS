CONSTANTS:
  c_test_prog              TYPE sobj_name VALUE 'ZABAP_TRANSLATIONS_TEST_PROG',
  c_lang_pl                TYPE sy-langu VALUE 'L',
  c_lang_en                TYPE sy-langu VALUE 'E',
  c_sel_text_ref           TYPE c LENGTH 9 VALUE 'D .',
  c_sel_text_no_ref_prefix TYPE c LENGTH 9 VALUE '  '.

CLASS ltcl_translatable_menu_el DEFINITION DEFERRED.
CLASS zcl_translatable_menu_el DEFINITION LOCAL FRIENDS ltcl_translatable_menu_el.
CLASS ltcl_translatable_menu_el DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES:
    tt_menu_el TYPE STANDARD TABLE OF textpool WITH EMPTY KEY.
    CLASS-METHODS:
      class_teardown.
    METHODS:
      setup,
      read_text_in_one_language FOR TESTING,
      read_text_in_multiple_lang FOR TESTING,
      mock_texts_read_in_polish,
      modify_texts FOR TESTING,
      save_modified_in_db IMPORTING en TYPE abap_bool DEFAULT abap_true pl TYPE abap_bool DEFAULT abap_true,
      verify_lxe_log_after_save FOR TESTING,
      verify_menu_els_after_save FOR TESTING.

    DATA:
    cut TYPE REF TO zif_translatable.
ENDCLASS.


CLASS ltcl_translatable_menu_el IMPLEMENTATION.
  METHOD class_teardown.
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD setup.
    ROLLBACK WORK.
    cut = NEW zcl_translatable_menu_el( c_test_prog ).
  ENDMETHOD.

  METHOD read_text_in_one_language.
    cut->read_language( c_lang_en ).
    DATA(read_texts) = cut->get_all_texts( ).

    "Texts
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'MENU_TEXTS|A|000001||T' ]-translations[ sap_lang = c_lang_en ]-content
    exp = 'Main status text' ).
    "Func
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'MENU_TEXTS|F|BACK|001|M' ]-translations[ sap_lang = c_lang_en ]-content
    exp = 'Back' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'MENU_TEXTS|F|FUNC1|001|I' ]-translations[ sap_lang = c_lang_en ]-content
    exp = 'Icon text function 1' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'MENU_TEXTS|F|FUNC1|001|M' ]-translations[ sap_lang = c_lang_en ]-content
    exp = 'Function text 1' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'MENU_TEXTS|F|FUNC1|001|Q' ]-translations[ sap_lang = c_lang_en ]-content
    exp = 'Info text function 1' ).
    "Menu
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'MENU_TEXTS|F|SUBMENU1|001|M' ]-translations[ sap_lang = c_lang_en ]-content
    exp = 'Submenu 1 text' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'MENU_TEXTS|M|000001||M' ]-translations[ sap_lang = c_lang_en ]-content
    exp = 'Menu 1' ).
  ENDMETHOD.

  METHOD read_text_in_multiple_lang.
    mock_texts_read_in_polish( ).
    cut->read_language( c_lang_en ).

    DATA(read_texts) = cut->get_all_texts( ).

    LOOP AT read_texts REFERENCE INTO DATA(read_text).
      cl_abap_unit_assert=>assert_equals( act = lines( read_text->translations )
      exp = 2 msg = |Expected translations in 2 langauges - ID { read_text->text_id }| ).
      cl_abap_unit_assert=>assert_differs( act = read_text->translations[ sap_lang = c_lang_en ]
      exp = read_text->translations[ sap_lang = c_lang_pl ] msg = |Expected different message in another lang { read_text->text_id }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD mock_texts_read_in_polish.
    cut->read_language( c_lang_en ).
    DATA(cut_casted) = CAST zcl_translatable_menu_el( cut ).

    LOOP AT cut_casted->texts REFERENCE INTO DATA(text).
      CLEAR text->translations.
      APPEND VALUE #( sap_lang = c_lang_pl content = |{ c_lang_pl }{ text->text_id }| ) TO text->translations.
    ENDLOOP.
  ENDMETHOD.

  METHOD modify_texts.
    cut->read_language( c_lang_en ).

    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->object_name
          object_type = cut->object_type
    ( text_id = 'MENU_TEXTS|A|000001||T' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified' ) ) )
    ( text_id = 'MENU_TEXTS|F|FUNC1|001|I' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified func icon' ) ) )
    ( text_id = 'MENU_TEXTS|F|SUBMENU1|001|M' translations = VALUE #( ( sap_lang = c_lang_pl content = 'New submenu in another lang' ) ) ) ).

    cut->modify_texts( new_texts ).
    "--------------------------------------------------
    DATA(current_texts) = cut->get_all_texts( ).

    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 1 ] table = current_texts msg = |Text 000001 not modified| ).
    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 2 ] table = current_texts msg = |Text FUNC1 not modified| ).
    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 3 ]-translations[ sap_lang = c_lang_pl ]
    table = current_texts[ KEY id_only text_id = 'MENU_TEXTS|F|SUBMENU1|001|M' ]-translations
    msg = |Another language version of text SUBMENU1 not added| ).
    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 3 ]-translations[ sap_lang = c_lang_pl ]
    table = current_texts[ KEY id_only text_id = 'MENU_TEXTS|F|SUBMENU1|001|M' ]-translations
    msg = |Text SUBMENU1 in EN shouldn't be modified| ).
  ENDMETHOD.


  METHOD save_modified_in_db.
    cut->read_language( c_lang_en ).
    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->object_name
                                                       object_type = cut->object_type
    ( text_id = 'MENU_TEXTS|A|000001||T' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified' ) ) )
    ( text_id = 'MENU_TEXTS|F|FUNC1|001|I' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified func icon' )
                                                                   ( sap_lang = c_lang_pl content = 'Modified func icon PL' ) ) ) ).
    cut->modify_texts( new_texts ).

    IF en = abap_true.
      cut->save_modified_texts( c_lang_en ).
    ENDIF.
    IF pl = abap_true.
      cut->save_modified_texts( c_lang_pl ).
    ENDIF.
  ENDMETHOD.

  METHOD verify_lxe_log_after_save.

    DATA(lxe_log) = zcl_translation_factory=>get_lxe_log( ).
    DATA(lxe_lang_en) = lxe_log->lxe_languages[ sap = c_lang_en ]-lxe_lang.
    DATA(lxe_lang_pl) = lxe_log->lxe_languages[ sap = c_lang_pl ]-lxe_lang.

    "One lang
    save_modified_in_db( pl = abap_false ).
    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'CA4' AND objname = @c_test_prog
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy3).
    cl_abap_unit_assert=>assert_subrc( exp = 4 msg = |Change log in PL shouldn't be found| ).
    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'CAD4' AND objname = @c_test_prog
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy4).
    cl_abap_unit_assert=>assert_subrc( exp = 4 msg = |Change log in PL shouldn't be found| ).

    "Both lang
    save_modified_in_db( ).
    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_en AND objtype = 'CA4' AND objname = @c_test_prog
    AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy1).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log in EN not found| ).

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_en AND objtype = 'CAD4' AND objname = @c_test_prog
    AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy2).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log in EN not found| ).

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'CA4' AND objname = @c_test_prog
    AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy5).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log in PL not found| ).

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'CAD4' AND objname = @c_test_prog
    AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy6).
    cl_abap_unit_assert=>assert_subrc( exp = 4 msg = |Change log in PL shouldn't be found| ).
  ENDMETHOD.


  METHOD verify_menu_els_after_save.
    SELECT * FROM rsmptexts WHERE progname = @c_test_prog INTO TABLE @DATA(rsmptexts).
    DATA(expected) = rsmptexts.

    expected[ sprsl = c_lang_en obj_type = 'A' obj_code = '000001' texttype = 'T' ]-text = 'Modified'.
    expected[ sprsl = c_lang_en obj_type = 'F' obj_code = 'FUNC1' sub_code = '001' texttype = 'I' ]-text = 'Modified func icon'.
    APPEND VALUE #( progname = c_test_prog sprsl = c_lang_pl obj_type = 'F' obj_code = 'FUNC1' sub_code = '001' texttype = 'I'
                    text = 'Modified func icon PL' ) TO expected.

    "Verification - another language not saved
    save_modified_in_db( pl = abap_false ).
    SELECT * FROM rsmptexts WHERE progname = @c_test_prog AND sprsl = @c_lang_pl INTO TABLE @rsmptexts.
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( rsmptexts ) msg = |Another lang shouldn't be saved| ).

    "Verify
    save_modified_in_db( ).

    SELECT * FROM rsmptexts WHERE progname = @c_test_prog INTO TABLE @rsmptexts.
    SORT: expected BY sprsl obj_type obj_code sub_code texttype, rsmptexts BY sprsl obj_type obj_code sub_code texttype.
    cl_abap_unit_assert=>assert_equals( exp = expected act = rsmptexts ).
  ENDMETHOD.

ENDCLASS.
