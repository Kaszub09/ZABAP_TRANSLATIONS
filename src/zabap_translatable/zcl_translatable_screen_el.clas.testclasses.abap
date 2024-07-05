CONSTANTS:
  c_test_prog TYPE sobj_name VALUE 'ZABAP_TRANSLATIONS_TEST_PROG',
  c_lang_pl   TYPE sy-langu VALUE 'L',
  c_lang_en   TYPE sy-langu VALUE 'E'.

CLASS ltcl_translatable_screen_el DEFINITION DEFERRED.
CLASS zcl_translatable_screen_el DEFINITION LOCAL FRIENDS ltcl_translatable_screen_el.
CLASS ltcl_translatable_screen_el DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES:
    tt_screen_el TYPE STANDARD TABLE OF textpool WITH EMPTY KEY.
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
      verify_d020t_after_save FOR TESTING,
      verify_d021t_after_save FOR TESTING.

    DATA:
    cut TYPE REF TO zif_translatable.
ENDCLASS.


CLASS ltcl_translatable_screen_el IMPLEMENTATION.
  METHOD class_teardown.
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD setup.
    ROLLBACK WORK.
    cut = NEW zcl_translatable_screen_el( c_test_prog ).
  ENDMETHOD.

  METHOD read_text_in_one_language.
    cut->read_language( c_lang_en ).
    DATA(read_texts) = cut->get_all_texts( ).

    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'SCREEN_TEXTS|0001|SRH4|' ]-translations[ sap_lang = c_lang_en ]-content
    exp = 'Screen 0001' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'SCREEN_TEXTS|0001|SRT4|RADIO_BUTTON' ]-translations[ sap_lang = c_lang_en ]-content
    exp = 'RADIO BUTTON' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'SCREEN_TEXTS|0002|SRH4|' ]-translations[ sap_lang = c_lang_en ]-content
    exp = 'TABSTRIP 1 TITLE' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'SCREEN_TEXTS|0001|SRT4|TADIR-OBJ_NAME' ]-translations[ sap_lang = c_lang_en ]-content
    exp = 'CUSTOM COL TEXT' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'SCREEN_TEXTS|0001|SRT4|TABLE_CONTROL_NEXT' ]-translations[ sap_lang = c_lang_en ]-content
    exp = '@30@' ).
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
    DATA(cut_casted) = CAST zcl_translatable_screen_el( cut ).

    LOOP AT cut_casted->texts REFERENCE INTO DATA(text).
      CLEAR text->translations.
      APPEND VALUE #( sap_lang = c_lang_pl content = |{ c_lang_pl }{ text->text_id }| ) TO text->translations.
    ENDLOOP.
  ENDMETHOD.

  METHOD modify_texts.
    cut->read_language( c_lang_en ).

    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->object_name
          object_type = cut->object_type
    ( text_id = 'SCREEN_TEXTS|0001|SRH4|' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified' ) ) )
    ( text_id = 'SCREEN_TEXTS|0001|SRT4|RADIO_BUTTON' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified radiobutton' ) ) )
    ( text_id = 'SCREEN_TEXTS|0002|SRH4|' translations = VALUE #( ( sap_lang = c_lang_pl content = 'New submenu in another lang' ) ) ) ).

    cut->modify_texts( new_texts ).
    "--------------------------------------------------
    DATA(current_texts) = cut->get_all_texts( ).

    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 1 ] table = current_texts msg = |Text 0001 SRH4 not modified| ).
    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 2 ] table = current_texts msg = |Text RADIO_BUTTON not modified| ).
    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 3 ]-translations[ sap_lang = c_lang_pl ]
    table = current_texts[ KEY id_only text_id = 'SCREEN_TEXTS|0002|SRH4|' ]-translations
        msg = |Another language version of text SUBMENU1 not added| ).
    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 3 ]-translations[ sap_lang = c_lang_pl ]
    table = current_texts[ KEY id_only text_id = 'SCREEN_TEXTS|0002|SRH4|' ]-translations
        msg = |Text SUBMENU1 in EN shouldn't be modified| ).
  ENDMETHOD.


  METHOD save_modified_in_db.
    cut->read_language( c_lang_en ).
    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->object_name
                                                       object_type = cut->object_type
    ( text_id = 'SCREEN_TEXTS|0001|SRH4|' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified' )
                                                                 ( sap_lang = c_lang_pl content = 'New in another lang' ) ) )
    ( text_id = 'SCREEN_TEXTS|0001|SRT4|RADIO_BUTTON' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified radiobutton' )
                                                                   ( sap_lang = c_lang_pl content = 'New in another lang radiobutton' ) ) ) ).
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
    WHERE targlng = @lxe_lang_pl AND objtype = 'SRH4' AND objname = 'ZABAP_TRANSLATIONS_TEST_PROG            0001'
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy3).
    cl_abap_unit_assert=>assert_subrc( exp = 4 msg = |Change log in PL shouldn't be found| ).

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'SRT4' AND objname = 'ZABAP_TRANSLATIONS_TEST_PROG            0001'
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy4).
    cl_abap_unit_assert=>assert_subrc( exp = 4 msg = |Change log in PL shouldn't be found| ).

    "Both lang
    save_modified_in_db( ).
    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_en AND objtype = 'SRH4' AND objname = 'ZABAP_TRANSLATIONS_TEST_PROG            0001'
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy1).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log in EN not found| ).

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_en AND objtype = 'SRT4' AND objname = 'ZABAP_TRANSLATIONS_TEST_PROG            0001'
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy5).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log in EN not found| ).

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'SRH4' AND objname = 'ZABAP_TRANSLATIONS_TEST_PROG            0001'
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy6).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log in PL not found| ).

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'SRT4' AND objname = 'ZABAP_TRANSLATIONS_TEST_PROG            0001'
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy2).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log in PL not found| ).
  ENDMETHOD.


  METHOD verify_d020t_after_save.
    SELECT * FROM d020t WHERE prog = @c_test_prog INTO TABLE @DATA(d020t).
    DATA(expected) = d020t.
    expected[ prog = c_test_prog dynr = '0001' lang = c_lang_en ]-dtxt = 'Modified'.
    APPEND VALUE #( prog = c_test_prog dynr = '0001' lang = c_lang_pl dtxt = 'New in another lang' ) TO expected.

    "Verification - another language not saved
    save_modified_in_db( pl = abap_false ).
    SELECT * FROM d020t WHERE prog = @c_test_prog AND lang = @c_lang_pl INTO TABLE @d020t.
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( d020t ) msg = |Another lang shouldn't be saved| ).

    "Verification
    save_modified_in_db( ).

    SELECT * FROM d020t WHERE prog = @c_test_prog INTO TABLE @d020t.

    SORT: expected BY dynr lang, d020t BY dynr lang.
    cl_abap_unit_assert=>assert_equals( exp = expected act = d020t ).
  ENDMETHOD.

  METHOD verify_d021t_after_save.
    SELECT * FROM d021t WHERE prog = @c_test_prog INTO TABLE @DATA(d021t).
    DATA(expected) = d021t.
    expected[ prog = c_test_prog dynr = '0001' lang = c_lang_en fldn = 'RADIO_BUTTON' ]-dtxt = 'Modified radiobutton'.
    APPEND VALUE #( prog = c_test_prog dynr = '0001' lang = c_lang_pl fldn = 'RADIO_BUTTON'
        dtxt = 'New in another lang radiobutton' ) TO expected.

    "Verification - another language not saved
    save_modified_in_db( pl = abap_false ).
    SELECT * FROM d021t WHERE prog = @c_test_prog AND lang = @c_lang_pl INTO TABLE @d021t.
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( d021t ) msg = |Another lang shouldn't be saved| ).

    "Verification
    save_modified_in_db( ).

    SELECT * FROM d021t WHERE prog = @c_test_prog INTO TABLE @d021t.

    SORT: expected BY dynr lang fldn, d021t BY dynr lang fldn.
    cl_abap_unit_assert=>assert_equals( exp = expected act = d021t ).
  ENDMETHOD.

ENDCLASS.
