*"* use this source file for your ABAP unit test classes
CONSTANTS:
  c_test_table TYPE lxeobjname VALUE 'ZABAP_TRA_TEST_T',
  c_lang_pl    TYPE sy-langu VALUE 'L',
  c_lang_en    TYPE sy-langu VALUE 'E'.

CLASS ltcl_translatable_table DEFINITION DEFERRED.
CLASS zcl_translatable_table DEFINITION LOCAL FRIENDS ltcl_translatable_table.
CLASS ltcl_translatable_table DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
        tt_table TYPE STANDARD TABLE OF textpool WITH EMPTY KEY.

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
      verify_dd02t_after_save FOR TESTING,
      verify_dd03t_after_save FOR TESTING,
      verify_dd08t_after_save FOR TESTING,
      verify_another_lang_not_saved FOR TESTING.

    DATA:
        cut TYPE REF TO zif_translatable.
ENDCLASS.

CLASS ltcl_translatable_table IMPLEMENTATION.
  METHOD class_teardown.
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD setup.
    ROLLBACK WORK.
    cut = NEW zcl_translatable_table( CONV #( c_test_table ) ).
  ENDMETHOD.

  METHOD read_text_in_one_language.
    cut->read_language( c_lang_en ).
    DATA(read_texts) = cut->get_all_texts( ).

    "Texts
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'DD02T||A|0000' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Translations test table' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'DD03T|FIELD_1|A|0000' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Field 1 description' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'DD08T|MANDT|A|0000' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Foreign key description' ).
  ENDMETHOD.

  METHOD read_text_in_multiple_lang.
    mock_texts_read_in_polish( ).
    cut->read_language( c_lang_en ).

    DATA(read_texts) = cut->get_all_texts( ).

    LOOP AT read_texts REFERENCE INTO DATA(read_text).
      cl_abap_unit_assert=>assert_equals( act = lines( read_text->translations )
        exp = 2 msg = |Expected translations in 2 langauges - ID { read_text->text_id }| ).
      cl_abap_unit_assert=>assert_differs( act = read_text->translations[ sap_lang = c_lang_en ]
        exp = read_text->translations[ sap_lang = c_lang_pl ] msg = |Expected different message in another lang  { read_text->text_id }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD mock_texts_read_in_polish.
    cut->read_language( c_lang_en ).
    DATA(cut_casted) = CAST zcl_translatable_table( cut ).

    LOOP AT cut_casted->texts REFERENCE INTO DATA(text).
      CLEAR text->translations.
      APPEND VALUE #( sap_lang = c_lang_pl content = |{ c_lang_pl }{ text->text_id }| ) TO text->translations.
    ENDLOOP.
  ENDMETHOD.

  METHOD modify_texts.
    cut->read_language( c_lang_en ).

    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->object_name
                                                       object_type = cut->object_type
        ( text_id = 'DD02T||A|0000' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified' )
                                                            ( sap_lang = c_lang_pl content = 'New in another lang' ) ) ) ).

    cut->modify_texts( new_texts ).
    "--------------------------------------------------
    DATA(current_texts) = cut->get_all_texts( ).

    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 1 ] table = current_texts msg = |Text DD02T not modified| ).
    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 1 ]-translations[ sap_lang = c_lang_pl ]
        table = current_texts[ KEY id_only text_id = 'DD02T||A|0000' ]-translations msg = |Another language version of text DD02T not added| ).
  ENDMETHOD.

  METHOD save_modified_in_db.
    cut->read_language( c_lang_en ).
    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->object_name
                                                       object_type = cut->object_type
        ( text_id = 'DD02T||A|0000' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified DD02T' )
                                                            ( sap_lang = c_lang_pl content = 'New in another lang DD02T' ) ) )
        ( text_id = 'DD03T|FIELD_1|A|0000' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified DD03T' )
                                                                   ( sap_lang = c_lang_pl content = 'New in another lang DD03T' ) ) )
        ( text_id = 'DD08T|MANDT|A|0000' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified DD08T' )
                                                                 ( sap_lang = c_lang_pl content = 'New in another lang DD08T' ) ) ) ).
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
    WHERE targlng = @lxe_lang_pl AND objtype = 'TABT' AND objname = @c_test_table
       AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy5).
    cl_abap_unit_assert=>assert_subrc( exp = 4 msg = |Change log in PL shouldn't be found| ).
    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'BEZD' AND objname = @c_test_table
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy6).
    cl_abap_unit_assert=>assert_subrc( exp = 4 msg = |Change log in PL shouldn't be found| ).

    "Both lang
    save_modified_in_db( ).
    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_en AND objtype = 'TABT' AND objname = @c_test_table
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy1).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log TABT in EN not found| ).
    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_en AND objtype = 'BEZD' AND objname = @c_test_table
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy2).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log BEZD in EN not found| ).

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'TABT' AND objname = @c_test_table
       AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy3).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log TABT in PL not found| ).
    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'BEZD' AND objname = @c_test_table
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy4).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log BEZD in PL not found| ).
  ENDMETHOD.

  METHOD verify_dd02t_after_save.
    SELECT * FROM dd02t WHERE tabname = @c_test_table INTO TABLE @DATA(dd02t).
    DATA(expected) = dd02t.
    expected[ tabname = c_test_table ddlanguage = c_lang_en as4local = 'A' as4vers = '0000' ]-ddtext = 'Modified DD02T'.
    APPEND VALUE #( tabname = c_test_table ddlanguage = c_lang_pl as4local = 'A' as4vers = '0000'
                    ddtext = 'New in another lang DD02T' ) TO expected.

    "Verification
    save_modified_in_db( ).

    SELECT * FROM dd02t WHERE tabname = @c_test_table INTO TABLE @dd02t.

    SORT: expected BY ddlanguage as4local as4vers, dd02t BY ddlanguage as4local as4vers.
    cl_abap_unit_assert=>assert_equals( exp = expected act = dd02t ).
  ENDMETHOD.

  METHOD verify_dd03t_after_save.
    SELECT * FROM dd03t WHERE tabname = @c_test_table INTO TABLE @DATA(dd03t).
    DATA(expected) = dd03t.
    expected[ tabname = c_test_table ddlanguage = c_lang_en as4local = 'A' fieldname = 'FIELD_1' ]-ddtext = 'Modified DD03T'.
    APPEND VALUE #( tabname = c_test_table ddlanguage = c_lang_pl as4local = 'A' fieldname = 'FIELD_1'
                    ddtext = 'New in another lang DD03T' ) TO expected.

    "Verification
    save_modified_in_db( ).

    SELECT * FROM dd03t WHERE tabname = @c_test_table INTO TABLE @dd03t.

    SORT: expected BY ddlanguage as4local fieldname, dd03t BY ddlanguage as4local fieldname.
    cl_abap_unit_assert=>assert_equals( exp = expected act = dd03t ).
  ENDMETHOD.

  METHOD verify_dd08t_after_save.
    SELECT * FROM dd08t WHERE tabname = @c_test_table INTO TABLE @DATA(dd08t).
    DATA(expected) = dd08t.
    expected[ tabname = c_test_table ddlanguage = c_lang_en as4local = 'A' fieldname = 'MANDT' as4vers = '0000' ]-ddtext = 'Modified DD08T'.
    APPEND VALUE #( tabname = c_test_table ddlanguage = c_lang_pl as4local = 'A' fieldname = 'MANDT' as4vers = '0000'
                    ddtext = 'New in another lang DD08T' ) TO expected.

    "Verification
    save_modified_in_db( ).

    SELECT * FROM dd08t WHERE tabname = @c_test_table INTO TABLE @dd08t.

    SORT: expected BY ddlanguage as4local fieldname, dd08t BY ddlanguage as4local fieldname.
    cl_abap_unit_assert=>assert_equals( exp = expected act = dd08t ).
  ENDMETHOD.

  METHOD verify_another_lang_not_saved.
    SELECT * FROM dd02t WHERE tabname = @c_test_table AND ddlanguage = @c_lang_pl ORDER BY PRIMARY KEY INTO TABLE @DATA(dd02t).
    SELECT * FROM dd03t WHERE tabname = @c_test_table AND ddlanguage = @c_lang_pl ORDER BY PRIMARY KEY INTO TABLE @DATA(dd03t).
    SELECT * FROM dd08t WHERE tabname = @c_test_table AND ddlanguage = @c_lang_pl ORDER BY PRIMARY KEY INTO TABLE @DATA(dd08t).
    DATA(expected_dd02t) = dd02t.
    DATA(expected_dd03t) = dd03t.
    DATA(expected_dd08t) = dd08t.

    save_modified_in_db( pl = abap_false ).

    SELECT * FROM dd02t WHERE tabname = @c_test_table AND ddlanguage = @c_lang_pl ORDER BY PRIMARY KEY INTO TABLE @dd02t.
    SELECT * FROM dd03t WHERE tabname = @c_test_table AND ddlanguage = @c_lang_pl ORDER BY PRIMARY KEY INTO TABLE @dd03t.
    SELECT * FROM dd08t WHERE tabname = @c_test_table AND ddlanguage = @c_lang_pl ORDER BY PRIMARY KEY INTO TABLE @dd08t.
    cl_abap_unit_assert=>assert_equals( exp = expected_dd02t act = dd02t ).
    cl_abap_unit_assert=>assert_equals( exp = expected_dd03t act = dd03t ).
    cl_abap_unit_assert=>assert_equals( exp = expected_dd08t act = dd08t ).


    DATA(lxe_log) = zcl_translation_factory=>get_lxe_log( ).
    DATA(lxe_lang_pl) = lxe_log->lxe_languages[ sap = c_lang_pl ]-lxe_lang.

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'TABT' AND objname = @c_test_table
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy3).
    cl_abap_unit_assert=>assert_subrc( exp = 4 ).

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'BEZD' AND objname = @c_test_table
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy4).
    cl_abap_unit_assert=>assert_subrc( exp = 4 ).
  ENDMETHOD.

ENDCLASS.
