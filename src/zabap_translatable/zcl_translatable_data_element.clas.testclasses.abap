*"* use this source file for your ABAP unit test classes
CONSTANTS:
  c_test_de TYPE lxeobjname VALUE 'ZABAP_TRANSLATION_TEST_DE',
  c_lang_pl TYPE sy-langu VALUE 'L',
  c_lang_en TYPE sy-langu VALUE 'E'.

CLASS ltcl_translatable_data_element DEFINITION DEFERRED.
CLASS zcl_translatable_data_element DEFINITION LOCAL FRIENDS ltcl_translatable_data_element.
CLASS ltcl_translatable_data_element DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
        tt_data_element TYPE STANDARD TABLE OF textpool WITH EMPTY KEY.

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
      verify_texts_after_save FOR TESTING,
      verify_texts_length_after_save FOR TESTING.

    DATA:
        cut TYPE REF TO zif_translatable.
ENDCLASS.

CLASS ltcl_translatable_data_element IMPLEMENTATION.
  METHOD class_teardown.
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD setup.
    ROLLBACK WORK.
    cut = NEW zcl_translatable_data_element( CONV #( c_test_de ) ).
  ENDMETHOD.

  METHOD read_text_in_one_language.
    cut->read_language( c_lang_en ).
    DATA(read_texts) = cut->get_all_texts( ).

    "Texts
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'D60|A|0000' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Translations test Data element' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'S10|A|0000' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Short' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'M20|A|0000' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Medium' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'L40|A|0000' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Long' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'H55|A|0000' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Heading' ).
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
    DATA(cut_casted) = CAST zcl_translatable_data_element( cut ).

    LOOP AT cut_casted->texts REFERENCE INTO DATA(text).
      CLEAR text->translations.
      APPEND VALUE #( sap_lang = c_lang_pl content = |{ c_lang_pl }{ text->text_id }| ) TO text->translations.
    ENDLOOP.
  ENDMETHOD.

  METHOD modify_texts.
    cut->read_language( c_lang_en ).

    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->object_name
                                                       object_type = cut->object_type
        ( text_id = 'M20|A|0000' translations = VALUE #( ( sap_lang = c_lang_pl content = 'New in another lang' ) ) )
        ( text_id = 'L40|A|0000' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified' ) ) )
        ( text_id = 'H55|A|0000' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified2' ) ) ) ).

    cut->modify_texts( new_texts ).
    "--------------------------------------------------
    DATA(current_texts) = cut->get_all_texts( ).

    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 1 ]-translations[ sap_lang = c_lang_pl ]
        table = current_texts[ KEY id_only text_id = 'M20|A|0000' ]-translations msg = |Another language version of text M20 not added| ).
    cl_abap_unit_assert=>assert_equals( act = current_texts[ KEY id_only text_id = 'M20|A|0000' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Medium' msg = |Text M20 in EN shouldn't be modified| ).
    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 2 ] table = current_texts msg = |Text L40 not modified| ).
    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 3 ] table = current_texts msg = |Text H55 not modified| ).
  ENDMETHOD.

  METHOD save_modified_in_db.
    cut->read_language( c_lang_en ).
    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->object_name
                                                       object_type = cut->object_type
        ( text_id = 'M20|A|0000' translations = VALUE #( ( sap_lang = c_lang_pl content = 'New in another lang' )
                                                         ( sap_lang = c_lang_en content = 'Modified' ) ) )
        ( text_id = 'L40|A|0000' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified super extra long text' ) ) ) ).
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
    WHERE targlng = @lxe_lang_pl AND objtype = 'DTEL' AND objname = @c_test_de
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy0).
    cl_abap_unit_assert=>assert_subrc( exp = 4 msg = |Change log in PL shouldn't be found| ).

    "Both lang
    save_modified_in_db( ).
    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_en AND objtype = 'DTEL' AND objname = @c_test_de
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy1).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log in EN not found| ).

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'DTEL' AND objname = @c_test_de
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy2).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log in PL not found| ).
  ENDMETHOD.

  METHOD verify_texts_after_save.
    SELECT * FROM dd04t WHERE rollname = @c_test_de AND ddlanguage = @c_lang_en INTO TABLE @DATA(dd04t_en).
    SELECT * FROM dd04t WHERE rollname = @c_test_de AND ddlanguage = @c_lang_pl INTO TABLE @DATA(dd04t_pl).

    DATA(expected_en) = dd04t_en.
    DATA(expected_pl) = dd04t_pl.

    expected_en[ rollname = c_test_de ddlanguage = c_lang_en as4local = 'A' as4vers = '0000' ]-scrtext_m = 'Modified'.
    expected_en[ rollname = c_test_de ddlanguage = c_lang_en as4local = 'A' as4vers = '0000' ]-scrtext_l = 'Modified super extra long text'.
    APPEND VALUE #( rollname = c_test_de ddlanguage = c_lang_pl as4local = 'A' as4vers = '0000' scrtext_m = 'New in another lang' ) TO expected_pl.

    "Verification - another language not saved
    save_modified_in_db( pl = abap_false ).
    SELECT * FROM dd04t WHERE rollname = @c_test_de AND ddlanguage = @c_lang_pl INTO TABLE @dd04t_pl.
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( dd04t_pl ) msg = |Another lang shouldn't be saved| ).

    "Verification - both languages saved
    save_modified_in_db( ).
    SELECT * FROM dd04t WHERE rollname = @c_test_de AND ddlanguage = @c_lang_en INTO TABLE @dd04t_en.
    SELECT * FROM dd04t WHERE rollname = @c_test_de AND ddlanguage = @c_lang_pl INTO TABLE @dd04t_pl.
    cl_abap_unit_assert=>assert_equals( exp = expected_en act = dd04t_en ).
    cl_abap_unit_assert=>assert_equals( exp = expected_pl act = dd04t_pl ).
  ENDMETHOD.

  METHOD verify_texts_length_after_save.
    SELECT * FROM dd04l WHERE rollname = @c_test_de INTO TABLE @DATA(dd04l).

    DATA(expected) = dd04l.
    expected[ rollname = c_test_de as4local = 'A' as4vers = '0000' ]-scrlen2 = strlen( 'New in another lang' ).
    expected[ rollname = c_test_de as4local = 'A' as4vers = '0000' ]-scrlen3 = strlen( 'Modified super extra long text' ).

    "Verification
    save_modified_in_db( ).

    SELECT * FROM dd04l WHERE rollname = @c_test_de INTO TABLE @dd04l.
    cl_abap_unit_assert=>assert_equals( exp = expected act = dd04l ).
  ENDMETHOD.
ENDCLASS.
