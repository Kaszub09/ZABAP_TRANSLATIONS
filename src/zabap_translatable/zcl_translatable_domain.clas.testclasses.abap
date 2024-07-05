*"* use this source file for your ABAP unit test classes
CONSTANTS:
  c_test_domain TYPE lxeobjname VALUE 'ZABAP_TRANSLATIONS_TEST_DOMAIN',
  c_lang_pl     TYPE sy-langu VALUE 'L',
  c_lang_en     TYPE sy-langu VALUE 'E'.

CLASS ltcl_translatable_domain DEFINITION DEFERRED.
CLASS zcl_translatable_domain DEFINITION LOCAL FRIENDS ltcl_translatable_domain.
CLASS ltcl_translatable_domain DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES:
        tt_domain TYPE STANDARD TABLE OF textpool WITH EMPTY KEY.
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
      verify_descriptions_after_save FOR TESTING,
      verify_values_after_save FOR TESTING.

    DATA:
        cut TYPE REF TO zif_translatable.
ENDCLASS.


CLASS ltcl_translatable_domain IMPLEMENTATION.
  METHOD class_teardown.
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD setup.
    ROLLBACK WORK.
    cut = NEW zcl_translatable_domain( CONV #( c_test_domain ) ).
  ENDMETHOD.

  METHOD read_text_in_one_language.
    cut->read_language( c_lang_en ).
    DATA(read_texts) = cut->get_all_texts( ).

    "Texts
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'DD01T|0000|A|0000' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Test domain' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'DD07T|0001|A|0000' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Value 1 description' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'DD07T|0002|A|0000' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Value 2 description' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'DD07T|0003|A|0000' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Range 1 description' ).
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
    DATA(cut_casted) = CAST zcl_translatable_domain( cut ).

    LOOP AT cut_casted->texts REFERENCE INTO DATA(text).
      CLEAR text->translations.
      APPEND VALUE #( sap_lang = c_lang_pl content = |{ c_lang_pl }{ text->text_id }| ) TO text->translations.
    ENDLOOP.
  ENDMETHOD.

  METHOD modify_texts.
    cut->read_language( c_lang_en ).

    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->object_name
                                                       object_type = cut->object_type
        ( text_id = 'DD01T|0000|A|0000' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified' ) ) )
        ( text_id = 'DD07T|0002|A|0000' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified' ) ) )
        ( text_id = 'DD07T|0003|A|0000' translations = VALUE #( ( sap_lang = c_lang_pl content = 'New in another lang' ) ) ) ).

    cut->modify_texts( new_texts ).
    "--------------------------------------------------
    DATA(current_texts) = cut->get_all_texts( ).

    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 3 ]-translations[ sap_lang = c_lang_pl ]
        table = current_texts[ KEY id_only text_id = 'DD07T|0003|A|0000' ]-translations msg = |Another language version of text 0003 not added| ).
    cl_abap_unit_assert=>assert_equals( act = current_texts[ KEY id_only text_id = 'DD07T|0003|A|0000' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Range 1 description' msg = |Text 0003 in EN shouldn't be modified| ).
    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 2 ] table = current_texts msg = |Text 0002 not modified| ).
    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 1 ] table = current_texts msg = |Text 0000 not modified| ).
  ENDMETHOD.


  METHOD save_modified_in_db.
    cut->read_language( c_lang_en ).
    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->object_name
                                                       object_type = cut->object_type
        ( text_id = 'DD01T|0000|A|0000' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified' ) ) )
        ( text_id = 'DD07T|9999|A|0000' translations = VALUE #( ( sap_lang = c_lang_en content = 'New' ) ) )
        ( text_id = 'DD07T|0002|A|0000' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified' ) ) )
        ( text_id = 'DD07T|0003|A|0000' translations = VALUE #( ( sap_lang = c_lang_pl content = 'New in another lang' ) ) ) ).
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
    WHERE targlng = @lxe_lang_pl AND objtype = 'VALU' AND objname = @c_test_domain
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy5).
    cl_abap_unit_assert=>assert_subrc( exp = 4 msg = |Change log VALU in PL shouldn't be found| ).
    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'DOMA' AND objname = @c_test_domain
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy6).
    cl_abap_unit_assert=>assert_subrc( exp = 4 msg = |Change log DOMA in PL shouldn't be found| ).


    "Both lang
    save_modified_in_db( ).
    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_en AND objtype = 'VALU' AND objname = @c_test_domain
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy1).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log VALU in EN not found| ).
    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_en AND objtype = 'DOMA' AND objname = @c_test_domain
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy2).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log DOMA in EN not found| ).

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'VALU' AND objname = @c_test_domain
       AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy3).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log VALU in PL not found| ).
    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'DOMA' AND objname = @c_test_domain
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy4).
    cl_abap_unit_assert=>assert_subrc( exp = 4 msg = |Change log DOMA in PL shouldn't be found| ).
  ENDMETHOD.

  METHOD verify_descriptions_after_save.
    SELECT * FROM dd01t WHERE domname = @c_test_domain INTO TABLE @DATA(dd01t).
    DATA(expected) = dd01t.
    expected[ domname = c_test_domain ddlanguage = c_lang_en as4local = 'A' as4vers = '0000' ]-ddtext = 'Modified'.

    "Verification - another language not saved
    save_modified_in_db( pl = abap_false ).
    SELECT * FROM dd01t WHERE domname = @c_test_domain and ddlanguage = @c_lang_pl INTO TABLE @dd01t.
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( dd01t ) msg = |Another lang shouldn't be saved| ).

    "Verification
    save_modified_in_db(  ).

    SELECT * FROM dd01t WHERE domname = @c_test_domain INTO TABLE @dd01t.
    cl_abap_unit_assert=>assert_equals( exp = expected act = dd01t ).
  ENDMETHOD.

  METHOD verify_values_after_save.
    SELECT * FROM dd07t WHERE domname = @c_test_domain INTO TABLE @DATA(dd07t).
    DATA(expected) = dd07t.
    expected[ domname = c_test_domain ddlanguage = c_lang_en valpos = '0002' as4local = 'A' as4vers = '0000' ]-ddtext = 'Modified'.
    APPEND VALUE #( domname = c_test_domain ddlanguage = c_lang_en valpos = '9999' as4local = 'A' as4vers = '0000' ddtext = 'New' ) TO expected.
    APPEND VALUE #( domname = c_test_domain ddlanguage = c_lang_pl valpos = '0003' as4local = 'A' as4vers = '0000'
                    ddtext = 'New in another lang' domvalue_l = '0' ) TO expected.

    "Verification - another language not saved
    save_modified_in_db( pl = abap_false ).
    SELECT * FROM dd07t WHERE domname = @c_test_domain AND ddlanguage = @c_lang_pl INTO TABLE @dd07t.
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( dd07t ) msg = |Another lang shouldn't be saved| ).

    "Verification
    save_modified_in_db(  ).

    SELECT * FROM dd07t WHERE domname = @c_test_domain INTO TABLE @dd07t.

    SORT: expected BY valpos ddlanguage as4local as4vers, dd07t BY valpos ddlanguage as4local as4vers.
    cl_abap_unit_assert=>assert_equals( exp = expected act = dd07t ).
  ENDMETHOD.

ENDCLASS.
