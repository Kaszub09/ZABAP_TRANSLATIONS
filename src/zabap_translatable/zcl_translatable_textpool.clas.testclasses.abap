CONSTANTS:
  c_test_prog              TYPE sobj_name VALUE 'ZABAP_TRANSLATIONS_TEST_PROG',
  c_lang_pl                TYPE sy-langu VALUE 'L',
  c_lang_en                TYPE sy-langu VALUE 'E',
  c_sel_text_ref           TYPE c LENGTH 9 VALUE 'D       .',
  c_sel_text_no_ref_prefix TYPE c LENGTH 9 VALUE '         '.

CLASS ltcl_translatable_textpool DEFINITION DEFERRED.
CLASS zcl_translatable_textpool DEFINITION LOCAL FRIENDS ltcl_translatable_textpool.
CLASS ltcl_translatable_textpool DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES:
        tt_textpool TYPE STANDARD TABLE OF textpool WITH EMPTY KEY.
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
      verify_textpools_after_save FOR TESTING,
      verify_textpool IMPORTING textpool TYPE tt_textpool sap_lang TYPE sy-langu.

    DATA:
        cut TYPE REF TO zif_translatable.
ENDCLASS.


CLASS ltcl_translatable_textpool IMPLEMENTATION.
  METHOD class_teardown.
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD setup.
    ROLLBACK WORK.
    cut = NEW zcl_translatable_textpool( c_test_prog ).
  ENDMETHOD.

  METHOD read_text_in_one_language.
    cut->read_language( c_lang_en ).
    DATA(read_texts) = cut->get_all_texts( ).

    "Texts
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'TEXTPOOL|I|001' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Message 1 EN' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'TEXTPOOL|I|002' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Message 2 EN' ).
    "Description
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'TEXTPOOL|R|' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Test program for translations' ).
    "Selection texts - first 8 character are reserved (spaces for no ref, 'D' and spaces for ref
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'TEXTPOOL|S|P_NO_REF' ]-translations[ sap_lang = c_lang_en ]-content
        exp = '        P No ref' ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'TEXTPOOL|S|S_NO_REF' ]-translations[ sap_lang = c_lang_en ]-content
        exp = |        SO No ref| ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'TEXTPOOL|S|P_REF' ]-translations[ sap_lang = c_lang_en ]-content
        exp = c_sel_text_ref ).
    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = 'TEXTPOOL|S|S_REF' ]-translations[ sap_lang = c_lang_en ]-content
        exp = c_sel_text_ref ).
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
    DATA(cut_casted) = CAST zcl_translatable_textpool( cut ).

    LOOP AT cut_casted->texts REFERENCE INTO DATA(text).
      CLEAR text->translations.
      APPEND VALUE #( sap_lang = c_lang_pl content = |{ c_lang_pl }{ text->text_id }| ) TO text->translations.
    ENDLOOP.
  ENDMETHOD.

  METHOD modify_texts.
    cut->read_language( c_lang_en ).

    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->object_name
                                                       object_type = cut->object_type
        ( text_id = 'TEXTPOOL|I|001' translations = VALUE #( ( sap_lang = c_lang_pl content = 'New in another lang' ) ) )
        ( text_id = 'TEXTPOOL|I|002' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified' ) ) )
        ( text_id = 'TEXTPOOL|I|999' translations = VALUE #( ( sap_lang = c_lang_en content = 'New' ) ) ) ).

    cut->modify_texts( new_texts ).
    "--------------------------------------------------
    DATA(current_texts) = cut->get_all_texts( ).

    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 2 ] table = current_texts msg = |Text 002 not modified| ).
    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 3 ] table = current_texts msg = |Text 999 not inserted| ).
    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 1 ]-translations[ sap_lang = c_lang_pl ]
        table = current_texts[ KEY id_only text_id = 'TEXTPOOL|I|001' ]-translations msg = |Another language version of text 001 not added| ).
    cl_abap_unit_assert=>assert_equals( act = current_texts[ KEY id_only text_id = 'TEXTPOOL|I|001' ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Message 1 EN' msg = |Text 001 in EN shouldn't be modified| ).
  ENDMETHOD.


  METHOD save_modified_in_db.
    cut->read_language( c_lang_en ).
    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->object_name
                                                       object_type = cut->object_type
        "EN
        ( text_id = 'TEXTPOOL|I|001' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified' ) ) )
        ( text_id = 'TEXTPOOL|I|999' translations = VALUE #( ( sap_lang = c_lang_en content = 'New' ) ) )
        ( text_id = 'TEXTPOOL|S|P_NO_REF' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified P_NO_REF' ) ) )
        "PL
        ( text_id = 'TEXTPOOL|I|002' translations = VALUE #( ( sap_lang = c_lang_pl content = 'New in another lang' ) ) )
        ( text_id = 'TEXTPOOL|S|P_REF' translations = VALUE #( ( sap_lang = c_lang_pl content = 'D       .' ) ) ) ).
    cut->modify_texts( new_texts ).

    IF en = abap_true.
      cut->save_modified_texts( c_lang_en ).
    ENDIF.
    IF en = abap_true.
      cut->save_modified_texts( c_lang_pl ).
    ENDIF.
  ENDMETHOD.

  METHOD verify_lxe_log_after_save.
    save_modified_in_db( ).

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @c_lang_en AND objtype = 'RPT4' AND objname = @c_test_prog
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy1).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log in EN not found| ).

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @c_lang_pl AND objtype = 'RPT4' AND objname = @c_test_prog
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy2).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log in PL not found| ).
  ENDMETHOD.


  METHOD verify_textpools_after_save.
    DATA:
      textpool_en TYPE tt_textpool,
      textpool_pl TYPE tt_textpool.
    READ TEXTPOOL c_test_prog INTO textpool_en LANGUAGE c_lang_en.
    READ TEXTPOOL c_test_prog INTO textpool_pl LANGUAGE c_lang_pl.

    save_modified_in_db(  ).

    "Build expected based on changes
    DATA(expected_en) = textpool_en.
    expected_en[ id = 'I' key = '001' ]-entry = 'Modified'.
    APPEND VALUE #( id = 'I' key = '999' entry = 'New' ) TO expected_en.
    expected_en[ id = 'S' key = 'P_NO_REF' ]-entry = '        Modified P_NO_REF'.

    DATA(expected_pl) = textpool_pl.
    APPEND VALUE #( id = 'I' key = '002' entry = 'New in another lang' ) TO expected_pl.
    APPEND VALUE #( id = 'S' key = 'P_REF' entry = 'D       .' ) TO expected_pl.

    "Verify
    verify_textpool( textpool = expected_en sap_lang = c_lang_en ).
    verify_textpool( textpool = expected_pl sap_lang = c_lang_pl ).
  ENDMETHOD.

  METHOD verify_textpool.
    DATA(textpool_exp) = textpool.
    DATA textpool_act TYPE tt_textpool.
    READ TEXTPOOL c_test_prog INTO textpool_act LANGUAGE sap_lang.

    "Clear length field - no need to compare, adjusted automatically when uploading new - even if translation is longer than max. of original
    LOOP AT textpool_exp REFERENCE INTO DATA(textpool_exp_row).
      textpool_exp_row->length = 0.
    ENDLOOP.
    LOOP AT textpool_act REFERENCE INTO DATA(textpool_act_row).
      textpool_act_row->length = 0.
    ENDLOOP.

    SORT: textpool_exp BY id key, textpool_act BY id key.
    cl_abap_unit_assert=>assert_equals( exp = textpool_exp act = textpool_act msg = |Different textpool in language { sap_lang }| ).
  ENDMETHOD.

ENDCLASS.
