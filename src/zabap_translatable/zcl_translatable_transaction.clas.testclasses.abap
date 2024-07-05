CONSTANTS:
  c_test_transaction TYPE sobj_name VALUE 'ZABAP_TRANS_L_A_TEST',
  c_lang_pl          TYPE sy-langu VALUE 'L',
  c_lang_en          TYPE sy-langu VALUE 'E'.

CLASS ltcl_translatable_transaction DEFINITION DEFERRED.
CLASS zcl_translatable_transaction DEFINITION LOCAL FRIENDS ltcl_translatable_transaction.
CLASS ltcl_translatable_transaction DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    TYPES:
        tt_transaction TYPE STANDARD TABLE OF textpool WITH EMPTY KEY.

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
      verify_transaction_after_save FOR TESTING.

    DATA:
        cut TYPE REF TO zif_translatable.
ENDCLASS.

CLASS ltcl_translatable_transaction IMPLEMENTATION.
  METHOD class_teardown.
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD setup.
    ROLLBACK WORK.
    cut = NEW zcl_translatable_transaction( c_test_transaction ).
  ENDMETHOD.

  METHOD read_text_in_one_language.
    cut->read_language( c_lang_en ).
    DATA(read_texts) = cut->get_all_texts( ).

    "Texts
    cl_abap_unit_assert=>assert_equals( act = read_texts[ 1 ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Translation test' ).
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
    DATA(cut_casted) = CAST zcl_translatable_transaction( cut ).

    LOOP AT cut_casted->texts REFERENCE INTO DATA(text).
      CLEAR text->translations.
      APPEND VALUE #( sap_lang = c_lang_pl content = |{ c_lang_pl }{ text->text_id }| ) TO text->translations.
    ENDLOOP.
  ENDMETHOD.

  METHOD modify_texts.
    cut->read_language( c_lang_en ).

    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->object_name
                                                       object_type = cut->object_type
        ( text_id = space translations = VALUE #( ( sap_lang = c_lang_pl content = 'New in another lang' )
                                                  ( sap_lang = c_lang_en content = 'Modified' ) ) ) ).

    cut->modify_texts( new_texts ).
    "--------------------------------------------------
    DATA(current_texts) = cut->get_all_texts( ).
    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 1 ]-translations[ sap_lang = c_lang_pl ]
        table = current_texts[ KEY id_only text_id = space ]-translations msg = |Another language version of text not added| ).
    cl_abap_unit_assert=>assert_equals( act = current_texts[ KEY id_only text_id = space ]-translations[ sap_lang = c_lang_en ]-content
        exp = 'Modified' msg = |Text in EN not modified| ).
  ENDMETHOD.

  METHOD save_modified_in_db.
    cut->read_language( c_lang_en ).
    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->object_name
                                                       object_type = cut->object_type
        ( text_id = space translations = VALUE #( ( sap_lang = c_lang_pl content = 'New in another lang' )
                                                  ( sap_lang = c_lang_en content = 'Modified' ) ) ) ).
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
    WHERE targlng = @lxe_lang_pl AND objtype = 'TRAN' AND objname = @c_test_transaction
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy3).
    cl_abap_unit_assert=>assert_subrc( exp = 4 msg = |Change log in PL shouldn't be found| ).

    "Both lang
    save_modified_in_db( ).
    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_en AND objtype = 'TRAN' AND objname = @c_test_transaction
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy1).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log in EN not found| ).

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'TRAN' AND objname = @c_test_transaction
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy2).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log in PL not found| ).
  ENDMETHOD.

  METHOD verify_transaction_after_save.
    "Build expected based on changes
    SELECT * FROM tstct WHERE tcode = @c_test_transaction INTO TABLE @DATA(transaction_texts).

    DATA(expected) = transaction_texts.
    expected[ sprsl = c_lang_en ]-ttext = 'Modified'.
    APPEND VALUE #( tcode = c_test_transaction sprsl = c_lang_pl ttext = 'New in another lang' ) TO expected.

    "Verification - another language not saved
    save_modified_in_db( pl = abap_false ).
    SELECT * FROM tstct WHERE tcode = @c_test_transaction AND sprsl = @c_lang_pl INTO TABLE @transaction_texts.
    cl_abap_unit_assert=>assert_equals( exp = 0 act = lines( transaction_texts ) msg = |Another lang shouldn't be saved| ).

    "Verify
    save_modified_in_db( ).
    SELECT * FROM tstct WHERE tcode = @c_test_transaction INTO TABLE @transaction_texts.
    cl_abap_unit_assert=>assert_equals( exp = expected act = transaction_texts ).
  ENDMETHOD.
ENDCLASS.
