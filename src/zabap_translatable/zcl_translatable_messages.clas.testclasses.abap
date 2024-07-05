CONSTANTS:
  c_test_msg_class TYPE sobj_name VALUE 'ZTRAN_TEST_MSG_CLASS',
  c_lang_pl        TYPE sy-langu VALUE 'L',
  c_lang_en        TYPE sy-langu VALUE 'E'.

CLASS ltcl_translatable_messages DEFINITION DEFERRED.
CLASS zcl_translatable_messages DEFINITION LOCAL FRIENDS ltcl_translatable_messages.
CLASS ltcl_translatable_messages DEFINITION FINAL FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
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
      verify_t100u_en_after_save FOR TESTING,
      verify_t100u_pl_after_save FOR TESTING,
      verify_t100_after_save FOR TESTING.

    DATA:
        cut TYPE REF TO zif_translatable.
ENDCLASS.

CLASS ltcl_translatable_messages IMPLEMENTATION.
  METHOD class_teardown.
    ROLLBACK WORK.
  ENDMETHOD.

  METHOD setup.
    ROLLBACK WORK.
    cut = NEW zcl_translatable_messages( c_test_msg_class ).
  ENDMETHOD.

  METHOD read_text_in_one_language.
    cut->read_language( c_lang_en ).
    DATA(read_texts) = cut->get_all_texts( ).

    MESSAGE ID c_test_msg_class TYPE 'S' NUMBER '001' INTO DATA(message1).

    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = '001' ]-translations[ sap_lang = c_lang_en ]-content
        exp = message1 ).
  ENDMETHOD.

  METHOD read_text_in_multiple_lang.
    mock_texts_read_in_polish( ).
    cut->read_language( c_lang_en ).

    DATA(read_texts) = cut->get_all_texts( ).
    cl_abap_unit_assert=>assert_equals( act = lines( read_texts[ KEY id_only text_id = '001' ]-translations )
        exp = 2 msg = |Expected translations in 2 langauges| ).

    MESSAGE ID c_test_msg_class TYPE 'S' NUMBER '001' INTO DATA(message1).

    cl_abap_unit_assert=>assert_equals( act = read_texts[ KEY id_only text_id = '001' ]-translations[ sap_lang = c_lang_en ]-content
        exp = message1 ).
    cl_abap_unit_assert=>assert_differs( act = read_texts[ KEY id_only text_id = '001' ]-translations[ sap_lang = c_lang_pl ]-content
        exp = message1 msg = |Expected different message in another language| ).
  ENDMETHOD.

  METHOD mock_texts_read_in_polish.
    cut->read_language( c_lang_en ).
    DATA(cut_casted) = CAST zcl_translatable_messages( cut ).

    LOOP AT cut_casted->texts REFERENCE INTO DATA(text).
      CLEAR text->translations.
      APPEND VALUE #( sap_lang = c_lang_pl content = |{ c_lang_pl }{ text->text_id }| ) TO text->translations.
    ENDLOOP.
  ENDMETHOD.

  METHOD modify_texts.
    cut->read_language( c_lang_en ).

    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->object_name object_type = cut->object_type
        ( text_id = '001' translations = VALUE #( ( sap_lang = c_lang_pl content = 'New in another lang' ) ) )
        ( text_id = '002' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified' ) ) )
        ( text_id = '999' translations = VALUE #( ( sap_lang = c_lang_en content = 'New' ) ) ) ).

    cut->modify_texts( new_texts ).
    "--------------------------------------------------
    MESSAGE ID c_test_msg_class TYPE 'S' NUMBER '001' INTO DATA(message1).
    DATA(current_texts) = cut->get_all_texts( ).

    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 2 ] table = current_texts msg = |Text 002 not modified| ).
    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 3 ] table = current_texts msg = |Text 999 not inserted| ).
    cl_abap_unit_assert=>assert_table_contains( line = new_texts[ 1 ]-translations[ sap_lang = c_lang_pl ]
        table = current_texts[ KEY id_only text_id = '001' ]-translations msg = |Another language version of text 001 not added| ).
    cl_abap_unit_assert=>assert_equals( act = current_texts[ KEY id_only text_id = '001' ]-translations[ sap_lang = c_lang_en ]-content
        exp = message1 msg = |Text 001 in EN shouldn't be modified| ).
  ENDMETHOD.

  METHOD save_modified_in_db.
    cut->read_language( c_lang_en ).
    DATA(new_texts) = VALUE zif_translatable=>tt_text( object_name = cut->object_name object_type = cut->object_type
        ( text_id = '001' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified' ) ) )
        ( text_id = '002' translations = VALUE #( ( sap_lang = c_lang_pl content = 'New in another lang' ) ) ) ).
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

    save_modified_in_db( ).
    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_en AND objtype = 'MESS' AND objname = 'ZTRAN_TEST_MSG_CLASS001'
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy2).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log for MSG 001 in EN not found| ).

    "It's not modified, but saved anyway since we don't differentiate if read text was or wasn't modified
    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_en AND objtype = 'MESS' AND objname = 'ZTRAN_TEST_MSG_CLASS002'
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy4).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log for MSG 002 in EN not found| ).

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'MESS' AND objname = 'ZTRAN_TEST_MSG_CLASS001'
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy5).
    cl_abap_unit_assert=>assert_subrc( exp = 4 msg = |Change log for MSG 001 in PL not expected| ).

    SELECT SINGLE @abap_true FROM lxe_log
    WHERE targlng = @lxe_lang_pl AND objtype = 'MESS' AND objname = 'ZTRAN_TEST_MSG_CLASS002'
        AND uname = @sy-uname AND udate = @sy-datum INTO @DATA(dummy6).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = |Change log for MSG 002 in PL not found| ).
  ENDMETHOD.

  METHOD verify_t100_after_save.
    SELECT * FROM t100 WHERE arbgb = @c_test_msg_class INTO TABLE @DATA(t100_exp).

    t100_exp[ sprsl = c_lang_en arbgb = c_test_msg_class msgnr = '001' ]-text = 'Modified'.
    "Delete/Append in case msg doesn't exists
    DELETE t100_exp WHERE sprsl = c_lang_pl AND arbgb = c_test_msg_class AND msgnr = '002'.
    APPEND VALUE #( sprsl = c_lang_pl arbgb = c_test_msg_class msgnr = '002' text =  'New in another lang' ) TO t100_exp.

    save_modified_in_db( ).

    SELECT * FROM t100 WHERE arbgb = @c_test_msg_class INTO TABLE @DATA(t100_act).

    SORT: t100_exp BY sprsl msgnr, t100_act BY sprsl msgnr.
    cl_abap_unit_assert=>assert_equals( act = t100_act exp = t100_exp ).
  ENDMETHOD.

  METHOD verify_t100u_en_after_save.
    SELECT * FROM t100u WHERE arbgb = @c_test_msg_class INTO TABLE @DATA(t100u_exp).
    t100u_exp[ arbgb = c_test_msg_class msgnr = '001' ]-name = sy-uname.
    t100u_exp[ arbgb = c_test_msg_class msgnr = '001' ]-datum = sy-datum.
    t100u_exp[ arbgb = c_test_msg_class msgnr = '002' ]-name = sy-uname.
    t100u_exp[ arbgb = c_test_msg_class msgnr = '002' ]-datum = sy-datum.

    save_modified_in_db( pl = abap_false ).

    SELECT * FROM t100u WHERE arbgb = @c_test_msg_class INTO TABLE @DATA(t100u_act).
    cl_abap_unit_assert=>assert_equals( act = t100u_act exp = t100u_exp ).
  ENDMETHOD.

  METHOD verify_t100u_pl_after_save.
    SELECT * FROM t100u WHERE arbgb = @c_test_msg_class INTO TABLE @DATA(t100u_exp).

    save_modified_in_db( en = abap_false ).

    SELECT * FROM t100u WHERE arbgb = @c_test_msg_class INTO TABLE @DATA(t100u_act).
    cl_abap_unit_assert=>assert_equals( act = t100u_act exp = t100u_exp ).
  ENDMETHOD.
ENDCLASS.
