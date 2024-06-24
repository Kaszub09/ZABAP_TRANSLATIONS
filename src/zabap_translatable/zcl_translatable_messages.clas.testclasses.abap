CONSTANTS:
  c_test_msg_class TYPE sobj_name VALUE 'ZTRAN_TEST_MSG_CLASS',
  c_lang_pl        TYPE sy-langu VALUE 'L',
  c_lang_en        TYPE sy-langu VALUE 'E'.

CLASS ltcl_translatable_messages DEFINITION DEFERRED.
CLASS zcl_translatable_messages DEFINITION LOCAL FRIENDS ltcl_translatable_messages.
CLASS ltcl_translatable_messages DEFINITION FINAL FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-METHODS:
      class_teardown.
    METHODS:
      setup,
      read_text_in_one_language FOR TESTING,
      read_text_in_multiple_lang FOR TESTING,
      mock_texts_read_in_polish,
      modify_texts FOR TESTING,
      save_modified_in_db FOR TESTING.

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
        ( text_id = '001' translations = VALUE #( ( sap_lang = c_lang_en content = 'Modified' )
                                                  ( sap_lang = c_lang_pl content = 'New in another lang' ) ) ) ).
    cut->modify_texts( new_texts ).

    cut->save_modified_texts( c_lang_en ).
    cut->save_modified_texts( c_lang_pl ).

    SELECT * FROM t100 WHERE arbgb = @c_test_msg_class INTO TABLE @DATA(t100_table).

    cl_abap_unit_assert=>assert_table_contains( table = t100_table
        line = VALUE t100( sprsl = c_lang_en arbgb = c_test_msg_class msgnr = '001' text =  'Modified' ) ).
    cl_abap_unit_assert=>assert_table_contains( table = t100_table
        line = VALUE t100( sprsl = c_lang_pl arbgb = c_test_msg_class msgnr = '001' text =  'New in another lang' ) ).
  ENDMETHOD.

ENDCLASS.
