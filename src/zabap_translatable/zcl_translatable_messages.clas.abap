CLASS zcl_translatable_messages DEFINITION
 PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_translatable.
    METHODS:
      constructor IMPORTING message_class TYPE sobj_name.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      get_text IMPORTING text_id TYPE string RETURNING VALUE(text) TYPE REF TO zif_translatable=>t_text,
      modify_translation IMPORTING sap_lang TYPE syst_langu content TYPE textpooltx
                         CHANGING  translations TYPE zif_translatable=>tt_translation..

    DATA:
      texts        TYPE zif_translatable=>tt_text.
ENDCLASS.



CLASS zcl_translatable_messages IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = message_class.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-message_class.
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    SELECT msgnr, text FROM t100
    WHERE arbgb = @zif_translatable~object_name AND sprsl = @sap_lang
    INTO TABLE @DATA(messages).

    LOOP AT messages REFERENCE INTO DATA(message).
      DATA(program_text) = get_text( CONV #( message->msgnr ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( message->text ) CHANGING translations = program_text->translations ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    texts = me->texts.
  ENDMETHOD.

  METHOD zif_translatable~modify_texts.
    LOOP AT new_texts REFERENCE INTO DATA(new_text).
      DATA(program_text) = get_text( new_text->text_id ).
      LOOP AT new_text->translations REFERENCE INTO DATA(new_translation).
        modify_translation( EXPORTING sap_lang = new_translation->sap_lang content = new_translation->content
                            CHANGING translations = program_text->translations ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~save_modified_texts.
    DATA t100_table TYPE STANDARD TABLE OF t100 WITH EMPTY KEY.

    LOOP AT texts REFERENCE INTO DATA(text).
      LOOP AT text->translations REFERENCE INTO DATA(translation) WHERE sap_lang = sap_lang.
        APPEND VALUE #( arbgb = zif_translatable~object_name msgnr = text->text_id text = translation->content ) TO t100_table.
      ENDLOOP.
    ENDLOOP.

    MODIFY t100 FROM TABLE @t100_table.
  ENDMETHOD.

  METHOD get_text.
    text = REF #( texts[ KEY id_only text_id = |{ CONV msgnr( text_id ) ALPHA = IN }| ] OPTIONAL ).
    IF NOT text IS BOUND.
      INSERT VALUE #( object_type = zif_translatable~object_type object_name = zif_translatable~object_name text_id = text_id ) INTO TABLE texts REFERENCE INTO text.
    ENDIF.
  ENDMETHOD.

  METHOD modify_translation.
    DATA(translation) = REF #( translations[ sap_lang = sap_lang ] OPTIONAL ).
    IF NOT translation IS BOUND.
      INSERT VALUE #( sap_lang = sap_lang ) INTO TABLE translations REFERENCE INTO translation.
    ENDIF.
    translation->content = content.
  ENDMETHOD.


ENDCLASS.
