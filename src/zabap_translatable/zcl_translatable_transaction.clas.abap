CLASS zcl_translatable_transaction DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_translation_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_translatable.

    METHODS:
      constructor IMPORTING transaction TYPE sobj_name.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS:
      get_text IMPORTING text_id TYPE string RETURNING VALUE(text) TYPE REF TO zif_translatable=>t_text,
      modify_translation IMPORTING sap_lang TYPE syst_langu content TYPE textpooltx
                         CHANGING  translations TYPE zif_translatable=>tt_translation.

    CONSTANTS:
      c_lxe_type  TYPE lxeobjtype VALUE 'TRAN'.

    DATA:
      texts    TYPE zif_translatable=>tt_text.
ENDCLASS.

CLASS zcl_translatable_transaction IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = transaction.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-transaction.
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    SELECT SINGLE ttext
    FROM tstct WHERE tcode = @zif_translatable~object_name AND sprsl = @sap_lang
    INTO @DATA(transaction_text).

    DATA(program_text) = get_text( space ).
    modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( transaction_text ) CHANGING translations = program_text->translations ).
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    texts = me->texts.
  ENDMETHOD.

  METHOD zif_translatable~modify_texts.
    LOOP AT new_texts REFERENCE INTO DATA(new_text) USING KEY text_id
    WHERE object_type = zif_translatable~object_type AND object_name = zif_translatable~object_name.
      DATA(program_text) = get_text( new_text->text_id ).
      LOOP AT new_text->translations REFERENCE INTO DATA(new_translation).
        modify_translation( EXPORTING sap_lang = new_translation->sap_lang content = new_translation->content
                            CHANGING translations = program_text->translations ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~save_modified_texts.
    DATA tstct_table TYPE STANDARD TABLE OF tstct WITH EMPTY KEY.

    LOOP AT texts REFERENCE INTO DATA(text).
      LOOP AT text->translations REFERENCE INTO DATA(translation) WHERE sap_lang = sap_lang.
        APPEND VALUE #( sprsl = sap_lang tcode = zif_translatable~object_name ttext = translation->content ) TO tstct_table.
      ENDLOOP.
    ENDLOOP.

    MODIFY tstct FROM TABLE @tstct_table.

    zcl_translation_factory=>get_lxe_log( )->update_lxe_log( VALUE #( (
        objname = zif_translatable~object_name objtype = c_lxe_type targlng = sap_lang ) ) ).
  ENDMETHOD.

  METHOD get_text.
    text = REF #( texts[ KEY id_only text_id = text_id ] OPTIONAL ).
    IF NOT text IS BOUND.
      INSERT VALUE #( object_type = zif_translatable~object_type object_name = zif_translatable~object_name
        text_id = text_id ) INTO TABLE texts REFERENCE INTO text.
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
