CLASS zcl_translation_objects DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING languages TYPE REF TO zcl_translation_languages,
      add_translatable IMPORTING translatable TYPE REF TO zif_translatable,
      get_all_texts RETURNING VALUE(texts) TYPE zif_translatable=>tt_text,
      save_all_translatable.

  PRIVATE SECTION.
    DATA:
      languages     TYPE REF TO zcl_translation_languages,
      translatables TYPE STANDARD TABLE OF REF TO zif_translatable WITH EMPTY KEY.
ENDCLASS.

CLASS zcl_translation_objects IMPLEMENTATION.
  METHOD constructor.
    me->languages = languages.
  ENDMETHOD.

  METHOD add_translatable.
    APPEND translatable TO translatables.
  ENDMETHOD.

  METHOD get_all_texts.
    LOOP AT translatables INTO DATA(translatable).
      LOOP AT languages->languages REFERENCE INTO DATA(lang).
        translatable->read_language( lang->sap ).
      ENDLOOP.
      APPEND LINES OF translatable->get_all_texts( ) TO texts.
    ENDLOOP.
  ENDMETHOD.

  METHOD save_all_translatable.
    LOOP AT translatables INTO DATA(translatable).
      LOOP AT languages->languages REFERENCE INTO DATA(lang).
        translatable->save_modified_texts( lang->sap ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
