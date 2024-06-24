CLASS zcl_translatable_program DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      zif_translatable.
    METHODS:
      constructor IMPORTING program TYPE sobj_name.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      get_text_sub_type IMPORTING text_id TYPE string RETURNING VALUE(sub_type) TYPE string.

    DATA:
        textpool TYPE REF TO zif_translatable_subcomponent.
ENDCLASS.



CLASS zcl_translatable_program IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = program.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-program.
    textpool = zcl_translation_factory=>create_translatable_sub( object_name =  program
        subcomponent = zcl_translation_globals=>c_subcomponent-textpool ).
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    textpool->zif_translatable~read_language( sap_lang ).
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    APPEND LINES OF textpool->zif_translatable~get_all_texts(  ) TO texts.
  ENDMETHOD.

  METHOD get_text_sub_type.
    SPLIT text_id AT '|' INTO sub_type DATA(dummy).
  ENDMETHOD.

  METHOD zif_translatable~modify_texts.
    LOOP AT new_texts REFERENCE INTO DATA(new_text).
      CASE get_text_sub_type( new_text->text_id ).
        WHEN textpool->sub_type.
          textpool->modify_text( new_text->* ).
        WHEN OTHERS."TODO error handling
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~save_modified_texts.
    textpool->zif_translatable~save_modified_texts( sap_lang ).
  ENDMETHOD.
ENDCLASS.
