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
      add_subcomponent IMPORTING sub_type TYPE string.

    DATA:
      subcomponents TYPE STANDARD TABLE OF REF TO zif_translatable.
ENDCLASS.

CLASS zcl_translatable_program IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = program.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-program.
    add_subcomponent( zcl_translation_globals=>c_subcomponent-textpool ).
    add_subcomponent( zcl_translation_globals=>c_subcomponent-screen_texts ).
    add_subcomponent( zcl_translation_globals=>c_subcomponent-menu_texts ).
  ENDMETHOD.

  METHOD add_subcomponent.
    DATA(subcomponent) = zcl_translation_factory=>create_translatable_sub( object_name = zif_translatable~object_name sub_type = sub_type ).
    APPEND subcomponent TO subcomponents.
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    LOOP AT subcomponents INTO DATA(subcomponent).
      subcomponent->read_language( sap_lang ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    LOOP AT subcomponents INTO DATA(subcomponent).
      APPEND LINES OF subcomponent->get_all_texts( ) TO texts.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~modify_texts.
    LOOP AT subcomponents INTO DATA(subcomponent).
      subcomponent->modify_texts( new_texts ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~save_modified_texts.
    LOOP AT subcomponents INTO DATA(subcomponent).
      subcomponent->save_modified_texts( sap_lang ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
