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
    TYPES:
      BEGIN OF t_subcomponent,
        sub_type TYPE string,
        object   TYPE REF TO zif_translatable_subcomponent,
      END OF t_subcomponent,
      tt_subcomponent TYPE HASHED TABLE OF t_subcomponent WITH UNIQUE KEY sub_type.

    METHODS:
      add_subcomponent IMPORTING sub_type TYPE string,
      get_text_sub_type IMPORTING text_id TYPE string RETURNING VALUE(sub_type) TYPE string.

    DATA:
      subcomponents TYPE tt_subcomponent.
ENDCLASS.

CLASS zcl_translatable_program IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = program.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-program.
    add_subcomponent( zcl_translation_globals=>c_subcomponent-textpool ).
    add_subcomponent( zcl_translation_globals=>c_subcomponent-screen_texts ).
  ENDMETHOD.

  METHOD add_subcomponent.
    DATA(subcomponent) = zcl_translation_factory=>create_translatable_sub( object_name = zif_translatable~object_name sub_type = sub_type ).
    INSERT VALUE #( sub_type = subcomponent->sub_type object = subcomponent  ) INTO TABLE subcomponents.
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    LOOP AT subcomponents REFERENCE INTO DATA(subcomponent).
      subcomponent->object->zif_translatable~read_language( sap_lang ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    LOOP AT subcomponents REFERENCE INTO DATA(subcomponent).
      APPEND LINES OF subcomponent->object->zif_translatable~get_all_texts( ) TO texts.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_text_sub_type.
    SPLIT text_id AT '|' INTO sub_type DATA(dummy).
  ENDMETHOD.

  METHOD zif_translatable~modify_texts.
    LOOP AT new_texts REFERENCE INTO DATA(new_text).
      DATA(subcomponent) = REF #( subcomponents[ sub_type = get_text_sub_type( new_text->text_id ) ] OPTIONAL ).
      IF NOT subcomponent IS BOUND.
        "TODO erorr handling
      ENDIF.
      subcomponent->object->modify_text( new_text->* ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~save_modified_texts.
    LOOP AT subcomponents REFERENCE INTO DATA(subcomponent).
      subcomponent->object->zif_translatable~save_modified_texts( sap_lang ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
