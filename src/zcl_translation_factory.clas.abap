CLASS zcl_translation_factory DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      create_translatable IMPORTING object_type TYPE trobjtype object_name TYPE sobj_name RETURNING VALUE(translatable) TYPE REF TO zif_translatable,
      create_translatable_sub IMPORTING subcomponent TYPE string object_name TYPE sobj_name
                              RETURNING VALUE(translatable) TYPE REF TO zif_translatable_subcomponent.
ENDCLASS.

CLASS zcl_translation_factory IMPLEMENTATION.
  METHOD create_translatable.
    CASE object_type.
      WHEN zcl_translation_globals=>c_object_type-program.
        translatable = NEW zcl_translatable_program( object_name ).
      WHEN OTHERS. "TODO error handling
    ENDCASE.
  ENDMETHOD.

  METHOD create_translatable_sub.
    CASE subcomponent.
      WHEN zcl_translation_globals=>c_subcomponent-textpool.
        translatable = NEW zcl_translatable_textpool( object_name ).
      WHEN OTHERS. "TODO error handling
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
