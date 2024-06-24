CLASS zcl_translation_globals DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      c_version TYPE string VALUE '0.1.0',
      BEGIN OF c_object_type,
        program TYPE trobjtype  VALUE 'PROG',
        class   TYPE trobjtype  VALUE 'CLAS',
      END OF c_object_type.
ENDCLASS.

CLASS zcl_translation_globals IMPLEMENTATION.
ENDCLASS.
