CLASS zcl_translation_globals DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      c_version TYPE string VALUE '0.2.0',
      BEGIN OF c_object_type,
        program        TYPE trobjtype  VALUE 'PROG',
        class          TYPE trobjtype  VALUE 'CLAS',
        function_group TYPE trobjtype  VALUE 'FUGR',
      END OF c_object_type,
      BEGIN OF c_subcomponent,
        textpool     TYPE string  VALUE 'TEXTPOOL',
        screen_texts TYPE string  VALUE 'SCREEN_TEXTS',
      END OF c_subcomponent.
ENDCLASS.

CLASS zcl_translation_globals IMPLEMENTATION.
ENDCLASS.
