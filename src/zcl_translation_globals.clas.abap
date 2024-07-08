CLASS zcl_translation_globals DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      c_version TYPE string VALUE '1.0.0',
      BEGIN OF c_object_type,
        transaction                   TYPE trobjtype  VALUE 'TRAN',
        program                       TYPE trobjtype  VALUE 'PROG',
        class                         TYPE trobjtype  VALUE 'CLAS',
        function_group                TYPE trobjtype  VALUE 'FUGR',
        function_group_include_sap    TYPE trobjtype  VALUE 'FUGS',
        function_group_include_client TYPE trobjtype  VALUE 'FUGX',
        message_class                 TYPE trobjtype  VALUE 'MSAG',
        table                         TYPE trobjtype  VALUE 'TABL',
        data_element                  TYPE trobjtype  VALUE 'DTEL',
        domain                        TYPE trobjtype  VALUE 'DOMA',
      END OF c_object_type,
      BEGIN OF c_subcomponent,
        textpool     TYPE string VALUE 'TEXTPOOL',
        screen_texts TYPE string VALUE 'SCREEN_TEXTS',
        menu_texts   TYPE string VALUE 'MENU_TEXTS',
      END OF c_subcomponent.
ENDCLASS.

CLASS zcl_translation_globals IMPLEMENTATION.
ENDCLASS.
