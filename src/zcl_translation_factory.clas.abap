CLASS zcl_translation_factory DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      create_translatable IMPORTING object_type TYPE trobjtype object_name TYPE sobj_name
                          RETURNING VALUE(translatable) TYPE REF TO zif_translatable
                          RAISING zcx_translation,
      create_translatable_sub IMPORTING sub_type TYPE string object_name TYPE sobj_name
                              RETURNING VALUE(translatable) TYPE REF TO zif_translatable
                              RAISING zcx_translation,
      get_lxe_log RETURNING VALUE(lxe_log) TYPE REF TO zcl_translations_lxe_log.

  PRIVATE SECTION.
    CLASS-DATA:
      lxe_log_instance TYPE REF TO zcl_translations_lxe_log.
ENDCLASS.

CLASS zcl_translation_factory IMPLEMENTATION.
  METHOD create_translatable.
    CASE object_type.
      WHEN zcl_translation_globals=>c_object_type-transaction.
        translatable = NEW zcl_translatable_transaction( object_name ).
      WHEN zcl_translation_globals=>c_object_type-program.
        translatable = NEW zcl_translatable_program( object_name ).
      WHEN zcl_translation_globals=>c_object_type-class.
        translatable = NEW zcl_translatable_class( object_name ).
      WHEN zcl_translation_globals=>c_object_type-function_group OR zcl_translation_globals=>c_object_type-function_group_include_sap
      OR zcl_translation_globals=>c_object_type-function_group_include_client.
        translatable = NEW zcl_translatable_function_gr( object_name ).
      WHEN zcl_translation_globals=>c_object_type-message_class.
        translatable = NEW zcl_translatable_messages( object_name ).
      WHEN zcl_translation_globals=>c_object_type-table.
        translatable = NEW zcl_translatable_table( object_name ).
      WHEN zcl_translation_globals=>c_object_type-data_element.
        translatable = NEW zcl_translatable_data_element( object_name ).
      WHEN zcl_translation_globals=>c_object_type-domain.
        translatable = NEW zcl_translatable_domain( object_name ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_translation EXPORTING custom_message = |Object type { object_type } not supported|.
    ENDCASE.
  ENDMETHOD.

  METHOD create_translatable_sub.
    CASE sub_type.
      WHEN zcl_translation_globals=>c_subcomponent-textpool.
        translatable = NEW zcl_translatable_textpool( object_name ).
      WHEN zcl_translation_globals=>c_subcomponent-screen_texts.
        translatable = NEW zcl_translatable_screen_el( object_name ).
      WHEN zcl_translation_globals=>c_subcomponent-menu_texts.
        translatable = NEW zcl_translatable_menu_el( object_name ).
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_translation EXPORTING custom_message = |Sub type { sub_type } not supported|.
    ENDCASE.
  ENDMETHOD.

  METHOD get_lxe_log.
    IF NOT lxe_log_instance IS BOUND.
      lxe_log_instance = NEW #( ).
    ENDIF.
    lxe_log = lxe_log_instance.
  ENDMETHOD.

ENDCLASS.
