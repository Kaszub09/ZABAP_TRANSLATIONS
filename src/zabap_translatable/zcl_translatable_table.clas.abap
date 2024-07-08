CLASS zcl_translatable_table DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_translation_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_translatable.

    METHODS:
      constructor IMPORTING table TYPE sobj_name.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      t_table TYPE c LENGTH 5,
      BEGIN OF  t_text_id_parsed,
        tab       TYPE t_table,
        fieldname TYPE fieldname,
        as4local  TYPE as4local,
        as4vers   TYPE as4vers,
      END OF  t_text_id_parsed.

    METHODS:
      get_text_id IMPORTING parsed TYPE  t_text_id_parsed RETURNING VALUE(text_id) TYPE string,
      parse_text_id IMPORTING text_id TYPE string RETURNING VALUE(parsed) TYPE t_text_id_parsed,
      get_text IMPORTING text_id TYPE string RETURNING VALUE(text) TYPE REF TO zif_translatable=>t_text,
      modify_translation IMPORTING sap_lang TYPE syst_langu content TYPE textpooltx
                         CHANGING  translations TYPE zif_translatable=>tt_translation.

    CONSTANTS:
      BEGIN OF c_lxe_type,
        descriptions TYPE lxeobjtype VALUE 'TABT',
        keys         TYPE lxeobjtype VALUE 'BEZD',
      END OF c_lxe_type,
      BEGIN OF c_table,
        dd02t TYPE t_table VALUE 'DD02T',
        dd03t TYPE t_table VALUE 'DD03T',
        dd08t TYPE t_table VALUE 'DD08T',
      END OF c_table.

    DATA:
      texts    TYPE zif_translatable=>tt_text.
ENDCLASS.

CLASS zcl_translatable_table IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = table.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-table.
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    DATA(empty_as4vers) = VALUE as4vers( ).
    SELECT @c_table-dd08t AS tab, fieldname, as4local, as4vers, ddtext
    FROM dd08t WHERE tabname = @zif_translatable~object_name AND ddlanguage = @sap_lang
    UNION
    SELECT @c_table-dd02t AS tab,@space AS fieldname, as4local, as4vers, ddtext
    FROM dd02t WHERE tabname = @zif_translatable~object_name AND ddlanguage = @sap_lang
    UNION
    SELECT @c_table-dd03t AS tab, fieldname, as4local, @empty_as4vers AS as4vers, ddtext
    FROM dd03t WHERE tabname = @zif_translatable~object_name AND ddlanguage = @sap_lang
    INTO TABLE @DATA(table_texts).

    LOOP AT table_texts REFERENCE INTO DATA(text).
      DATA(program_text) = get_text( get_text_id( VALUE #( tab = text->tab fieldname = text->fieldname
                                                           as4local = text->as4local as4vers = text->as4vers ) ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( text->ddtext ) CHANGING translations = program_text->translations ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    texts = me->texts.
  ENDMETHOD.

  METHOD get_text_id.
    text_id = |{ parsed-tab }\|{ parsed-fieldname }\|{ parsed-as4local }\|{ parsed-as4vers }|.
  ENDMETHOD.

  METHOD parse_text_id.
    SPLIT text_id AT '|' INTO parsed-tab parsed-fieldname parsed-as4local parsed-as4vers.
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
    DATA(lxe_log) = zcl_translation_factory=>get_lxe_log( ).
    DATA lxe_log_table TYPE lxe_log->tt_lxe_log.

    DATA dd02t_table TYPE STANDARD TABLE OF dd02t WITH EMPTY KEY.
    DATA dd03t_table TYPE STANDARD TABLE OF dd03t WITH EMPTY KEY.
    DATA dd08t_table TYPE STANDARD TABLE OF dd08t WITH EMPTY KEY.

    LOOP AT texts REFERENCE INTO DATA(text).
      DATA(parsed) = parse_text_id( text->text_id ).
      LOOP AT text->translations REFERENCE INTO DATA(translation) WHERE sap_lang = sap_lang.
        CASE parsed-tab.
          WHEN c_table-dd02t.
            APPEND VALUE #( BASE CORRESPONDING dd02t( parsed ) tabname = zif_translatable~object_name
                            ddlanguage = sap_lang ddtext  = translation->content ) TO dd02t_table.
            APPEND VALUE #( objname = zif_translatable~object_name objtype = c_lxe_type-descriptions targlng = sap_lang ) TO lxe_log_table.
          WHEN c_table-dd03t.
            APPEND VALUE #( BASE CORRESPONDING dd03t( parsed ) tabname = zif_translatable~object_name
                            ddlanguage = sap_lang ddtext  = translation->content ) TO dd03t_table.
            APPEND VALUE #( objname = zif_translatable~object_name objtype = c_lxe_type-descriptions targlng = sap_lang ) TO lxe_log_table.
          WHEN c_table-dd08t.
            APPEND VALUE #( BASE CORRESPONDING dd08t( parsed ) tabname = zif_translatable~object_name
                            ddlanguage = sap_lang ddtext  = translation->content ) TO dd08t_table.
            APPEND VALUE #( objname = zif_translatable~object_name objtype = c_lxe_type-keys targlng = sap_lang ) TO lxe_log_table.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    MODIFY dd02t FROM TABLE @dd02t_table.
    MODIFY dd03t FROM TABLE @dd03t_table.
    MODIFY dd08t FROM TABLE @dd08t_table.

    lxe_log->update_lxe_log( lxe_log_table ).
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
