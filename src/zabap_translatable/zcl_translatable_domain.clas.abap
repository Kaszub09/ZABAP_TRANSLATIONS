CLASS zcl_translatable_domain DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_translation_factory.

  PUBLIC SECTION.
    INTERFACES:
      zif_translatable.

    METHODS:
      constructor IMPORTING domain TYPE sobj_name.

  PRIVATE SECTION.
    TYPES:
      t_table TYPE c LENGTH 5,
      BEGIN OF  t_text_id_parsed,
        tab      TYPE t_table,
        valpos   TYPE valpos,
        as4local TYPE as4local,
        as4vers  TYPE as4vers,
      END OF  t_text_id_parsed,
      tt_dd07t TYPE SORTED TABLE OF dd07t WITH UNIQUE KEY domname ddlanguage as4local valpos as4vers.

    METHODS:
      get_text_id IMPORTING parsed TYPE  t_text_id_parsed RETURNING VALUE(text_id) TYPE string,
      parse_text_id IMPORTING text_id TYPE string RETURNING VALUE(parsed) TYPE t_text_id_parsed,
      get_text IMPORTING text_id TYPE string RETURNING VALUE(text) TYPE REF TO zif_translatable=>t_text,
      modify_translation IMPORTING sap_lang TYPE syst_langu content TYPE textpooltx
                         CHANGING translations TYPE zif_translatable=>tt_translation,
      update_dd07t IMPORTING dd07t TYPE tt_dd07t.

    CONSTANTS:
      BEGIN OF c_lxe_type,
        descriptions TYPE lxeobjtype VALUE 'DOMA',
        constants    TYPE lxeobjtype VALUE 'VALU',
      END OF c_lxe_type,
      BEGIN OF c_table,
        dd01t TYPE t_table VALUE 'DD01T',
        dd07t TYPE t_table VALUE 'DD07T',
      END OF c_table.

    DATA:
      texts       TYPE zif_translatable=>tt_text.

ENDCLASS.

CLASS zcl_translatable_domain IMPLEMENTATION.
  METHOD constructor.
    zif_translatable~object_name = domain.
    zif_translatable~object_type = zcl_translation_globals=>c_object_type-domain.
  ENDMETHOD.

  METHOD zif_translatable~read_language.
    DATA(valpos_empty) = VALUE valpos( ).

    SELECT @c_table-dd01t AS tab, @valpos_empty AS valpos, as4local, as4vers, ddtext
    FROM dd01t WHERE domname = @zif_translatable~object_name AND ddlanguage = @sap_lang
    UNION
    SELECT @c_table-dd07t AS tab, valpos, as4local, as4vers, ddtext
    FROM dd07t WHERE domname = @zif_translatable~object_name AND ddlanguage = @sap_lang
    INTO TABLE @DATA(table_texts).

    LOOP AT table_texts REFERENCE INTO DATA(text).
      DATA(program_text) = get_text( get_text_id( VALUE #( tab = text->tab valpos = text->valpos as4local = text->as4local as4vers = text->as4vers ) ) ).
      modify_translation( EXPORTING sap_lang = sap_lang content = CONV #( text->ddtext ) CHANGING translations = program_text->translations ).
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_translatable~get_all_texts.
    texts = me->texts.
  ENDMETHOD.

  METHOD get_text_id.
    text_id = |{ parsed-tab }\|{ parsed-valpos }\|{ parsed-as4local }\|{ parsed-as4vers }|.
  ENDMETHOD.

  METHOD parse_text_id.
    SPLIT text_id AT '|' INTO parsed-tab parsed-valpos parsed-as4local parsed-as4vers.
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
    DATA dd01t_table TYPE STANDARD TABLE OF dd01t WITH EMPTY KEY.
    DATA dd07t_table TYPE tt_dd07t.

    LOOP AT texts REFERENCE INTO DATA(text).
      DATA(parsed) = parse_text_id( text->text_id ).
      LOOP AT text->translations REFERENCE INTO DATA(translation) WHERE sap_lang = sap_lang.
        CASE parsed-tab.
          WHEN c_table-dd01t.
            INSERT VALUE #( BASE CORRESPONDING dd01t( parsed ) domname = zif_translatable~object_name
                            ddlanguage = sap_lang ddtext  = translation->content ) INTO TABLE dd01t_table.
            APPEND VALUE #( objname = zif_translatable~object_name objtype = c_lxe_type-descriptions targlng = sap_lang ) TO lxe_log_table.

          WHEN c_table-dd07t.
            INSERT VALUE #( BASE CORRESPONDING dd07t( parsed ) domname = zif_translatable~object_name
                           ddlanguage = sap_lang ddtext = translation->content ) INTO TABLE dd07t_table.
            APPEND VALUE #( objname = zif_translatable~object_name objtype = c_lxe_type-constants targlng = sap_lang ) TO lxe_log_table.

        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    MODIFY dd01t FROM TABLE @dd01t_table.
    update_dd07t( dd07t_table ).

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

  METHOD update_dd07t.
    IF lines( dd07t ) = 0.
      RETURN.
    ENDIF.

    "Update only texts, leave DOMVAL... as is
    DATA dd07t_db TYPE tt_dd07t.
    SELECT * FROM dd07t
    FOR ALL ENTRIES IN @dd07t
    WHERE dd07t~domname = @dd07t-domname AND dd07t~as4local = @dd07t-as4local AND dd07t~valpos = @dd07t-valpos AND dd07t~as4vers = @dd07t-as4vers
    INTO CORRESPONDING FIELDS OF TABLE @dd07t_db.

    DATA dd07t_to_update TYPE STANDARD TABLE OF dd07t WITH EMPTY KEY.
    LOOP AT dd07t REFERENCE INTO DATA(dd07t_row).
      DATA(dd07t_to_append) = dd07t_row->*.

      LOOP AT dd07t_db REFERENCE INTO DATA(dd04t_db_row)
      WHERE domname = dd07t_row->domname AND as4local = dd07t_row->as4local
      AND valpos = dd07t_row->valpos AND as4vers = dd07t_row->as4vers.
        dd07t_to_append = VALUE #( BASE CORRESPONDING #( dd04t_db_row->* ) ddlanguage = dd07t_row->ddlanguage ddtext = dd07t_row->ddtext ).
      ENDLOOP.

      APPEND dd07t_to_append TO dd07t_to_update.
    ENDLOOP.

    MODIFY dd07t FROM TABLE @dd07t_to_update.
  ENDMETHOD.
ENDCLASS.
