INTERFACE zif_translatable PUBLIC.
  TYPES:
    BEGIN OF t_translation,
      sap_lang     TYPE sy-langu,
      content TYPE textpool-entry,
    END OF t_translation,
    tt_translation TYPE SORTED TABLE OF t_translation WITH UNIQUE KEY sap_lang,
    BEGIN OF t_text,
      object_type  TYPE trobjtype,
      object_name  TYPE sobj_name,
      text_id      TYPE string,
      translations TYPE tt_translation,
    END OF t_text,
    tt_text TYPE STANDARD TABLE OF t_text WITH EMPTY KEY
        WITH UNIQUE SORTED KEY text_id COMPONENTS object_type object_name text_id
        WITH NON-UNIQUE SORTED KEY id_only COMPONENTS text_id.

  METHODS:
    read_language IMPORTING sap_lang TYPE sy-langu,
    get_all_texts RETURNING VALUE(texts) TYPE tt_text,
    modify_texts IMPORTING new_texts TYPE tt_text,
    save_modified_texts IMPORTING sap_lang TYPE sy-langu.
ENDINTERFACE.
