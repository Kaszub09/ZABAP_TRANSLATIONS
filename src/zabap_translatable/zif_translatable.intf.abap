INTERFACE zif_translatable PUBLIC.
  TYPES:
    BEGIN OF t_translation,
      sap_lang TYPE sy-langu,
      content  TYPE textpool-entry,
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
    "! <p class="shorttext synchronized" lang="en">Existing texts are re-read and synchronized to SAP</p>
    read_language IMPORTING sap_lang TYPE sy-langu,
    "! <p class="shorttext synchronized" lang="en">Returns all texts read and/or modified</p>
    get_all_texts RETURNING VALUE(texts) TYPE tt_text,
    "! @parameter new_texts | <p class="shorttext synchronized" lang="en">Texts with different object type/name are skipped</p>
    modify_texts IMPORTING new_texts TYPE tt_text,
    "! <p class="shorttext synchronized" lang="en">Save read and/or modified texts in database</p>
    save_modified_texts IMPORTING sap_lang TYPE sy-langu.

  DATA:
    object_type TYPE trobjtype READ-ONLY,
    object_name TYPE sobj_name READ-ONLY.
ENDINTERFACE.
