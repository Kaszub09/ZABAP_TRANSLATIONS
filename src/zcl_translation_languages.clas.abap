CLASS zcl_translation_languages DEFINITION PUBLIC FINAL CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      tt_lang_range TYPE RANGE OF sy-langu,
      BEGIN OF t_language,
        sap TYPE sy-langu,
        iso TYPE laiso,
      END OF t_language,
      tt_language TYPE SORTED TABLE OF t_language WITH UNIQUE KEY sap
        WITH UNIQUE SORTED KEY iso COMPONENTS iso.

    METHODS:
      constructor IMPORTING lang_range TYPE tt_lang_range.

    DATA:
        languages TYPE tt_language READ-ONLY.

ENDCLASS.

CLASS zcl_translation_languages IMPLEMENTATION.
  METHOD constructor.
    SELECT spras AS sap, laiso AS iso FROM t002 INTO CORRESPONDING FIELDS OF TABLE @languages WHERE spras IN @lang_range.
  ENDMETHOD.
ENDCLASS.
