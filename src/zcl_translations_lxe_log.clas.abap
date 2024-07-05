CLASS zcl_translations_lxe_log DEFINITION PUBLIC CREATE PRIVATE GLOBAL FRIENDS zcl_translation_factory.

  PUBLIC SECTION.
    TYPES:
      tt_lxe_log TYPE STANDARD TABLE OF lxe_log WITH EMPTY KEY,
      BEGIN OF t_lxe_language,
        sap      TYPE sy-langu,
        lxe_lang TYPE lxeisolang,
      END OF t_lxe_language,
      tt_lxe_language TYPE SORTED TABLE OF t_lxe_language WITH UNIQUE KEY sap.

    METHODS:
      constructor,
      "! <p class="shorttext synchronized" lang="en">Updates: custmnr, uname, udate, utime, lang (lx_languages)</p>
      update_lxe_log IMPORTING lxe_log_table TYPE tt_lxe_log.

    DATA:
        lxe_languages TYPE tt_lxe_language READ-ONLY.
  PRIVATE SECTION.
    DATA custmnr TYPE lxecustmnr.
ENDCLASS.

CLASS zcl_translations_lxe_log IMPLEMENTATION.
  METHOD constructor.
    SELECT r3_lang AS sap, language AS lxe_lang FROM lxe_t002 WHERE is_r3_lang = @abap_true INTO CORRESPONDING FIELDS OF TABLE @lxe_languages.
    SELECT SINGLE custmnr FROM lxe_custmnr INTO @custmnr.
  ENDMETHOD.

  METHOD update_lxe_log.
    DATA(lxe_log_tab) = lxe_log_table.

    SORT lxe_log_tab BY targlng objtype objname tabkey.
    DELETE ADJACENT DUPLICATES FROM lxe_log_tab COMPARING targlng objtype objname tabkey.

    GET TIME.
    LOOP AT lxe_log_tab REFERENCE INTO DATA(lxe_log_row).
      lxe_log_row->custmnr = custmnr.
      lxe_log_row->uname = sy-uname.
      lxe_log_row->udate = sy-datum.
      lxe_log_row->utime = sy-uzeit.
      IF strlen( lxe_log_row->targlng ) = 1.
        lxe_log_row->targlng = lxe_languages[ sap = lxe_log_row->targlng ]-lxe_lang.
      ENDIF.
    ENDLOOP.

    MODIFY lxe_log FROM TABLE @lxe_log_tab.
  ENDMETHOD.
ENDCLASS.
