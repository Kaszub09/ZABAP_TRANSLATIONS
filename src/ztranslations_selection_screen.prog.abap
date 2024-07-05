*&---------------------------------------------------------------------*
*&  Include  ztranslations_selection_screen
*&---------------------------------------------------------------------*
TABLES:
  tadir.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
SELECT-OPTIONS: s_lang FOR sy-langu.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE TEXT-s04.
SELECT-OPTIONS:
s_packag FOR tadir-devclass.

SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS:
s_transa FOR tadir-obj_name,
s_prog FOR tadir-obj_name,
s_class FOR tadir-obj_name,
s_fungr FOR tadir-obj_name,
s_msgcls FOR tadir-obj_name.

SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS:
s_table FOR tadir-obj_name,
s_datael FOR tadir-obj_name,
s_domain FOR tadir-obj_name.
SELECTION-SCREEN END OF BLOCK b04.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-s02.
PARAMETERS:
  p_export RADIOBUTTON GROUP g1,
  p_import RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-s03.
PARAMETERS: p_file TYPE string.
SELECTION-SCREEN END OF BLOCK b03.
