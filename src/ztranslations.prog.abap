*&---------------------------------------------------------------------*
*& Report ztranslations
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztranslations.

TABLES:
  tadir.

"---------------------------------------------------------------------
" SELECTION SCREEN
"---------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-s01.
SELECT-OPTIONS: s_lang FOR sy-langu.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE TEXT-s04.
SELECT-OPTIONS:
s_packag FOR tadir-devclass,
s_prog FOR tadir-obj_name.
SELECTION-SCREEN END OF BLOCK b04.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-s02.
PARAMETERS:
  p_export RADIOBUTTON GROUP g1,
  p_import RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-s03.
PARAMETERS: p_file TYPE string.
SELECTION-SCREEN END OF BLOCK b03.

"---------------------------------------------------------------------
" REPORT
"---------------------------------------------------------------------

START-OF-SELECTION.
  DATA(languages) = NEW zcl_translation_languages( s_lang[] ).

  IF p_export = abap_true.
    PERFORM export.
  ELSEIF p_import = abap_true.
    PERFORM import.
  ENDIF.

FORM export.
  SELECT FROM tadir
  FIELDS object  AS object_type, obj_name AS object_name
  WHERE devclass IN @s_packag
    AND ( object = @zcl_translation_globals=>c_object_type-program AND obj_name IN @s_prog )
  INTO TABLE @DATA(objects).

  DATA(translation_objects) = NEW zcl_translation_objects( languages ).
  LOOP AT objects REFERENCE INTO DATA(object).
    DATA(translatable) = zcl_translation_factory=>create_translatable( object_type = object->object_type object_name = object->object_name ).
    translation_objects->add_translatable( translatable ).
  ENDLOOP.

  DATA(file) = NEW zcl_translation_file( languages ).
  DATA(all_texts) = translation_objects->get_all_texts( ).
  file->export_to_excel( texts = all_texts full_file_path = p_file ).
ENDFORM.

FORM import.
  DATA(file) = NEW zcl_translation_file( languages ).
  DATA(all_texts) = file->import_from_excel( p_file ).

  DATA(translation_objects) = NEW zcl_translation_objects( languages ).

  LOOP AT all_texts REFERENCE INTO DATA(single_text)
  GROUP BY ( object_type = single_text->object_type object_name = single_text->object_name ) REFERENCE INTO DATA(grouped).
    DATA(translatable) = zcl_translation_factory=>create_translatable( object_type = grouped->object_type object_name = grouped->object_name ).
    translatable->modify_texts( FILTER #( all_texts USING KEY text_id
        WHERE object_type = grouped->object_type AND object_name = grouped->object_name ) ).
    translation_objects->add_translatable( translatable ).
  ENDLOOP.

  translation_objects->save_all_translatable( ).
ENDFORM.
