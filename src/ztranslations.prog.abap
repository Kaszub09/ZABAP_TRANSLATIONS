*&---------------------------------------------------------------------*
*& Report ztranslations
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztranslations.
INCLUDE ztranslations_selection_screen.

START-OF-SELECTION.
  DATA(languages) = NEW zcl_translation_languages( s_lang[] ).
  TRY.
      IF p_export = abap_true.
        PERFORM export.
        MESSAGE |Successfuly exported| TYPE 'S'.

      ELSEIF p_import = abap_true.
        PERFORM import.
        MESSAGE |Successfuly imported| TYPE 'S'.

      ENDIF.

    CATCH zcx_translation INTO DATA(zcx).
      MESSAGE zcx->get_text( ) TYPE 'E'.
  ENDTRY.

FORM export RAISING zcx_translation.
  SELECT FROM tadir
  FIELDS object AS object_type, obj_name AS object_name
  WHERE devclass IN @s_packag
    "Restrict object types in case user only selected package
    AND object IN ( @zcl_translation_globals=>c_object_type-transaction, @zcl_translation_globals=>c_object_type-program,
                    @zcl_translation_globals=>c_object_type-class, @zcl_translation_globals=>c_object_type-function_group,
                    @zcl_translation_globals=>c_object_type-message_class, @zcl_translation_globals=>c_object_type-table,
                    @zcl_translation_globals=>c_object_type-data_element, @zcl_translation_globals=>c_object_type-domain )
    "Make possible selecting different objects in one query, and select them only if user set some condition
    AND ( ( object = @zcl_translation_globals=>c_object_type-transaction AND obj_name IN @s_transa AND @( lines( s_transa[] ) ) > 0 )
       OR ( object = @zcl_translation_globals=>c_object_type-program AND obj_name IN @s_prog AND @( lines( s_prog[] ) ) > 0 )
       OR ( object = @zcl_translation_globals=>c_object_type-class AND obj_name IN @s_class AND @( lines( s_class[] ) ) > 0 )
       OR ( object = @zcl_translation_globals=>c_object_type-function_group AND obj_name IN @s_fungr AND @( lines( s_fungr[] ) ) > 0 )
       OR ( object = @zcl_translation_globals=>c_object_type-message_class AND obj_name IN @s_msgcls AND @( lines( s_msgcls[] ) ) > 0 )
       OR ( object = @zcl_translation_globals=>c_object_type-table AND obj_name IN @s_table AND @( lines( s_table[] ) ) > 0 )
       OR ( object = @zcl_translation_globals=>c_object_type-data_element AND obj_name IN @s_datael AND @( lines( s_datael[] ) ) > 0 )
       OR ( object = @zcl_translation_globals=>c_object_type-domain AND obj_name IN @s_domain AND @( lines( s_domain[] ) ) > 0 )
       OR @( lines( s_packag[] ) ) > 0 )
    AND delflag = @abap_false
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

FORM import RAISING zcx_translation.
  DATA(file) = NEW zcl_translation_file( languages ).
  DATA(all_texts) = file->import_from_excel( p_file ).

  DATA(translation_objects) = NEW zcl_translation_objects( languages ).

  LOOP AT all_texts REFERENCE INTO DATA(single_text)
  GROUP BY ( object_type = single_text->object_type object_name = single_text->object_name ) REFERENCE INTO DATA(grouped).
    DATA(translatable) = zcl_translation_factory=>create_translatable( object_type = grouped->object_type object_name = grouped->object_name ).
    translatable->modify_texts( all_texts ).
    translation_objects->add_translatable( translatable ).
  ENDLOOP.

  translation_objects->save_all_translatable( ).
ENDFORM.
