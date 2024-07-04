*&---------------------------------------------------------------------*
*& Report zabap_translations_test_prog
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_translations_test_prog.


parameters: p_no_ref type i.
select-options s_no_ref for p_no_ref.

tables: tadir.
parameters: p_ref type tadir-object.
select-options s_ref for tadir-object.
