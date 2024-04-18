*&---------------------------------------------------------------------*
*& 包含               ZVARIANT_COMPARE_CLASS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& 类声明
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION .
  PUBLIC SECTION.

    METHODS:
      handle_top_of_page_left FOR EVENT top_of_page  OF cl_gui_alv_grid
        IMPORTING e_dyndoc_id,
      handle_top_of_page_right FOR EVENT top_of_page  OF cl_gui_alv_grid
        IMPORTING e_dyndoc_id.

ENDCLASS .
*&---------------------------------------------------------------------*
*& 类实现
*&---------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_top_of_page_left.
    PERFORM frm_top_of_page_left USING e_dyndoc_id.
  ENDMETHOD.                    "handle_top_of_page
  METHOD handle_top_of_page_right.
    PERFORM frm_top_of_page_right USING e_dyndoc_id.
  ENDMETHOD.                    "handle_top_of_page
ENDCLASS .
FORM frm_top_of_page_left USING p_eref_doc TYPE REF TO cl_dd_document.
  DATA: lv_back TYPE sdydo_key VALUE space.
  DATA: lv_html TYPE string,
        m_p     TYPE i.
  DATA: l_line   TYPE i,
        l_line_s TYPE i,
        l_line_e TYPE i.
  lv_html = |<html><p>开始时间:{ wa_secds1-lv_start }<br>结束时间:{ wa_secds1-lv_end }<br>取数时长:{ wa_secds1-secds }秒</p>|.
  p_eref_doc->html_insert(
    EXPORTING
      contents = lv_html
    CHANGING
      position = m_p
  ).
  CHECK alv_grid IS NOT INITIAL.
  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      document = gref_doc_left
      bottom   = space.
  CALL METHOD gref_doc_left->merge_document.
  CALL METHOD gref_doc_left->set_document_background
    EXPORTING
      picture_id = lv_back.
  gref_doc_left->html_control = gref_viewer_left.
  CALL METHOD gref_doc_left->display_document
    EXPORTING
      reuse_control = 'X'
      parent        = ref_container_left_top.
ENDFORM.

FORM frm_top_of_page_right USING p_eref_doc TYPE REF TO cl_dd_document.
  DATA: lv_back TYPE sdydo_key VALUE space.
  DATA: lv_html TYPE string,
        m_p     TYPE i.
  DATA: l_line   TYPE i,
        l_line_s TYPE i,
        l_line_e TYPE i.
  lv_html = |<html><p>开始时间:{ wa_secds2-lv_start }<br>结束时间:{ wa_secds2-lv_end }<br>取数时长:{ wa_secds2-secds }秒</p>|.
  p_eref_doc->html_insert(
    EXPORTING
      contents = lv_html
    CHANGING
      position = m_p
  ).
  CHECK alv_grid IS NOT INITIAL.
  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'
    EXPORTING
      document = gref_doc_right
      bottom   = space.
  CALL METHOD gref_doc_right->merge_document.
  CALL METHOD gref_doc_right->set_document_background
    EXPORTING
      picture_id = lv_back.
  gref_doc_right->html_control = gref_viewer_right.
  CALL METHOD gref_doc_right->display_document
    EXPORTING
      reuse_control = 'X'
      parent        = ref_container_right_top.
ENDFORM.

DATA event_handler  TYPE REF TO lcl_event_receiver.
