*----------------------------------------------------------------------*
***INCLUDE ZVARIANT_COMPARE_STATUS_090O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0900 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0900 OUTPUT.
  SET PF-STATUS 'STA900'.
  SET TITLEBAR 'TIT900' WITH ''.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SHOWALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE showalv OUTPUT.
  IF alv_container IS INITIAL.
    alv_container = NEW #( repid = sy-repid dynnr = sy-dynnr side = cl_gui_docking_container=>dock_at_top extension = 500 ).
    alv_splitter_container = NEW #( parent = alv_container rows = 2 columns = 2 ).
    alv_splitter_container->set_row_height( id = 1 height = 10 ).
     ref_container_left_top = alv_splitter_container->get_container( row = 1 column = 1 ).
     ref_container_left = alv_splitter_container->get_container( row = 2 column = 1 ).
    alv_grid = NEW #( i_parent =  ref_container_left ).
     ref_container_right_top = alv_splitter_container->get_container( row = 1 column = 2 ).
     ref_container_right = alv_splitter_container->get_container( row = 2 column = 2 ).
    alv_grid_opt = NEW #( i_parent =  ref_container_right ).

    PERFORM:callalv,callalv_opt.
    IF ret2[] IS NOT INITIAL.
      PERFORM showmsg(zpubform) TABLES ret2.
      MESSAGE s000(oo) WITH '数据不一致' DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
      MESSAGE s000(oo) WITH '数据一致'.
    ENDIF.
  ELSE.
*    PERFORM:frm_refresh_alv,frm_refresh_alv_opt.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form callalv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM callalv .
  CHECK <tab_alv> IS ASSIGNED.
  CREATE OBJECT event_handler.
  SET HANDLER event_handler->handle_top_of_page_left
            FOR alv_grid.
  CREATE OBJECT gref_doc_left.
  CALL METHOD gref_doc_left->initialize_document.

  CALL METHOD alv_grid->list_processing_events
    EXPORTING
      i_event_name = 'TOP_OF_PAGE'
      i_dyndoc_id  = gref_doc_left.
  PERFORM callalv_oo
  TABLES <tab_alv> USING alv_grid gt_fldct 'P1' gs_slayt.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form callalv_opt
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM callalv_opt .
  CHECK <tab_opt_alv> IS ASSIGNED.
  CREATE OBJECT event_handler.
  SET HANDLER event_handler->handle_top_of_page_right
            FOR alv_grid_opt.
  CREATE OBJECT gref_doc_right.
  CALL METHOD gref_doc_right->initialize_document.

  CALL METHOD alv_grid_opt->list_processing_events
    EXPORTING
      i_event_name = 'TOP_OF_PAGE'
      i_dyndoc_id  = gref_doc_right.
  PERFORM callalv_oo
  TABLES <tab_opt_alv> USING alv_grid_opt gt_fldct_opt 'P2' gs_slayt_opt.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_refresh_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_refresh_alv .
  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl,
        ls_stable  TYPE lvc_s_stbl.
  ls_stable-row = 'X'.  "固定行
  ls_stable-col = 'X'.  "固定列

  CHECK alv_grid IS NOT INITIAL.

  CALL METHOD alv_grid->refresh_table_display
    EXPORTING
      is_stable = ls_stable
*     I_SOFT_REFRESH = 'X'
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CALL METHOD cl_gui_cfw=>flush.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_refresh_alv_opt
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_refresh_alv_opt .
  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl,
        ls_stable  TYPE lvc_s_stbl.

  ls_stable-row = 'X'.  "固定行
  ls_stable-col = 'X'.  "固定列

  CHECK alv_grid_opt IS NOT INITIAL.

  CALL METHOD alv_grid_opt->refresh_table_display
    EXPORTING
      is_stable = ls_stable
*     I_SOFT_REFRESH = 'X'
    EXCEPTIONS
      finished  = 1
      OTHERS    = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CALL METHOD cl_gui_cfw=>flush.
ENDFORM.


FORM callalv_oo TABLES intab
USING p_alvgrid TYPE REF TO cl_gui_alv_grid
      p_fieldcat TYPE lvc_t_fcat
      p_handle
      p_layoutc TYPE lvc_s_layo
  .
  DATA: it_ef1c   TYPE ui_functions,
        variantc  TYPE disvariant,
        it_filter TYPE lvc_t_filt,
        wa_filter TYPE lvc_s_filt,
        it_save   TYPE char01,
        wa_fieldc TYPE lvc_s_fcat.
  CLEAR: it_ef1c[],variantc,wa_fieldc,
  it_filter,wa_filter.
*LAYOUT
  p_layoutc-zebra      = 'X'.
*布局
  variantc-report = sy-repid.
  variantc-handle = p_handle.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_ef1c.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_ef1c.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_ef1c.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_ef1c.
  APPEND cl_gui_alv_grid=>mc_fc_loc_move_row      TO it_ef1c.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_ef1c.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_ef1c.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_ef1c.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_ef1c.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_ef1c.
  APPEND cl_gui_alv_grid=>mc_fc_refresh          TO it_ef1c.

*ALV展示
  CALL METHOD p_alvgrid->set_table_for_first_display
    EXPORTING
      i_save                        = 'A'
      is_layout                     = p_layoutc
      is_variant                    = variantc
      it_toolbar_excluding          = it_ef1c[]
    CHANGING
      it_outtab                     = intab[]
      it_fieldcatalog               = p_fieldcat
      it_filter                     = it_filter
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.
