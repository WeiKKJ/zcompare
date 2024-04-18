*&---------------------------------------------------------------------*
*& Report ZVARIANT_COMPARE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zvariant_compare MESSAGE-ID zxmd_msg.
DATA:alv_grid                TYPE REF TO cl_gui_alv_grid,
     alv_grid_opt            TYPE REF TO cl_gui_alv_grid,
     alv_container           TYPE REF TO cl_gui_docking_container,
     alv_splitter_container  TYPE REF TO cl_gui_splitter_container,
     ref_container_left      TYPE REF TO cl_gui_container,
     ref_container_right     TYPE REF TO cl_gui_container,
     ref_container_left_top  TYPE REF TO cl_gui_container, " 用于放置top-of-page  17.04.2024 18:51:48 by kkw
     ref_container_right_top TYPE REF TO cl_gui_container,
     gref_doc_left           TYPE REF TO cl_dd_document,
     gref_viewer_left        TYPE REF TO cl_gui_html_viewer,
     gref_doc_right          TYPE REF TO cl_dd_document,
     gref_viewer_right       TYPE REF TO cl_gui_html_viewer.
DATA: gt_fldct     TYPE lvc_t_fcat,
      gs_slayt     TYPE lvc_s_layo,
      gt_fldct_opt TYPE lvc_t_fcat,
      gs_slayt_opt TYPE lvc_s_layo,
      ret2         TYPE TABLE OF bapiret2 WITH HEADER LINE,
      cc           TYPE char10.
DATA:gt_valutab  TYPE STANDARD TABLE OF rsparams,
     gt_valutabl TYPE STANDARD TABLE OF rsparamsl.
DATA:it_fieldname TYPE TABLE OF char30 WITH HEADER LINE.
FIELD-SYMBOLS:<tab>        TYPE STANDARD TABLE,
              <tab_opt>    TYPE STANDARD TABLE,
              <fs>         TYPE any,
              <fs_table>   TYPE ANY TABLE,
              <fs_opt>     TYPE any,
              <fs_alv>     TYPE any,
              <fs_opt_alv> TYPE any,
              <lvc_t_scol> TYPE lvc_t_scol.
DATA:dref           TYPE REF TO data,
     table_type     TYPE REF TO cl_abap_tabledescr,
     struct_type    TYPE REF TO cl_abap_structdescr,
     componentdescr TYPE abap_componentdescr,
     component_tab  TYPE abap_component_tab.
"alv展示用表
FIELD-SYMBOLS:<tab_alv>     TYPE STANDARD TABLE,
              <tab_opt_alv> TYPE STANDARD TABLE.
" 计算报表取数时长  17.04.2024 19:17:14 by kkw
TYPES:BEGIN OF ty_secds,
        lv_start TYPE i,
        lv_end   TYPE i,
        secds    TYPE p DECIMALS 1,
      END OF ty_secds.
DATA:wa_secds1 TYPE ty_secds,
     wa_secds2 TYPE ty_secds.
INCLUDE zvariant_compare_class.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE btxt1.
  PARAMETERS:p_rep    TYPE rsvar-report MEMORY ID prep,
             p_rep_op TYPE rsvar-report MEMORY ID prepop,
             p_varian TYPE rsvar-variant MEMORY ID pvarian.

SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  btxt1 = '数据筛选'(t01).

AT SELECTION-SCREEN. "PAI
  CASE sy-ucomm.
    WHEN 'ONLI'.
      PERFORM auth_check.
  ENDCASE.

INITIALIZATION.

START-OF-SELECTION.
  PERFORM getdata.

*&---------------------------------------------------------------------*
*&      Form  auth_check
*&---------------------------------------------------------------------*
FORM auth_check.
*  AUTHORITY-CHECK OBJECT 'M_BEST_WRK'
*  ID 'ACTVT' DUMMY
*  ID 'WERKS' FIELD p_werks.
*  IF sy-subrc <> 0.
*    MESSAGE e000(oo) WITH '无工厂权限:'(m01) p_werks.
*  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& getdata
*&---------------------------------------------------------------------*
FORM getdata.
  UNASSIGN:<tab>,<tab_opt>.
  CLEAR:ret2[].
  CALL FUNCTION 'RS_VARIANT_CONTENTS'
    EXPORTING
      report               = p_rep "程序名
      variant              = p_varian         "变式名
      move_or_write        = 'M'
*     no_import            = ' '
*     execute_direct       = ' '
* IMPORTING
*     sp                   =
    TABLES
*     l_params             =
*     l_params_nonv        =
*     l_selop              =
*     l_selop_nonv         =
      valutab              = gt_valutab
      valutabl             = gt_valutabl
*     OBJECTS              =
*     free_selections_desc =
*     free_selections_value       =
*     free_selections_obj  =
    EXCEPTIONS
      variant_non_existent = 1
      variant_obsolete     = 2
      OTHERS               = 3.
  IF sy-subrc <> 0.
    MESSAGE s000(oo) WITH '获取变式' p_varian '出现问题' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  DATA:ls_data   TYPE REF TO data,
       seltab    TYPE TABLE OF rsparams,
       seltab_wa LIKE LINE OF seltab.
  CLEAR:seltab,ls_data.
  LOOP AT gt_valutab ASSIGNING FIELD-SYMBOL(<gt_valutab>).
    CLEAR seltab_wa.
    seltab_wa-selname   = <gt_valutab>-selname.
    seltab_wa-kind      = <gt_valutab>-kind   .
    seltab_wa-sign      = <gt_valutab>-sign   .
    seltab_wa-option    = <gt_valutab>-option .
    seltab_wa-low       = <gt_valutab>-low    .
    seltab_wa-high      = <gt_valutab>-high   .
    APPEND seltab_wa TO seltab.
  ENDLOOP.

*  原程序  18.02.2024 17:36:59 by kkw
  cl_salv_bs_runtime_info=>set( display = abap_false metadata = abap_true data = abap_true ).
  CLEAR:wa_secds1,ls_data.
  GET RUN TIME FIELD wa_secds1-lv_start.
  SUBMIT (p_rep)
  WITH SELECTION-TABLE seltab
  AND RETURN
  .
  GET RUN TIME FIELD wa_secds1-lv_end.
  wa_secds1-secds = ( wa_secds1-lv_end - wa_secds1-lv_start ) / 1000000 .
*  获取原程序报表数据
  TRY.
      cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = ls_data ).
    CATCH  cx_salv_bs_sc_runtime_info.
      EXIT.
  ENDTRY.
*  获取原程序字段名及其描述
  TRY .
      DATA(metadata) = cl_salv_bs_runtime_info=>get_metadata( ).
    CATCH cx_salv_bs_sc_runtime_info.
  ENDTRY.
  cl_salv_bs_runtime_info=>clear_all( ).
  ASSIGN ls_data->* TO <tab>.
  IF <tab> IS NOT ASSIGNED.
    EXIT.
  ENDIF.

*  优化后程序  18.02.2024 17:37:13 by kkw
  cl_salv_bs_runtime_info=>set( display = abap_false metadata = abap_true data = abap_true ).
  CLEAR:wa_secds2,ls_data.
  GET RUN TIME FIELD wa_secds2-lv_start.
  SUBMIT (p_rep_op)
  WITH SELECTION-TABLE seltab
  AND RETURN
  .
  GET RUN TIME FIELD wa_secds2-lv_end.
  wa_secds2-secds = ( wa_secds2-lv_end - wa_secds2-lv_start ) / 1000000 .
*  获取优化后程序报表数据
  TRY.
      cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = ls_data ).
    CATCH  cx_salv_bs_sc_runtime_info.
      EXIT.
  ENDTRY.
*  获取优化后程序字段名及其描述
  TRY .
      DATA(metadata_opt) = cl_salv_bs_runtime_info=>get_metadata( ).
    CATCH cx_salv_bs_sc_runtime_info.
  ENDTRY.
  cl_salv_bs_runtime_info=>clear_all( ).
  ASSIGN ls_data->* TO <tab_opt>.
  IF <tab_opt> IS NOT ASSIGNED.
    EXIT.
  ENDIF.

*  比对数据  18.02.2024 17:40:14 by kkw
  CHECK <tab> IS ASSIGNED AND <tab_opt> IS ASSIGNED.
*  SORT <tab>.
*  SORT <tab_opt>.
  DESCRIBE TABLE <tab> LINES DATA(tab_lines).
  DESCRIBE TABLE <tab_opt> LINES DATA(tab_opt_lines).
  IF tab_lines NE tab_opt_lines.
    MESSAGE s000(oo) WITH '数据行数不一致' DISPLAY LIKE 'E'.
    PERFORM inmsg(zpubform) TABLES ret2 USING '' 'E' '' '数据行数不一致' '' '' ''.
    DATA(diff) = tab_lines - tab_opt_lines.
    IF tab_lines > tab_opt_lines.
      DO abs( diff ) TIMES.
        INSERT INITIAL LINE INTO TABLE <tab_opt> ASSIGNING FIELD-SYMBOL(<ff0>).
      ENDDO.
    ELSE.
      DO abs( diff ) TIMES.
        INSERT INITIAL LINE INTO TABLE <tab> ASSIGNING FIELD-SYMBOL(<ff1>).
      ENDDO.
    ENDIF.
  ENDIF.

*  DATA(component_table) = CAST cl_abap_structdescr(
*        CAST cl_abap_tabledescr(
*        cl_abap_tabledescr=>describe_by_data( <tab> )
*        )->get_table_line_type( )
*        )->get_components( ).
  DATA(compdescr_table) = CAST cl_abap_structdescr(
        CAST cl_abap_tabledescr(
        cl_abap_tabledescr=>describe_by_data( <tab> )
        )->get_table_line_type( )
        )->components.
  DATA(compdescr_table_opt) = CAST cl_abap_structdescr(
        CAST cl_abap_tabledescr(
        cl_abap_tabledescr=>describe_by_data( <tab_opt> )
        )->get_table_line_type( )
        )->components.
*  赋值优化后的表比优化前的表名字不一致的字段
  CLEAR:it_fieldname,it_fieldname[].
  LOOP AT compdescr_table_opt ASSIGNING FIELD-SYMBOL(<compdescr_table_opt>).
    CLEAR:it_fieldname.
    READ TABLE compdescr_table ASSIGNING FIELD-SYMBOL(<compdescr_table>) WITH KEY name = <compdescr_table_opt>-name.
    IF sy-subrc NE 0.
      it_fieldname = <compdescr_table_opt>-name.
      APPEND it_fieldname.
    ENDIF.
  ENDLOOP.

*  构建alv展示的内表和fieldcat
  CLEAR:gt_fldct,component_tab,dref.
  UNASSIGN:<tab_alv>.
  PERFORM catset TABLES gt_fldct USING: 'KKWXH' '' '' '序号'.
  LOOP AT compdescr_table ASSIGNING <compdescr_table>.
    CLEAR:componentdescr.
    componentdescr-name = <compdescr_table>-name.
    CASE <compdescr_table>-type_kind.
      WHEN 'h' OR 'v' OR 'u'.
        componentdescr-type ?= cl_abap_elemdescr=>get_c( p_length = 70 ).
      WHEN OTHERS.
        componentdescr-type ?= cl_abap_elemdescr=>get_by_kind( p_type_kind = <compdescr_table>-type_kind p_length = <compdescr_table>-length p_decimals = <compdescr_table>-decimals ).
    ENDCASE.
*    追加fieldname描述
    READ TABLE metadata-t_fcat ASSIGNING FIELD-SYMBOL(<t_fcat>) WITH KEY fieldname = <compdescr_table>-name.
    IF sy-subrc EQ 0.
      DATA(scrtext_l) = |{ <t_fcat>-scrtext_l }({ <compdescr_table>-name })|.
      PERFORM catset TABLES gt_fldct USING: <compdescr_table>-name '' '' scrtext_l.
    ELSE.
      PERFORM catset TABLES gt_fldct USING: <compdescr_table>-name '' '' <compdescr_table>-name.
    ENDIF.
    APPEND componentdescr TO component_tab.
  ENDLOOP.
*  插入alv颜色内表  20.02.2024 11:39:44 by kkw
  gs_slayt-ctab_fname = 'KKWKKW'.
  CLEAR:componentdescr.
  componentdescr-name = 'KKWKKW'.
  struct_type ?= cl_abap_structdescr=>describe_by_name( p_name = 'LVC_S_SCOL' ).
  componentdescr-type ?= cl_abap_tabledescr=>get( p_line_type = struct_type  ).
  APPEND componentdescr TO component_tab.
*  插入序号列  20.02.2024 11:39:21 by kkw
  CLEAR:componentdescr.
  componentdescr-name = 'KKWXH'.
  componentdescr-type ?= cl_abap_elemdescr=>get_i( ).
  INSERT componentdescr INTO component_tab INDEX 1.

  table_type = cl_abap_tabledescr=>create(
    p_line_type = cl_abap_structdescr=>create( component_tab )
    p_table_kind = cl_abap_tabledescr=>tablekind_std
    p_unique     = abap_false
    ).
  CREATE DATA dref TYPE HANDLE table_type.
  IF dref IS BOUND.
    ASSIGN dref->* TO <tab_alv>.
  ENDIF.
*  优化后的报表相关
  CLEAR:gt_fldct_opt,component_tab,dref.
  PERFORM catset TABLES gt_fldct_opt USING: 'KKWXH' '' '' '序号'.
  LOOP AT compdescr_table_opt ASSIGNING <compdescr_table>.
    CLEAR:componentdescr.
    componentdescr-name = <compdescr_table>-name.
    CASE <compdescr_table>-type_kind.
      WHEN 'h' OR 'v' OR 'u'.
        componentdescr-type ?= cl_abap_elemdescr=>get_c( p_length = 70 ).
      WHEN OTHERS.
        componentdescr-type ?= cl_abap_elemdescr=>get_by_kind( p_type_kind = <compdescr_table>-type_kind p_length = <compdescr_table>-length p_decimals = <compdescr_table>-decimals ).
    ENDCASE.
*    追加fieldname描述
    READ TABLE metadata_opt-t_fcat ASSIGNING FIELD-SYMBOL(<t_fcat_opt>) WITH KEY fieldname = <compdescr_table>-name.
    IF sy-subrc EQ 0.
      DATA(scrtext_l_opt) = |{ <t_fcat_opt>-scrtext_l }({ <compdescr_table>-name })|.
      PERFORM catset TABLES gt_fldct_opt USING: <compdescr_table>-name '' '' scrtext_l_opt.
    ELSE.
      PERFORM catset TABLES gt_fldct_opt USING: <compdescr_table>-name '' '' <compdescr_table>-name.
    ENDIF.
    APPEND componentdescr TO component_tab.
  ENDLOOP.
  gs_slayt_opt-ctab_fname = 'KKWKKW'.
  CLEAR:componentdescr.
  componentdescr-name = 'KKWKKW'.
  struct_type ?= cl_abap_structdescr=>describe_by_name( p_name = 'LVC_S_SCOL' ).
  componentdescr-type ?= cl_abap_tabledescr=>get( p_line_type = struct_type  ).
  APPEND componentdescr TO component_tab.

*  插入序号列  20.02.2024 11:39:21 by kkw
  CLEAR:componentdescr.
  componentdescr-name = 'KKWXH'.
  componentdescr-type ?= cl_abap_elemdescr=>get_i( ).
  INSERT componentdescr INTO component_tab INDEX 1.

  table_type = cl_abap_tabledescr=>create(
  p_line_type = cl_abap_structdescr=>create( component_tab )
  p_table_kind = cl_abap_tabledescr=>tablekind_std
  p_unique     = abap_false
  ).
  CREATE DATA dref TYPE HANDLE table_type.
  IF dref IS BOUND.
    ASSIGN dref->* TO <tab_opt_alv>.
  ENDIF.

*  拿着原始报表数据去比对优化后的报表数据，比对数据仅针对内表的首层扁平结构
  LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<wa_tab>).
    DATA(tabix) = sy-tabix.
    cc = tabix.
    CONDENSE cc NO-GAPS.
    INSERT INITIAL LINE INTO TABLE <tab_alv> ASSIGNING FIELD-SYMBOL(<tab_alv_new_line>).
    INSERT INITIAL LINE INTO TABLE <tab_opt_alv> ASSIGNING FIELD-SYMBOL(<tab_opt_alv_new_line>).
*    赋值序号列  20.02.2024 11:42:25 by kkw
    ASSIGN COMPONENT 'KKWXH' OF STRUCTURE <tab_alv_new_line> TO FIELD-SYMBOL(<kkwxh>).
    IF <kkwxh> IS ASSIGNED.
      <kkwxh> = tabix.
    ENDIF.
    ASSIGN COMPONENT 'KKWXH' OF STRUCTURE <tab_opt_alv_new_line> TO FIELD-SYMBOL(<kkwxh_opt>).
    IF <kkwxh_opt> IS ASSIGNED.
      <kkwxh_opt> = tabix.
    ENDIF.
*    取出对应行的优化后的报表的数据
    READ TABLE <tab_opt> ASSIGNING FIELD-SYMBOL(<wa_tab_opt>) INDEX tabix.
*    赋值优化后的报表不一致字段的颜色  19.02.2024 20:48:57 by kkw
    LOOP AT it_fieldname.
      ASSIGN COMPONENT it_fieldname OF STRUCTURE <tab_opt_alv_new_line> TO FIELD-SYMBOL(<ff>).
      IF <ff> IS ASSIGNED.
        ASSIGN COMPONENT 'KKWKKW' OF STRUCTURE <tab_opt_alv_new_line> TO <lvc_t_scol>.
        IF <lvc_t_scol> IS ASSIGNED.
          INSERT INITIAL LINE INTO TABLE <lvc_t_scol> ASSIGNING FIELD-SYMBOL(<lvc_t_scol_line>).
          <lvc_t_scol_line>-fname = it_fieldname.
          <lvc_t_scol_line>-color-col = 5.
          <lvc_t_scol_line>-color-int = 0.
          <lvc_t_scol_line>-color-inv = 0.
        ENDIF.
        UNASSIGN <ff>.
      ENDIF.
    ENDLOOP.
*    开始比对数据并将数据赋值到alv展示的内表  19.02.2024 13:03:34 by kkw
    LOOP AT compdescr_table ASSIGNING <compdescr_table>.
      UNASSIGN:<fs>,<fs_opt>,<fs_alv>,<fs_opt_alv>.
      ASSIGN COMPONENT <compdescr_table>-name OF STRUCTURE <wa_tab> TO <fs>.
      ASSIGN COMPONENT <compdescr_table>-name OF STRUCTURE <wa_tab_opt> TO <fs_opt>.
      ASSIGN COMPONENT <compdescr_table>-name OF STRUCTURE <tab_alv_new_line> TO <fs_alv>.
      ASSIGN COMPONENT <compdescr_table>-name OF STRUCTURE <tab_opt_alv_new_line> TO <fs_opt_alv>.
      IF <compdescr_table>-type_kind = 'h' AND  <compdescr_table>-name NE 'KKWKKW'.
        ASSIGN COMPONENT <compdescr_table>-name OF STRUCTURE <wa_tab> TO <fs_table>.
*        填充新内表
        <fs_alv> = |{ icon_list }{ <compdescr_table>-name }[ { lines( <fs_table> ) } ]|.
        CONTINUE.
      ELSEIF <compdescr_table>-type_kind = 'v' OR <compdescr_table>-type_kind = 'u'.
*        填充新内表
        <fs_alv> = |{ icon_structure }{ <compdescr_table>-name }|.
        CONTINUE.
      ENDIF.
*      比对数据填充新内表
      IF <fs> IS ASSIGNED AND <fs_opt> IS ASSIGNED.
        <fs_alv> = <fs>.
        <fs_opt_alv> = <fs_opt>.
        IF <fs> NE <fs_opt>.
          ASSIGN COMPONENT 'KKWKKW' OF STRUCTURE <tab_alv_new_line> TO <lvc_t_scol>.
          IF <lvc_t_scol> IS ASSIGNED.
            INSERT INITIAL LINE INTO TABLE <lvc_t_scol> ASSIGNING <lvc_t_scol_line>.
            <lvc_t_scol_line>-fname = <compdescr_table>-name.
            <lvc_t_scol_line>-color-col = 6.
            <lvc_t_scol_line>-color-int = 0.
            <lvc_t_scol_line>-color-inv = 0.
          ENDIF.
          ASSIGN COMPONENT 'KKWKKW' OF STRUCTURE <tab_opt_alv_new_line> TO <lvc_t_scol>.
          IF <lvc_t_scol> IS ASSIGNED.
            INSERT INITIAL LINE INTO TABLE <lvc_t_scol> ASSIGNING <lvc_t_scol_line>.
            <lvc_t_scol_line>-fname = <compdescr_table>-name.
            <lvc_t_scol_line>-color-col = 6.
            <lvc_t_scol_line>-color-int = 0.
            <lvc_t_scol_line>-color-inv = 0.
          ENDIF.
          PERFORM inmsg(zpubform) TABLES ret2 USING 'ZXMD_MSG' 'E' '135' cc <compdescr_table>-name <fs> <fs_opt>.
        ENDIF.
      ELSEIF <fs_opt> IS NOT ASSIGNED.
        <fs_alv> = <fs>.
        ASSIGN COMPONENT 'KKWKKW' OF STRUCTURE <tab_alv_new_line> TO <lvc_t_scol>.
        IF <lvc_t_scol> IS ASSIGNED.
          INSERT INITIAL LINE INTO TABLE <lvc_t_scol> ASSIGNING <lvc_t_scol_line>.
          <lvc_t_scol_line>-fname = <compdescr_table>-name.
          <lvc_t_scol_line>-color-col = 5.
          <lvc_t_scol_line>-color-int = 0.
          <lvc_t_scol_line>-color-inv = 0.
        ENDIF.
        PERFORM inmsg(zpubform) TABLES ret2 USING 'ZXMD_MSG' 'E' '136' cc <compdescr_table>-name <fs> p_rep_op.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
  CALL SCREEN 900.
ENDFORM.

*---------------------------------------------------------------------*
* set fieldcat
*---------------------------------------------------------------------*
FORM catset TABLES t_fldcat
USING pv_field pv_reftab pv_reffld pv_text.
  DATA: ls_fldcat TYPE lvc_s_fcat.

  ls_fldcat-fieldname =  pv_field.    "字段名
  ls_fldcat-scrtext_l =  pv_text.     "长描述
  ls_fldcat-scrtext_m =  ls_fldcat-scrtext_l.     "长描述
  ls_fldcat-scrtext_s =  ls_fldcat-scrtext_l.     "长描述
  ls_fldcat-coltext   =  ls_fldcat-scrtext_l.     "列描述
  ls_fldcat-ref_table =  pv_reftab.   "参考表名
  ls_fldcat-ref_field =  pv_reffld.   "参考字段名
  ls_fldcat-col_opt   = 'A'.          "自动优化列宽

  CASE ls_fldcat-fieldname.
    WHEN 'GSMNG'.
      ls_fldcat-qfieldname = 'MEINS'.
      ls_fldcat-no_zero    = 'X'.
    WHEN 'MENGE'.
      ls_fldcat-qfieldname = 'MEINS'.
      ls_fldcat-no_zero    = 'X'.
    WHEN 'WRBTR'.
      ls_fldcat-cfieldname = 'WAERS'.
    WHEN 'LIFNR' OR 'AUFNR' OR 'KUNNR'.
      ls_fldcat-edit_mask = '==ALPHA'.
    WHEN 'MATNR' OR 'IDNRK'.
      ls_fldcat-edit_mask = '==MATN1'.
    WHEN 'MEINS' .
      ls_fldcat-edit_mask = '==CUNIT'.
    WHEN 'KKWXH'.
      ls_fldcat-fix_column = 'X'.
*      ls_fldcat-key = 'X'.
  ENDCASE.

  APPEND ls_fldcat TO t_fldcat.
  CLEAR ls_fldcat.
ENDFORM.

INCLUDE zvariant_compare_status_090o01.

INCLUDE zvariant_compare_user_commai01.
