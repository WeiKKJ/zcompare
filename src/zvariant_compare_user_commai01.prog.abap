*----------------------------------------------------------------------*
***INCLUDE ZVARIANT_COMPARE_USER_COMMAI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0900  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0900 INPUT.
  DATA:ok_code LIKE sy-ucomm.
  DATA:save_ok LIKE sy-ucomm.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR '&F03' OR '&F15' OR '&F12'.
      LEAVE TO SCREEN 0.
    WHEN 'SHOWMSG'.
      IF ret2[] IS NOT INITIAL.
        PERFORM showmsg(zpubform) TABLES ret2.
      ELSE.
        MESSAGE s000(oo) WITH '数据一致'.
      ENDIF.
  ENDCASE.
  CLEAR:sy-ucomm.
ENDMODULE.
