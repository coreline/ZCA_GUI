FUNCTION z_ca_gui_screen_container.
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  IMPORTING
*"     REFERENCE(IV_TITLE) TYPE  CLIKE DEFAULT SPACE
*"     REFERENCE(IO_LOGIC) TYPE REF TO  ZCL_GUI_SCREEN
*"     REFERENCE(IV_NO_TOOLBAR) TYPE  C DEFAULT SPACE
*"     REFERENCE(IV_START_COLUMN) TYPE  I DEFAULT 0
*"     REFERENCE(IV_START_LINE) TYPE  I DEFAULT 0
*"     REFERENCE(IV_END_COLUMN) TYPE  I DEFAULT 0
*"     REFERENCE(IV_END_LINE) TYPE  I DEFAULT 0
*"----------------------------------------------------------------------
  DATA(lv_dynnr) = CONV sy-dynnr( 100 + strlen( iv_no_toolbar ) ).
  DATA(lv_popup) = abs( iv_start_column ) + abs( iv_start_line )
                 + abs( iv_end_column ) + abs( iv_end_line ).

  zcl_gui_screen_controller=>get_instance( )->push_screen(
      io_logic = io_logic
      iv_popup = xsdbool( lv_popup NE 0 )
  ).

  IF lv_popup IS INITIAL.
    CALL SCREEN lv_dynnr.
  ELSEIF iv_end_column EQ 0 AND iv_end_line EQ 0.
    CALL SCREEN lv_dynnr
      STARTING AT iv_start_column iv_start_line.
  ELSE.
    CALL SCREEN lv_dynnr
      STARTING AT iv_start_column iv_start_line
      ENDING AT iv_end_column iv_end_line.
  ENDIF.

  zcl_gui_screen_controller=>get_instance( )->pop_screen( ).
ENDFUNCTION.
