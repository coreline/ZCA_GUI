*&---------------------------------------------------------------------*
*& Include          LZCA_GUI_SCREENI01
*&---------------------------------------------------------------------*

MODULE user_command_01xx INPUT.
  zcl_gui_screen_controller=>get_instance( )->on_user_command(
      iv_ucomm = sy-ucomm
      iv_exit  = abap_false
  ).
ENDMODULE.

MODULE user_command_01xx_exit INPUT.
  zcl_gui_screen_controller=>get_instance( )->on_user_command(
      iv_ucomm = sy-ucomm
      iv_exit  = abap_true
  ).
  SET SCREEN 0.
ENDMODULE.
