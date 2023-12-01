*&---------------------------------------------------------------------*
*& Include          LZCA_GUI_SCREENO01
*&---------------------------------------------------------------------*

MODULE status_01xx OUTPUT.
  zcl_gui_screen_controller=>get_instance( )->on_status( sy-repid ).
ENDMODULE.
