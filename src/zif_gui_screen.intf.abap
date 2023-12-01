INTERFACE zif_gui_screen
  PUBLIC .
  TYPES mtv_standard_command TYPE sy-ucomm.
  CONSTANTS mc_button_back TYPE mtv_standard_command VALUE 'BACK'.
  CONSTANTS mc_button_exit TYPE mtv_standard_command VALUE 'EXIT'.
  CONSTANTS mc_button_cancel TYPE mtv_standard_command VALUE 'RW'.
  CONSTANTS mc_button_refresh TYPE mtv_standard_command VALUE 'REFRESH'.
  CONSTANTS mc_button_save TYPE mtv_standard_command VALUE 'SAVE'.
ENDINTERFACE.
