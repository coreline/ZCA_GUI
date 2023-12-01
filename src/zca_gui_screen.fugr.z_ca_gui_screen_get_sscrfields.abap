FUNCTION Z_CA_GUI_SCREEN_GET_SSCRFIELDS .
*"----------------------------------------------------------------------
*"*"Локальный интерфейс:
*"  EXPORTING
*"     REFERENCE(ER_SSCRFIELDS_REF) TYPE REF TO
*"        ZCL_GUI_SCREEN_CONTROLLER=>MTS_SSCRFIELDS
*"----------------------------------------------------------------------

  er_sscrfields_ref = REF #( gs_sscrfields ).

ENDFUNCTION.
