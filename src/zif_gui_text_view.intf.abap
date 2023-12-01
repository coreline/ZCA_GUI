interface ZIF_GUI_TEXT_VIEW
  public .


  methods DISPLAY
    importing
      !IV_CONTENT type STRING
    raising
      ZCX_GUI_TEXT_VIEW .
  methods REFRESH
    raising
      ZCX_GUI_TEXT_VIEW .
  methods SET_VISIBLE
    importing
      !IV_VALUE type ABAP_BOOL
    raising
      ZCX_GUI_TEXT_VIEW .
  methods FREE .
endinterface.
