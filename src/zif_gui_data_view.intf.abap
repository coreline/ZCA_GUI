interface ZIF_GUI_DATA_VIEW
  public .


  types MTV_PRETTY_MODE type I .

  constants:
    BEGIN OF mcs_pretty_mode,
      camel_case TYPE mtv_pretty_mode VALUE 1,
      snake_case TYPE mtv_pretty_mode VALUE 2,
    END OF mcs_pretty_mode .

  events DATA_CHANGED .

  methods SET_VISIBLE
    importing
      !IV_VALUE type ABAP_BOOL
    raising
      ZCX_GUI_DATA_VIEW .
  methods SET_EDIT
    importing
      !IV_VALUE type ABAP_BOOL
    raising
      ZCX_GUI_DATA_VIEW .
  methods SET_COMPRESS
    importing
      !IV_VALUE type ABAP_BOOL
    raising
      ZCX_GUI_DATA_VIEW .
  methods SET_PRETTY
    importing
      !IV_VALUE type MTV_PRETTY_MODE
    raising
      ZCX_GUI_DATA_VIEW .
  methods DISPLAY
    importing
      !IV_ROOT_NAME type C default SPACE
    changing
      !C_DATA type ANY
    raising
      ZCX_GUI_DATA_VIEW .
  methods REFRESH
    raising
      ZCX_GUI_DATA_VIEW .
  methods APPLY_CHANGES
    raising
      ZCX_GUI_DATA_VIEW .
endinterface.
