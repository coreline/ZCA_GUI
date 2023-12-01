class ZCL_GUI_SCREEN definition
  public
  create public

  global friends ZCL_GUI_SCREEN_CONTROLLER .

public section.

  events INIT
    exporting
      value(IO_CONTAINER) type ref to CL_GUI_CUSTOM_CONTAINER optional .
  events EXIT
    exporting
      value(IV_COMMAND) type ZIF_GUI_SCREEN=>MTV_STANDARD_COMMAND .
  events PBO .
  events PAI
    exporting
      value(IV_COMMAND) type ZIF_GUI_SCREEN=>MTV_STANDARD_COMMAND .
  events ALV_FUNCTION
    exporting
      value(IR_COMMAND) type ref to SY-UCOMM .

  methods CONSTRUCTOR .
  methods CREATE_FUNCTION
    importing
      !IV_COMMAND type ZIF_GUI_SCREEN=>MTV_STANDARD_COMMAND
      !IV_ICON type SMP_DYNTXT-ICON_ID optional
      !IV_TEXT type SMP_DYNTXT-ICON_TEXT optional
      !IV_TOOLTIP type SMP_DYNTXT-QUICKINFO optional
    raising
      ZCX_GUI_SCREEN .
  methods ENABLE_FUNCTION
    importing
      !IV_COMMAND type ZIF_GUI_SCREEN=>MTV_STANDARD_COMMAND
      !IV_VALUE type ABAP_BOOL default ABAP_TRUE
    raising
      ZCX_GUI_SCREEN .
  methods DISABLE_FUNCTION
    importing
      !IV_COMMAND type ZIF_GUI_SCREEN=>MTV_STANDARD_COMMAND
      !IV_VALUE type ABAP_BOOL default ABAP_TRUE
    raising
      ZCX_GUI_SCREEN .
  methods CHANGE_FUNCTION
    importing
      !IV_COMMAND type ZIF_GUI_SCREEN=>MTV_STANDARD_COMMAND
      !IV_ICON type SMP_DYNTXT-ICON_ID optional
      !IV_TEXT type SMP_DYNTXT-ICON_TEXT optional
      !IV_TOOLTIP type SMP_DYNTXT-QUICKINFO optional
    raising
      ZCX_GUI_SCREEN .
  methods IS_INITIALIZED
    returning
      value(RV_VALUE) type ABAP_BOOL .
  methods HIDE_TOOLBAR
    raising
      ZCX_GUI_SCREEN .
  methods DISPLAY .
  methods POPUP
    importing
      !IV_COL type I default 1
      !IV_ROW type I default 1
      !IV_WIDTH type I optional
      !IV_HEIGHT type I optional .
  methods SET_TITLE
    importing
      !IV_TITLE type CSEQUENCE .
  methods USE_ALV_STATUS
    importing
      !IO_GRID type ref to CL_GUI_ALV_GRID
    raising
      ZCX_GUI_SCREEN .
  methods USE_DEFAULT_STATUS .
  methods SUPPRESS_STATUS
    importing
      !IV_VALUE type ABAP_BOOL default ABAP_TRUE .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF mts_functions,
        ucomm   TYPE sy-ucomm,
        command TYPE zif_gui_screen=>mtv_standard_command,
        dyntxt  TYPE smp_dyntxt,
        rsfunc  TYPE REF TO rsfunc_txt,
        enabled TYPE flag,
      END OF mts_functions .
  types:
    mtt_functions TYPE TABLE OF mts_functions WITH KEY ucomm .

  data MT_FUNCTIONS type MTT_FUNCTIONS .
  data MS_SSCRFIELDS type ZCL_GUI_SCREEN_CONTROLLER=>MTS_SSCRFIELDS .
  data MV_TOOLBAR_HIDDEN type ABAP_BOOL .
  data MV_TITLE type STRING .
  data MV_SUPPRESS_STATUS type ABAP_BOOL .
  data MO_ALV type ref to CL_GUI_ALV_GRID .

  methods RAISE_INIT
    importing
      !IO_CONTAINER type ref to CL_GUI_CUSTOM_CONTAINER .
  methods RAISE_EXIT
    importing
      !IV_COMMAND type SY-UCOMM .
  methods RAISE_PBO .
  methods RAISE_PAI
    importing
      value(IV_COMMAND) type SY-UCOMM .
ENDCLASS.



CLASS ZCL_GUI_SCREEN IMPLEMENTATION.


  METHOD change_function.
    IF NOT line_exists( mt_functions[ command = iv_command ] ).
      RAISE EXCEPTION TYPE zcx_gui_screen
        MESSAGE e005(zca_gui) " Функция & не существует
        WITH iv_command.
    ENDIF.
    READ TABLE mt_functions ASSIGNING FIELD-SYMBOL(<ls_command>)
      WITH KEY command = iv_command.
    <ls_command>-dyntxt = VALUE #(
        text      = COND #( WHEN iv_text NE space THEN iv_text ELSE iv_command )
        icon_id   = iv_icon
        icon_text = iv_text
        quickinfo = iv_tooltip
    ).
    <ls_command>-rsfunc->* = <ls_command>-dyntxt.
  ENDMETHOD.


  METHOD constructor.
    mv_title = sy-title.
    mt_functions = VALUE #(
      ( ucomm = 'BACK'    command = zif_gui_screen=>mc_button_back    enabled = abap_true )
      ( ucomm = 'EXIT'    command = zif_gui_screen=>mc_button_exit    enabled = abap_true )
      ( ucomm = 'RW'      command = zif_gui_screen=>mc_button_cancel  enabled = abap_true )
      ( ucomm = 'REFRESH' command = zif_gui_screen=>mc_button_refresh )
      ( ucomm = 'SAVE'    command = zif_gui_screen=>mc_button_save )
      ( ucomm = 'FC01'    rsfunc = REF #( ms_sscrfields-functxt_01 ) )
      ( ucomm = 'FC02'    rsfunc = REF #( ms_sscrfields-functxt_02 ) )
      ( ucomm = 'FC03'    rsfunc = REF #( ms_sscrfields-functxt_03 ) )
      ( ucomm = 'FC04'    rsfunc = REF #( ms_sscrfields-functxt_04 ) )
      ( ucomm = 'FC05'    rsfunc = REF #( ms_sscrfields-functxt_05 ) )
      ( ucomm = 'FC06'    rsfunc = REF #( ms_sscrfields-functxt_06 ) )
      ( ucomm = 'FC07'    rsfunc = REF #( ms_sscrfields-functxt_07 ) )
      ( ucomm = 'FC08'    rsfunc = REF #( ms_sscrfields-functxt_08 ) )
      ( ucomm = 'FC09'    rsfunc = REF #( ms_sscrfields-functxt_09 ) )
      ( ucomm = 'FC10'    rsfunc = REF #( ms_sscrfields-functxt_10 ) )
    ).
  ENDMETHOD.


  METHOD create_function.
    IF line_exists( mt_functions[ command = iv_command ] ).
      RAISE EXCEPTION TYPE zcx_gui_screen
        MESSAGE e006(zca_gui) " Функция & уже существует
        WITH iv_command.
    ENDIF.
    IF NOT line_exists( mt_functions[ command = space ] ).
      RAISE EXCEPTION TYPE zcx_gui_screen
        MESSAGE e007(zca_gui). " Превышен предел создания функций
    ENDIF.
    READ TABLE mt_functions ASSIGNING FIELD-SYMBOL(<ls_command>)
      WITH KEY command = space.
    <ls_command>-command = iv_command.
    <ls_command>-enabled = abap_true.
    <ls_command>-dyntxt = VALUE #(
        text      = COND #( WHEN iv_text NE space THEN iv_text ELSE iv_command )
        icon_id   = iv_icon
        icon_text = iv_text
        quickinfo = iv_tooltip
    ).
    <ls_command>-rsfunc->* = <ls_command>-dyntxt.
  ENDMETHOD.


  METHOD disable_function.
    enable_function(
        iv_command = iv_command
        iv_value   = xsdbool( iv_value EQ space )
    ).
  ENDMETHOD.


  METHOD display.
    popup( iv_row = 0 iv_col = 0 iv_width = 0 iv_height = 0 ).
  ENDMETHOD.


  METHOD enable_function.
    IF NOT line_exists( mt_functions[ command = iv_command ] ).
      RAISE EXCEPTION TYPE zcx_gui_screen
        MESSAGE e005(zca_gui) " Функция & не существует
        WITH iv_command.
    ENDIF.
    mt_functions[ command = iv_command ]-enabled = xsdbool( iv_value NE space ).
  ENDMETHOD.


  METHOD hide_toolbar.
    CHECK mv_toolbar_hidden IS INITIAL.
    IF is_initialized( ).
      RAISE EXCEPTION TYPE zcx_gui_screen
        MESSAGE e008(zca_gui). " Экран инициализирован. Действие невозможно
    ENDIF.
    mv_toolbar_hidden = abap_true.
  ENDMETHOD.


  METHOD is_initialized.
    rv_value = zcl_gui_screen_controller=>get_instance( )->is_initialized( ).
  ENDMETHOD.


  METHOD popup.
    IF iv_width * iv_height NE 0.
      DATA(lv_end_column) = iv_col + iv_width.
      DATA(lv_end_line) = iv_row + iv_height.
    ENDIF.

    CALL FUNCTION 'Z_CA_GUI_SCREEN_CONTAINER'
      EXPORTING
        io_logic        = me
        iv_no_toolbar   = mv_toolbar_hidden
        iv_start_column = iv_col
        iv_start_line   = iv_row
        iv_end_column   = lv_end_column
        iv_end_line     = lv_end_line.
  ENDMETHOD.


  METHOD raise_exit.
    RAISE EVENT exit
      EXPORTING
        iv_command = iv_command.
  ENDMETHOD.


  METHOD raise_init.
    RAISE EVENT init
      EXPORTING
        io_container = io_container.
  ENDMETHOD.


  METHOD raise_pai.
    DO 1 TIMES.
      CHECK mo_alv IS BOUND.

      CALL METHOD mo_alv->transfer_fcode_slis_to_lvc
        EXPORTING
          i_fcode_slis   = iv_command
        IMPORTING
          e_fcode_lvc    = DATA(lv_dummy)
        EXCEPTIONS
          no_match_found = 1.
      CHECK sy-subrc EQ 0.

      RAISE EVENT alv_function
        EXPORTING
          ir_command = REF #( iv_command ).
      IF iv_command IS NOT INITIAL.
        CALL METHOD mo_alv->set_function_code
          CHANGING
            c_ucomm = iv_command.
      ENDIF.
    ENDDO.
    IF iv_command IS NOT INITIAL.
      RAISE EVENT pai
        EXPORTING
          iv_command = iv_command.
    ENDIF.
  ENDMETHOD.


  METHOD raise_pbo.
    RAISE EVENT pbo.
  ENDMETHOD.


  METHOD set_title.
    mv_title = iv_title.
  ENDMETHOD.


  METHOD suppress_status.
    mv_suppress_status = iv_value.
  ENDMETHOD.


  METHOD use_alv_status.
    IF io_grid IS NOT BOUND.
      RAISE EXCEPTION TYPE zcx_gui_screen
        MESSAGE e010(zca_gui). " Не определена ссылка на инстанцию CL_GUI_ALV_GRID
    ENDIF.
    IF mo_alv IS BOUND AND mo_alv NE io_grid.
      RAISE EXCEPTION TYPE zcx_gui_screen
        MESSAGE e009(zca_gui). " GUI-статус уже используется другой таблицей
    ENDIF.

    mo_alv = io_grid.
  ENDMETHOD.


  METHOD use_default_status.
    CLEAR mo_alv.
  ENDMETHOD.
ENDCLASS.
