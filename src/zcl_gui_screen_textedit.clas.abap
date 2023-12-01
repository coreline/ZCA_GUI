class ZCL_GUI_SCREEN_TEXTEDIT definition
  public
  create public .

public section.

  events INIT
    exporting
      value(IO_TEXTEDIT) type ref to CL_GUI_TEXTEDIT .
  events EXIT
    exporting
      value(IV_COMMAND) type ZIF_GUI_SCREEN=>MTV_STANDARD_COMMAND .
  events PBO .
  events PAI
    exporting
      value(IV_COMMAND) type ZIF_GUI_SCREEN=>MTV_STANDARD_COMMAND .

  methods CONSTRUCTOR
    importing
      value(IV_TEXT) type STRING
      !IV_MAX_NUMBER_CHARS type I optional
      !IV_WORDWRAP_MODE type I default CL_GUI_TEXTEDIT=>WORDWRAP_AT_WINDOWBORDER
      !IV_WORDWRAP_POSITION type I default -1
      !IV_WORDWRAP_TO_LINEBREAK_MODE type I default CL_GUI_TEXTEDIT=>FALSE
      !IV_FILEDROP_MODE type I default CL_GUI_TEXTEDIT=>DROPFILE_EVENT_OFF .
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
  methods SET_READONLY .
  methods SET_EDITABLE
    importing
      !IV_VALUE type ABAP_BOOL default ABAP_TRUE .
  methods GET_TEXT
    returning
      value(RV_VALUE) type STRING .
  PROTECTED SECTION.
    DATA mr_outtab TYPE REF TO data .

  PRIVATE SECTION.
    DATA mo_screen TYPE REF TO zcl_gui_screen .
    DATA mo_textedit TYPE REF TO cl_gui_textedit .
    DATA mv_default_status TYPE abap_bool .
    DATA mv_text TYPE string.
    DATA mv_max_number_chars  TYPE i.
    DATA mv_wordwrap_mode	TYPE i.
    DATA mv_wordwrap_position	TYPE i.
    DATA mv_wordwrap_to_linebreak_mode TYPE i.
    DATA mv_filedrop_mode	TYPE i.

    METHODS on_screen_init
      FOR EVENT init OF zcl_gui_screen
      IMPORTING
        !io_container .
    METHODS on_screen_pbo
        FOR EVENT pbo OF zcl_gui_screen .
    METHODS on_screen_pai
      FOR EVENT pai OF zcl_gui_screen
      IMPORTING
        !iv_command .
    METHODS on_screen_exit
      FOR EVENT exit OF zcl_gui_screen
      IMPORTING
        !iv_command .
ENDCLASS.



CLASS ZCL_GUI_SCREEN_TEXTEDIT IMPLEMENTATION.


  METHOD change_function.
    mo_screen->change_function(
        iv_command = iv_command
        iv_icon    = iv_icon
        iv_text    = iv_text
        iv_tooltip = iv_tooltip
    ).
  ENDMETHOD.


  METHOD constructor.
    mv_text = iv_text.
    mv_max_number_chars = iv_max_number_chars.
    mv_wordwrap_mode = iv_wordwrap_mode.
    mv_wordwrap_position = iv_wordwrap_position.
    mv_wordwrap_to_linebreak_mode = iv_wordwrap_to_linebreak_mode.
    mv_filedrop_mode = iv_filedrop_mode.
    mo_screen = NEW #( ).
    SET HANDLER on_screen_init FOR mo_screen.
    SET HANDLER on_screen_exit FOR mo_screen.
    SET HANDLER on_screen_pbo FOR mo_screen.
    SET HANDLER on_screen_pai FOR mo_screen.
  ENDMETHOD.


  METHOD create_function.
    mo_screen->create_function(
      iv_command = iv_command
      iv_icon    = iv_icon
      iv_text    = iv_text
      iv_tooltip = iv_tooltip
    ).
  ENDMETHOD.


  METHOD disable_function.
    mo_screen->disable_function(
      iv_command = iv_command
      iv_value   = iv_value
    ).
  ENDMETHOD.


  METHOD display.
    mo_screen->display( ).
  ENDMETHOD.


  METHOD enable_function.
    mo_screen->enable_function(
      iv_command = iv_command
      iv_value   = iv_value
    ).
  ENDMETHOD.


  METHOD get_text.
    rv_value = mv_text.
  ENDMETHOD.


  METHOD on_screen_exit.
    RAISE EVENT exit
      EXPORTING
        iv_command = iv_command.
  ENDMETHOD.


  METHOD on_screen_init.
    mo_textedit = NEW #(
        max_number_chars           = mv_max_number_chars
        wordwrap_mode              = mv_wordwrap_mode
        wordwrap_position          = mv_wordwrap_position
        wordwrap_to_linebreak_mode = mv_wordwrap_to_linebreak_mode
        filedrop_mode              = mv_filedrop_mode
        parent                     = io_container
    ).
    mo_textedit->set_textstream( mv_text ).

    RAISE EVENT init
      EXPORTING
        io_textedit = mo_textedit.
  ENDMETHOD.


  METHOD on_screen_pai.
    CALL METHOD mo_textedit->get_textstream
      IMPORTING
        text = mv_text.
    cl_gui_cfw=>flush( ).

    RAISE EVENT pai
      EXPORTING
        iv_command = iv_command.
  ENDMETHOD.


  METHOD on_screen_pbo.
    CALL METHOD mo_textedit->get_textstream
      IMPORTING
        text = mv_text.
    RAISE EVENT pbo.
  ENDMETHOD.


  METHOD popup.
    mo_screen->popup(
      iv_col    = iv_col
      iv_row    = iv_row
      iv_width  = iv_width
      iv_height = iv_height
    ).
  ENDMETHOD.


  METHOD set_editable.
    mo_textedit->set_readonly_mode( COND #( WHEN iv_value EQ space THEN 1 ) ).
  ENDMETHOD.


  METHOD set_readonly.
    set_editable( space ).
  ENDMETHOD.


  METHOD set_title.
    mo_screen->set_title( iv_title ).
  ENDMETHOD.
ENDCLASS.
