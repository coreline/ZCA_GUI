CLASS zcl_gui_html_viewer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_gui_text_view .

    ALIASES display
      FOR zif_gui_text_view~display .
    ALIASES refresh
      FOR zif_gui_text_view~refresh .
    ALIASES set_visible
      FOR zif_gui_text_view~set_visible .
    ALIASES free
      FOR zif_gui_text_view~free .

    METHODS constructor
      IMPORTING
        !io_parent TYPE REF TO cl_gui_container
      RAISING
        zcx_gui_text_view .
  PROTECTED SECTION.
private section.

  data MO_HTML_VIEWER type ref to CL_GUI_HTML_VIEWER .
  data:
    mv_assigned_url TYPE c LENGTH 2048 .
  data MT_HTML_SOURCE type SOLI_TAB .
  data MV_CONTENT type STRING .
ENDCLASS.



CLASS ZCL_GUI_HTML_VIEWER IMPLEMENTATION.


  METHOD constructor.
    CREATE OBJECT mo_html_viewer
      EXPORTING
        parent             = io_parent
      EXCEPTIONS
        cntl_error         = 1
        cntl_install_error = 2
        dp_install_error   = 3
        dp_error           = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_text_view.
    ENDIF.
  ENDMETHOD.


  METHOD zif_gui_text_view~display.
    mv_content = iv_content.
    SHIFT mv_content LEFT DELETING LEADING space.
    IF iv_content IS INITIAL.
      mv_content = |<html></html>|.
    ELSEIF strlen( mv_content ) LE 12 OR
       NOT ( to_lower( mv_content(9) ) EQ '<!doctype'
          OR to_lower( mv_content(5) ) EQ '<html'
          OR to_lower( mv_content(5) ) EQ '<body' ).
      mv_content = |<html><head><meta charset="utf-8"></head><body>{ mv_content }</body></html>|.
    ENDIF.
    mt_html_source = cl_bcs_convert=>string_to_soli( iv_string = mv_content ).
    refresh( ).
  ENDMETHOD.


  METHOD zif_gui_text_view~free.
    IF mo_html_viewer IS BOUND.
      mo_html_viewer->free( ).
    ENDIF.
    FREE mo_html_viewer.
  ENDMETHOD.


  METHOD zif_gui_text_view~refresh.
    CALL METHOD mo_html_viewer->load_data
      EXPORTING
        url                    = mv_assigned_url
        type                   = 'text'
        subtype                = 'html'
      IMPORTING
        assigned_url           = mv_assigned_url
      CHANGING
        data_table             = mt_html_source
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_text_view.
    ENDIF.
    mo_html_viewer->show_url( mv_assigned_url ).
  ENDMETHOD.


  METHOD zif_gui_text_view~set_visible.
    CALL METHOD mo_html_viewer->set_visible
      EXPORTING
        visible           = iv_value
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_text_view.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
