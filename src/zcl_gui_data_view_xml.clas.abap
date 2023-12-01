CLASS zcl_gui_data_view_xml DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_gui_data_view .

    ALIASES mcs_pretty_mode
      FOR zif_gui_data_view~mcs_pretty_mode .
    ALIASES apply_changes
      FOR zif_gui_data_view~apply_changes .
    ALIASES display
      FOR zif_gui_data_view~display .
    ALIASES refresh
      FOR zif_gui_data_view~refresh .
    ALIASES set_compress
      FOR zif_gui_data_view~set_compress .
    ALIASES set_edit
      FOR zif_gui_data_view~set_edit .
    ALIASES set_pretty
      FOR zif_gui_data_view~set_pretty .
    ALIASES set_visible
      FOR zif_gui_data_view~set_visible .
    ALIASES data_changed
      FOR zif_gui_data_view~data_changed .
    ALIASES mtv_pretty_mode
      FOR zif_gui_data_view~mtv_pretty_mode .

    METHODS constructor
      IMPORTING
        !io_parent TYPE REF TO cl_gui_container
      RAISING
        zcx_gui_data_view .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mo_container TYPE REF TO cl_gui_splitter_container .
    DATA mo_text_editor TYPE REF TO cl_gui_textedit .
    DATA mo_html_viewer TYPE REF TO cl_gui_html_viewer .
    DATA mv_edit_mode TYPE abap_bool.
    DATA mv_compress_flag TYPE abap_bool.
    DATA mv_pretty_mode TYPE i.
    DATA mv_root_name TYPE string.
    DATA mo_data TYPE REF TO data.
    DATA mv_is_data_string TYPE flag.
ENDCLASS.



CLASS ZCL_GUI_DATA_VIEW_XML IMPLEMENTATION.


  METHOD constructor.
    CREATE OBJECT mo_container
      EXPORTING
        parent            = io_parent
        rows              = 1
        columns           = 1
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    CREATE OBJECT mo_html_viewer
      EXPORTING
        parent             = mo_container->get_container( row = 1 column = 1 )
      EXCEPTIONS
        cntl_error         = 1
        cntl_install_error = 2
        dp_install_error   = 3
        dp_error           = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    CREATE OBJECT mo_text_editor
      EXPORTING
        parent                     = mo_container->get_container( row = 1 column = 1 )
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_windowborder
        wordwrap_to_linebreak_mode = cl_gui_textedit=>false
      EXCEPTIONS
        error_cntl_create          = 1
        error_cntl_init            = 2
        error_cntl_link            = 3
        error_dp_create            = 4
        gui_type_not_supported     = 5
        OTHERS                     = 6.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    CALL METHOD mo_text_editor->set_font_fixed
      EXPORTING
        mode                   = cl_gui_textedit=>true
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    CALL METHOD mo_text_editor->set_toolbar_mode
      EXPORTING
        toolbar_mode           = cl_gui_textedit=>false
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    CALL METHOD mo_text_editor->set_statusbar_mode
      EXPORTING
        statusbar_mode         = cl_gui_textedit=>false
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    set_edit( abap_false ).
    set_pretty( mcs_pretty_mode-snake_case ).
  ENDMETHOD.


  METHOD zif_gui_data_view~apply_changes.
    DATA lo_data_new TYPE REF TO data.
    DATA lv_xml TYPE string.
    DATA lv_pretty_mode TYPE i.
    FIELD-SYMBOLS <l_data> TYPE any.
    FIELD-SYMBOLS <l_data_new> TYPE any.

    CHECK mv_edit_mode EQ abap_true.

    ASSIGN mo_data->* TO <l_data>.
    ASSERT sy-subrc EQ 0.

    CREATE DATA lo_data_new LIKE <l_data>.
    ASSIGN lo_data_new->* TO <l_data_new>.

    CALL METHOD mo_text_editor->get_textstream
      IMPORTING
        text                   = lv_xml
      EXCEPTIONS
        error_cntl_call_method = 1
        not_supported_by_gui   = 2
        OTHERS                 = 3.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2
        OTHERS            = 3.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    TRY.
        IF mv_is_data_string EQ abap_true.
          <l_data_new> = lv_xml.
        ELSE.
          CALL METHOD zcl_xml_serializer=>deserialize
            EXPORTING
              iv_xml         = lv_xml
              iv_pretty_name = mv_pretty_mode
            IMPORTING
              e_data         = <l_data_new>.
        ENDIF.
      CATCH cx_ixml_parse_error INTO DATA(lx_parse).
        RAISE EXCEPTION TYPE zcx_gui_data_view
          MESSAGE e002(zca_gui) " Ошибка десериализации
          EXPORTING
            previous = lx_parse.
    ENDTRY.

    IF <l_data_new> NE <l_data>.
      MOVE <l_data_new> TO <l_data>.
      RAISE EVENT data_changed.
    ENDIF.
  ENDMETHOD.


  METHOD zif_gui_data_view~display.
    mv_root_name = iv_root_name.
    mo_data = REF #( c_data ).

    DESCRIBE FIELD c_data TYPE DATA(lv_type).
    mv_is_data_string = boolc( lv_type CA 'gC' AND iv_root_name IS INITIAL ).

    refresh( ).
  ENDMETHOD.


  METHOD zif_gui_data_view~refresh.
    DATA lv_xml TYPE string.
    DATA lv_xhtml TYPE xstring.
    DATA lo_xml_document  TYPE REF TO cl_xml_document_base.
    DATA lv_html_url TYPE c LENGTH 500.
    DATA lt_html_lines TYPE swxmlcont.
    DATA lv_html_size TYPE i.
    FIELD-SYMBOLS <l_data> TYPE any.

    ASSIGN mo_data->* TO <l_data>.
    ASSERT sy-subrc EQ 0.

    IF mv_is_data_string EQ abap_true.
      lv_xml = <l_data>.

      CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
        EXPORTING
          text   = lv_xml
        IMPORTING
          buffer = lv_xhtml
        EXCEPTIONS
          failed = 1
          OTHERS = 2.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_gui_data_view.
      ENDIF.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lv_xhtml
        IMPORTING
          output_length = lv_html_size
        TABLES
          binary_tab    = lt_html_lines.
    ELSE.
      lv_xml = zcl_xml_serializer=>serialize(
          i_data         = <l_data>
          iv_root_tag    = mv_root_name
          iv_pretty_name = mv_pretty_mode
          iv_compress    = mv_compress_flag
      ).

      lo_xml_document = NEW #( ).
      lo_xml_document->parse_string( lv_xml ).

      CALL METHOD lo_xml_document->render_2_table
        IMPORTING
          table = lt_html_lines
          size  = lv_html_size.
    ENDIF.

    CALL METHOD mo_html_viewer->load_data
      EXPORTING
        type                   = 'TEXT'
        subtype                = 'XML'
        size                   = lv_html_size
      IMPORTING
        assigned_url           = lv_html_url
      CHANGING
        data_table             = lt_html_lines
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5.
    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    CALL METHOD mo_html_viewer->show_url
      EXPORTING
        url                    = lv_html_url
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    CALL METHOD mo_text_editor->set_textstream
      EXPORTING
        text                   = lv_xml
      EXCEPTIONS
        error_cntl_call_method = 1
        not_supported_by_gui   = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.
  ENDMETHOD.


  METHOD zif_gui_data_view~set_compress.
    mv_compress_flag = iv_value.
  ENDMETHOD.


  METHOD zif_gui_data_view~set_edit.
    mv_edit_mode = iv_value.

    CALL METHOD mo_html_viewer->set_visible
      EXPORTING
        visible           = boolc( mv_edit_mode EQ abap_false )
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    CALL METHOD mo_text_editor->set_visible
      EXPORTING
        visible           = boolc( mv_edit_mode EQ abap_true )
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.
  ENDMETHOD.


  METHOD zif_gui_data_view~set_pretty.
    CASE iv_value.
      WHEN mcs_pretty_mode-camel_case.
        mv_pretty_mode = zcl_xml_serializer=>mc_pretty_mode_camel_case.
      WHEN mcs_pretty_mode-snake_case.
        mv_pretty_mode = zcl_xml_serializer=>mc_pretty_mode_upper_case.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_gui_data_view
          MESSAGE e003(zca_gui). " Выбранный режим преобразования имен не поддерживается
    ENDCASE.
  ENDMETHOD.


  METHOD zif_gui_data_view~set_visible.
    CALL METHOD mo_container->set_visible
      EXPORTING
        visible           = iv_value
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
