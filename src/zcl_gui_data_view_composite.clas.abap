CLASS zcl_gui_data_view_composite DEFINITION
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

    TYPES mtv_data_viewer TYPE sy-ucomm .

    CONSTANTS:
      BEGIN OF mcs_data_view,
        xml  TYPE mtv_data_viewer VALUE 'XML_DATA_VIEW',
        json TYPE mtv_data_viewer VALUE 'JSON_DATA_VIEW',
        tree TYPE mtv_data_viewer VALUE 'TREE_DATA_VIEW',
      END OF mcs_data_view .
    CONSTANTS:
      BEGIN OF mcs_functions,
        toggle_display_change TYPE sy-ucomm VALUE 'ICON_TOGGLE_DISPLAY_CHANGE',
        save_changes          TYPE sy-ucomm VALUE 'SAVE_CHANGES',
        apply_changes         TYPE sy-ucomm VALUE 'APPLY_CHANGES',
        show_empty            TYPE sy-ucomm VALUE 'SHOW_EMPTY',
        hide_empty            TYPE sy-ucomm VALUE 'HIDE_EMPTY',
        camel_case            TYPE sy-ucomm VALUE 'CAMEL_CASE',
        snake_case            TYPE sy-ucomm VALUE 'SNAKE_CASE',
        xml_data_view         TYPE sy-ucomm VALUE mcs_data_view-xml,
        json_data_view        TYPE sy-ucomm VALUE mcs_data_view-json,
        tree_data_view        TYPE sy-ucomm VALUE mcs_data_view-tree,
      END OF mcs_functions .

    METHODS constructor
      IMPORTING
        !io_parent TYPE REF TO cl_gui_container
      RAISING
        zcx_gui_data_view .
    METHODS set_data_viewer
      IMPORTING
        !iv_value TYPE mtv_data_viewer
      RAISING
        zcx_gui_data_view .
    METHODS set_enable_function
      IMPORTING
        !iv_name  TYPE sy-ucomm
        !iv_value TYPE abap_bool
      RAISING
        zcx_gui_data_view .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF mts_data_viewer,
        ucomm    TYPE sy-ucomm,
        viewer   TYPE REF TO zif_gui_data_view,
        editable TYPE flag,
        icon     TYPE icon_d,
        text     TYPE string,
      END OF mts_data_viewer .
  types:
    mtt_data_viewer TYPE TABLE OF mts_data_viewer .
  types:
    BEGIN OF mts_toolbar,
        fcode	    TYPE ui_func,
        icon      TYPE icon_d,
        butn_type	TYPE tb_btype,
        quickinfo	TYPE iconquick,
        text      TYPE text40,
      END OF mts_toolbar .
  types:
    mtt_toolbar TYPE TABLE OF mts_toolbar WITH DEFAULT KEY .

  data MO_CONTAINER type ref to CL_GUI_SPLITTER_CONTAINER .
  data MO_TOOLBAR type ref to CL_GUI_TOOLBAR .
  data MT_DATA_VIEWER type MTT_DATA_VIEWER .
  data MT_TOOLBAR type MTT_TOOLBAR .
  data MO_DATA_VIEWER type ref to ZIF_GUI_DATA_VIEW .
  data MO_DATA type ref to DATA .
  data MV_EDIT type FLAG .

  methods ON_TOOLBAR_COMMAND
    for event FUNCTION_SELECTED of CL_GUI_TOOLBAR
    importing
      !FCODE .
  methods ON_DATA_CHANGED
    for event DATA_CHANGED of ZIF_GUI_DATA_VIEW .
ENDCLASS.



CLASS ZCL_GUI_DATA_VIEW_COMPOSITE IMPLEMENTATION.


  METHOD constructor.
    FIELD-SYMBOLS <ls_data_viewer> LIKE LINE OF mt_data_viewer.
    DATA lo_toolbar_container TYPE REF TO cl_gui_container.
    DATA lo_viewer_container TYPE REF TO cl_gui_container.

    CREATE OBJECT mo_container
      EXPORTING
        parent            = io_parent
        rows              = 2
        columns           = 1
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    CALL METHOD mo_container->set_row_mode
      EXPORTING
        mode              = cl_gui_splitter_container=>mode_absolute
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    CALL METHOD mo_container->set_row_height
      EXPORTING
        id                = 1
        height            = 26
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    CALL METHOD mo_container->set_row_sash
      EXPORTING
        id                = 1
        type              = cl_gui_splitter_container=>type_movable
        value             = cl_gui_splitter_container=>false
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    CALL METHOD mo_container->set_row_sash
      EXPORTING
        id                = 1
        type              = cl_gui_splitter_container=>type_sashvisible
        value             = cl_gui_splitter_container=>false
      EXCEPTIONS
        cntl_error        = 1
        cntl_system_error = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    lo_toolbar_container = mo_container->get_container( row = 1 column = 1 ).
    lo_viewer_container = mo_container->get_container( row = 2 column = 1 ).

    mt_data_viewer = VALUE #(
      (
        ucomm    = mcs_functions-xml_data_view
        viewer   = NEW zcl_gui_data_view_xml( lo_viewer_container )
        editable = abap_true
        icon     = icon_xml_doc
        text     = CONV #( 'XML представление'(xml) )
      )
      (
        ucomm    = mcs_functions-json_data_view
        viewer   = NEW zcl_gui_data_view_json( lo_viewer_container )
        editable = abap_true
        icon     = icon_htm
        text     = CONV #( 'JSON представление'(jsn) )
      )
      (
        ucomm    = mcs_functions-tree_data_view
        viewer   = NEW zcl_gui_data_view_tree( lo_viewer_container )
        editable = abap_false
        icon     = icon_display_tree
        text     = CONV #( 'Древовидное представление'(tre) )
      )
    ).

    CLEAR mt_toolbar[].
    APPEND LINES OF VALUE mtt_toolbar(
      (
        fcode	    = mcs_functions-toggle_display_change
        icon      = icon_toggle_display_change
        butn_type	= 0 " Кнопка (стандартная)
        quickinfo	= 'Просмотреть <-> Изменить'(edt)
      )
      (
        fcode	    = mcs_functions-save_changes
        icon      = icon_system_save
        butn_type	= 0 " Кнопка (стандартная)
        quickinfo	= 'Применить'(sav)
      )
      (
        fcode	    = mcs_functions-apply_changes
        icon      = icon_save_as_template
        butn_type	= 0 " Кнопка (стандартная)
        quickinfo	= 'Применить изменения'(apl)
      )
      (
        butn_type	= 3 " Разделитель
      )
      (
        fcode	    = mcs_functions-show_empty
        icon      = icon_expand
        butn_type	= 0 " Кнопка (стандартная)
        quickinfo	= 'Отображать все поля'(shw)
      )
      (
        fcode	    = mcs_functions-hide_empty
        icon      = icon_collapse
        butn_type	= 0 " Кнопка (стандартная)
        quickinfo	= 'Скрыть пустые поля'(hid)
      )
      (
        butn_type	= 3 " Разделитель
      )
    ) TO mt_toolbar.
    LOOP AT mt_data_viewer ASSIGNING <ls_data_viewer>.
      APPEND VALUE #(
        fcode	    = <ls_data_viewer>-ucomm
        icon      = <ls_data_viewer>-icon
        butn_type	= 5 " Зависимая кнопка (Checkbox)
        quickinfo	= <ls_data_viewer>-text
      ) TO mt_toolbar.
    ENDLOOP.
    APPEND LINES OF VALUE mtt_toolbar(
      (
        butn_type	= 3 " Разделитель
      )
      (
        fcode	    = mcs_functions-camel_case
        icon      = icon_text_field
        butn_type	= 0 " Кнопка (стандартная)
        text      = 'CamelCase'(ccm)
      )
      (
        fcode	    = mcs_functions-snake_case
        icon      = icon_text_field
        butn_type	= 0 " Кнопка (стандартная)
        text      = 'SNAKE_CASE'(scm)
      )
    ) TO mt_toolbar.

    CREATE OBJECT mo_toolbar
      EXPORTING
        parent             = lo_toolbar_container
      EXCEPTIONS
        cntl_install_error = 1
        cntl_error         = 2
        cntb_wrong_version = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.

    LOOP AT mt_toolbar ASSIGNING FIELD-SYMBOL(<ls_toolbar>).
      CALL METHOD mo_toolbar->add_button
        EXPORTING
          fcode            = <ls_toolbar>-fcode
          icon             = <ls_toolbar>-icon
          butn_type        = <ls_toolbar>-butn_type
          quickinfo        = <ls_toolbar>-quickinfo
          text             = <ls_toolbar>-text
        EXCEPTIONS
          cntl_error       = 1
          cntb_btype_error = 2
          cntb_error_fcode = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_gui_data_view.
      ENDIF.
    ENDLOOP.

    CALL METHOD mo_toolbar->set_registered_events
      EXPORTING
        events = VALUE #( ( eventid = cl_gui_toolbar=>m_id_function_selected ) ).

    SET HANDLER on_toolbar_command FOR mo_toolbar.

    LOOP AT mt_data_viewer ASSIGNING <ls_data_viewer>.
      SET HANDLER on_data_changed FOR <ls_data_viewer>-viewer.
    ENDLOOP.

    set_data_viewer( mcs_data_view-xml ).
    set_pretty( mcs_pretty_mode-snake_case ).
    set_edit( abap_false ).
  ENDMETHOD.


  METHOD on_data_changed.
    RAISE EVENT data_changed.
  ENDMETHOD.


  METHOD on_toolbar_command.
    DATA lx_ex TYPE REF TO zcx_gui_data_view.

    READ TABLE mt_data_viewer ASSIGNING FIELD-SYMBOL(<ls_data_viewer>)
      WITH KEY ucomm = fcode.
    IF sy-subrc EQ 0.
      set_data_viewer( <ls_data_viewer>-ucomm ).
      refresh( ).
      RETURN.
    ENDIF.

    CASE fcode.
      WHEN mcs_functions-toggle_display_change.
        TRY.
            set_edit( boolc( mv_edit EQ space ) ).
          CATCH zcx_gui_data_view.
        ENDTRY.

      WHEN mcs_functions-save_changes.
        TRY.
            apply_changes( ).
            refresh( ).
          CATCH zcx_gui_data_view INTO lx_ex.
            MESSAGE lx_ex TYPE rs_c_success DISPLAY LIKE rs_c_error.
        ENDTRY.

      WHEN mcs_functions-apply_changes.
        TRY.
            apply_changes( ).
            refresh( ).
            set_edit( space ).
          CATCH zcx_gui_data_view INTO lx_ex.
            MESSAGE lx_ex TYPE rs_c_success DISPLAY LIKE rs_c_error.
        ENDTRY.

      WHEN mcs_functions-show_empty.
        set_compress( abap_false ).
        refresh( ).

      WHEN mcs_functions-hide_empty.
        set_compress( abap_true ).
        refresh( ).

      WHEN mcs_functions-snake_case.
        set_pretty( zif_gui_data_view=>mcs_pretty_mode-snake_case ).
        refresh( ).

      WHEN mcs_functions-camel_case.
        set_pretty( zif_gui_data_view=>mcs_pretty_mode-camel_case ).
        refresh( ).

    ENDCASE.
  ENDMETHOD.


  METHOD set_data_viewer.
    FIELD-SYMBOLS <ls> LIKE LINE OF mt_data_viewer.

    READ TABLE mt_data_viewer ASSIGNING <ls>
      WITH KEY ucomm = iv_value.
    IF sy-subrc EQ 0.
      mo_data_viewer = <ls>-viewer.
      set_edit( boolc( mv_edit EQ abap_true AND <ls>-editable EQ abap_true ) ).
    ELSE.
      RAISE EXCEPTION TYPE zcx_gui_data_view
        MESSAGE e004(zca_gui). " Режим отображения данных не определен
    ENDIF.

    LOOP AT mt_data_viewer ASSIGNING <ls>.
      <ls>-viewer->set_visible( boolc( <ls>-ucomm EQ iv_value ) ).

      READ TABLE mo_toolbar->m_table_button TRANSPORTING NO FIELDS
        WITH KEY function = <ls>-ucomm
                 disabled = space.
      CHECK sy-subrc EQ 0.

      CALL METHOD mo_toolbar->set_button_state
        EXPORTING
          enabled          = abap_true
          checked          = boolc( <ls>-ucomm EQ iv_value )
          fcode            = <ls>-ucomm
        EXCEPTIONS
          cntl_error       = 1
          cntb_error_fcode = 2
          OTHERS           = 3.
      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE zcx_gui_data_view.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_enable_function.
    CALL METHOD mo_toolbar->set_button_state
      EXPORTING
        enabled          = iv_value
        checked          = abap_false
        fcode            = CONV #( iv_name )
      EXCEPTIONS
        cntl_error       = 1
        cntb_error_fcode = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_data_view.
    ENDIF.
  ENDMETHOD.


  METHOD zif_gui_data_view~apply_changes.
    mo_data_viewer->apply_changes( ).
  ENDMETHOD.


  METHOD zif_gui_data_view~display.
    LOOP AT mt_data_viewer ASSIGNING FIELD-SYMBOL(<ls_data_viewer>).
      CALL METHOD <ls_data_viewer>-viewer->display
        EXPORTING
          iv_root_name = iv_root_name
        CHANGING
          c_data       = c_data.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_gui_data_view~refresh.
    mo_data_viewer->refresh( ).
  ENDMETHOD.


  METHOD zif_gui_data_view~set_compress.
    LOOP AT mt_data_viewer ASSIGNING FIELD-SYMBOL(<ls_data_viewer>).
      <ls_data_viewer>-viewer->set_compress( iv_value ).
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_gui_data_view~set_edit.
    mo_data_viewer->set_edit( iv_value ).
    mv_edit = iv_value.
    set_enable_function(
        iv_name  = mcs_functions-save_changes
        iv_value = mv_edit ).
    set_enable_function(
        iv_name  = mcs_functions-apply_changes
        iv_value = mv_edit ).
  ENDMETHOD.


  METHOD zif_gui_data_view~set_pretty.
    LOOP AT mt_data_viewer ASSIGNING FIELD-SYMBOL(<ls_data_viewer>).
      <ls_data_viewer>-viewer->set_pretty( iv_value ).
    ENDLOOP.
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
