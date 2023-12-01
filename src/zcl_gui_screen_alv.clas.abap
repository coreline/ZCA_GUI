class ZCL_GUI_SCREEN_ALV definition
  public
  create public .

public section.

  types:
    BEGIN OF mts_alv_display_settings,
        save                TYPE char01,
        s_variant           TYPE disvariant,
        s_layout            TYPE lvc_s_layo,
        t_toolbar_excluding TYPE ui_functions,
        t_fieldcatalog      TYPE lvc_t_fcat,
        t_sort              TYPE lvc_t_sort,
        t_filter            TYPE lvc_t_filt,
      END OF mts_alv_display_settings .

  events INIT
    exporting
      value(IO_GRID) type ref to CL_GUI_ALV_GRID
      value(IR_SETS) type ref to MTS_ALV_DISPLAY_SETTINGS .
  events EXIT
    exporting
      value(IV_COMMAND) type ZIF_GUI_SCREEN=>MTV_STANDARD_COMMAND .
  events PBO .
  events PAI
    exporting
      value(IV_COMMAND) type ZIF_GUI_SCREEN=>MTV_STANDARD_COMMAND .
  events DOUBLE_CLICK
    exporting
      value(IR_DATA) type ref to DATA
      value(IV_ROW) type SALV_DE_ROW
      value(IV_COLUMN) type SALV_DE_COLUMN .
  events ALV_FUNCTION
    exporting
      value(IR_COMMAND) type ref to SY-UCOMM .

  methods CONSTRUCTOR
    importing
      !IR_OUTTAB type ref to DATA .
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
  methods REFRESH
    raising
      ZCX_GUI_SCREEN .
  methods SET_EDITABLE
    importing
      !IV_VALUE type ABAP_BOOL default ABAP_TRUE .
  methods GET_FIELDCATALOG
    returning
      value(RT_FIELDCATALOG) type LVC_T_FCAT .
  methods SET_FIELDCATALOG
    importing
      !IT_FIELDCATALOG type LVC_T_FCAT .
  methods GET_SELECTED_ROWS
    importing
      !IV_EXPAND_SUBTOTALS type ABAP_BOOL default SPACE
    returning
      value(RT_ROWS) type INT4_TABLE .
  methods SET_SELECTED_ROWS
    importing
      !IT_ROWS type INT4_TABLE .
  methods GET_SELECTED_REF
    returning
      value(RT_TABLE) type TREXT_DATA_REF .
  methods USE_ALV_STATUS
    importing
      !IV_VALUE type ABAP_BOOL default ABAP_TRUE .
  PROTECTED SECTION.
    DATA mr_outtab TYPE REF TO data .

  PRIVATE SECTION.
    DATA mo_screen TYPE REF TO zcl_gui_screen .
    DATA mo_alv TYPE REF TO cl_gui_alv_grid .
    DATA mv_default_status TYPE abap_bool.
    DATA mv_pai_raised TYPE abap_bool.

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
    METHODS on_alv_function
      FOR EVENT alv_function OF zcl_gui_screen
      IMPORTING
        !ir_command .
    METHODS on_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
        !e_row
        !e_column
        !es_row_no .
    METHODS build_fieldcatalog
      RETURNING
        VALUE(rt_fcat) TYPE lvc_t_fcat .
ENDCLASS.



CLASS ZCL_GUI_SCREEN_ALV IMPLEMENTATION.


  METHOD build_fieldcatalog.
    DATA lo_row TYPE REF TO data.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    ASSIGN mr_outtab->* TO <lt_table>.

    DATA(lo_table_descr) = cl_abap_tabledescr=>describe_by_data( <lt_table> ).
    DATA(lo_struct_descr) = CAST cl_abap_tabledescr( lo_table_descr )->get_table_line_type( ).
    DATA(lv_relative_name) = CONV dd02l-tabname( lo_struct_descr->get_relative_name( ) ).
    IF lo_struct_descr->is_ddic_type( ).
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name   = lv_relative_name
          i_bypassing_buffer = abap_true
        CHANGING
          ct_fieldcat        = rt_fcat.
    ELSE.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = DATA(lo_salv_table)
        CHANGING
          t_table      = <lt_table>.
      rt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
          r_columns      = lo_salv_table->get_columns( )
          r_aggregations = lo_salv_table->get_aggregations( )
      ).
    ENDIF.

    CREATE DATA lo_row LIKE LINE OF <lt_table>.
    ASSIGN lo_row->* TO FIELD-SYMBOL(<ls_row>).
    LOOP AT rt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      ASSIGN COMPONENT <ls_fcat>-fieldname OF STRUCTURE <ls_row> TO FIELD-SYMBOL(<l_value>).
      CHECK sy-subrc EQ 0.
      DESCRIBE FIELD <l_value> TYPE DATA(lv_type).
      CHECK lv_type CA 'huvz'.
      DELETE rt_fcat.
    ENDLOOP.
    DELETE rt_fcat WHERE inttype CA 'huvz'.
  ENDMETHOD.


  METHOD change_function.
    mo_screen->change_function(
        iv_command = iv_command
        iv_icon    = iv_icon
        iv_text    = iv_text
        iv_tooltip = iv_tooltip
    ).
  ENDMETHOD.


  METHOD constructor.
    mr_outtab = ir_outtab.
    mo_screen = NEW #( ).
    SET HANDLER on_screen_init FOR mo_screen.
    SET HANDLER on_screen_exit FOR mo_screen.
    SET HANDLER on_screen_pbo FOR mo_screen.
    SET HANDLER on_screen_pai FOR mo_screen.
    SET HANDLER on_alv_function FOR mo_screen.
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


  METHOD get_fieldcatalog.
    CALL METHOD mo_alv->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = rt_fieldcatalog.
  ENDMETHOD.


  METHOD get_selected_ref.
    FIELD-SYMBOLS <lt_table> TYPE INDEX TABLE.
    ASSIGN mr_outtab->* TO <lt_table>.

    DATA(lt_rows) = get_selected_rows( ).
    LOOP AT lt_rows INTO DATA(lv_row).
      READ TABLE <lt_table> INDEX lv_row ASSIGNING FIELD-SYMBOL(<ls>).
      rt_table = VALUE #( BASE rt_table ( REF #( <ls> ) ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_selected_rows.
    CALL METHOD mo_alv->get_selected_rows
      IMPORTING
        et_index_rows = DATA(lt_rows).
    CALL METHOD mo_alv->get_subtotals
      IMPORTING
        et_grouplevels = DATA(lt_levels).

    LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<ls_row>).
      IF <ls_row>-rowtype IS INITIAL.
        APPEND <ls_row>-index TO rt_rows.
      ELSEIF iv_expand_subtotals EQ abap_true.
        DATA(ls_level) = lt_levels[ <ls_row>-index ].
        WHILE ls_level-index_from LE ls_level-index_to.
          APPEND ls_level-index_from TO rt_rows.
          ADD 1 TO ls_level-index_from.
        ENDWHILE.
      ENDIF.
    ENDLOOP.

    SORT rt_rows.
    DELETE ADJACENT DUPLICATES FROM rt_rows.
  ENDMETHOD.


  METHOD on_alv_function.
    RAISE EVENT alv_function
      EXPORTING
        ir_command = ir_command.
  ENDMETHOD.


  METHOD on_double_click.
    FIELD-SYMBOLS <lt_outtab> TYPE STANDARD TABLE.

    ASSIGN mr_outtab->* TO <lt_outtab>.
    CHECK sy-subrc EQ 0.

    READ TABLE <lt_outtab> ASSIGNING FIELD-SYMBOL(<ls>) INDEX e_row-index.
    CHECK sy-subrc EQ 0.

    RAISE EVENT double_click
      EXPORTING
        ir_data   = REF #( <ls> )
        iv_row    = CONV #( e_row-index )
        iv_column = CONV #( e_column-fieldname ).
  ENDMETHOD.


  METHOD on_screen_exit.
    RAISE EVENT exit
      EXPORTING
        iv_command = iv_command.
  ENDMETHOD.


  METHOD on_screen_init.
    DATA ls_set TYPE mts_alv_display_settings.
    FIELD-SYMBOLS <lt_table> TYPE STANDARD TABLE.
    ASSIGN mr_outtab->* TO <lt_table>.

    mo_alv = NEW #(
      i_parent      = io_container
      i_appl_events = abap_true
    ).
    IF mv_default_status IS INITIAL.
      mo_screen->use_alv_status( mo_alv ).
    ENDIF.

    " Чтобы были доступны вкладки "сортировка" и "фильтр" в "изменить формат"
    mo_alv->_get_grid_facade( )->set_fullscreen_hub( NEW lcl_fullscreen_grid_hub( ) ).

    ls_set-save = abap_undefined.

    ls_set-s_layout-zebra = abap_true.
    " Layout: sel_mode
    " ''  Выделение по умолчанию как B
    " A - Выделение строк и столбцов
    " B - Выделение строки
    " C - Выделение нескольких строк
    " D - Выделение ячеек
    ls_set-s_layout-sel_mode = 'A'.
    ls_set-s_layout-cwidth_opt = abap_true.
    ls_set-s_layout-no_toolbar = abap_true.

    ls_set-t_fieldcatalog = build_fieldcatalog( ).

    SET HANDLER on_double_click FOR mo_alv.

    RAISE EVENT init
      EXPORTING
        io_grid = mo_alv
        ir_sets = REF #( ls_set ).

    " I_SAVE:
    " ''  Сохранение формата не разрешено
    " U - Сохранение пользовательских форматов
    " X - Сохранение глобальных форматов
    " A - Сохранение любых форматов
    IF ls_set-s_variant-report IS INITIAL.
      ls_set-s_variant = VALUE #( ).
      ls_set-save = space.
    ELSEIF ls_set-save EQ abap_undefined.
      ls_set-save = 'A'.
    ENDIF.

    CALL METHOD mo_alv->set_table_for_first_display
      EXPORTING
        i_bypassing_buffer            = abap_true
        i_save                        = ls_set-save
        is_variant                    = ls_set-s_variant
        is_layout                     = ls_set-s_layout
        it_toolbar_excluding          = ls_set-t_toolbar_excluding
      CHANGING
        it_outtab                     = <lt_table>
        it_fieldcatalog               = ls_set-t_fieldcatalog
        it_sort                       = ls_set-t_sort
        it_filter                     = ls_set-t_filter
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_screen
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD on_screen_pai.
    mv_pai_raised = abap_true.
    RAISE EVENT pai
      EXPORTING
        iv_command = iv_command.
  ENDMETHOD.


  METHOD on_screen_pbo.
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


  METHOD refresh.
    CHECK mo_alv IS BOUND.
    CHECK mv_pai_raised EQ abap_true.

    " GetCurrentCell
    CALL METHOD mo_alv->get_current_cell
      IMPORTING
        e_row     = DATA(l_row)
        e_col     = DATA(l_col)
        es_row_id = DATA(ls_row)
        es_col_id = DATA(ls_col)
        es_row_no = DATA(ls_row_id).

    " GetSelectedCells
    CALL METHOD mo_alv->get_selected_cells
      IMPORTING
        et_cell = DATA(lt_cells).

    " GetSelectedCellsId
    CALL METHOD mo_alv->get_selected_cells_id
      IMPORTING
        et_cells = DATA(lt_cell_ids).

    " GetSelectedColumns
    CALL METHOD mo_alv->get_selected_columns
      IMPORTING
        et_index_columns = DATA(lt_cols).

    " GetSelectedRows
    CALL METHOD mo_alv->get_selected_rows
      IMPORTING
        et_index_rows = DATA(lt_rows)
        et_row_no     = DATA(lt_row_ids).

    " GetScrollInfoViaId
    CALL METHOD mo_alv->get_scroll_info_via_id
      IMPORTING
        es_row_no   = DATA(ls_row_id_s)
        es_row_info = DATA(ls_row_s)
        es_col_info = DATA(ls_col_s).


    mo_alv->refresh_table_display( ).

    " SetCurrentCell
    CALL METHOD mo_alv->set_current_cell_via_id
      EXPORTING
        is_row_id    = ls_row
        is_column_id = ls_col
        is_row_no    = ls_row_id.

    " SetSelectedCells
    CALL METHOD mo_alv->set_selected_cells
      EXPORTING
        it_cells = lt_cells.

    " SetSelectedCellsId
    CALL METHOD mo_alv->set_selected_cells_id
      EXPORTING
        it_cells = lt_cell_ids.

    " SetSelectedColumns
    CALL METHOD mo_alv->set_selected_columns
      EXPORTING
        it_col_table             = lt_cols
        is_keep_other_selections = abap_true.

    " SetSelectedRows
    CALL METHOD mo_alv->set_selected_rows
      EXPORTING
        it_index_rows            = lt_rows
        it_row_no                = lt_row_ids
        is_keep_other_selections = abap_true.

    " SetScrollInfoViaId
    CALL METHOD mo_alv->set_scroll_info_via_id
      EXPORTING
        is_row_no   = ls_row_id_s
        is_row_info = ls_row_s
        is_col_info = ls_col_s.
  ENDMETHOD.


  METHOD set_editable.
    CALL METHOD mo_alv->get_registered_events
      IMPORTING
        events = DATA(lt_events).
    IF NOT line_exists( lt_events[ eventid = cl_gui_alv_grid=>mc_evt_modified ]  ).
      mo_alv->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
      mo_alv->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ).
      mo_alv->activate_display_protocol( ).
    ENDIF.
    mo_alv->set_ready_for_input( strlen( iv_value ) ).
  ENDMETHOD.


  METHOD set_fieldcatalog.
    mo_alv->set_frontend_fieldcatalog( it_fieldcatalog ).
  ENDMETHOD.


  METHOD set_selected_rows.
    DATA lt_rows TYPE lvc_t_row.

    LOOP AT it_rows INTO DATA(lv_row).
      APPEND VALUE #( index = lv_row ) TO lt_rows.
    ENDLOOP.

    CALL METHOD mo_alv->set_selected_rows
      EXPORTING
        it_index_rows = lt_rows.
  ENDMETHOD.


  METHOD set_title.
    mo_screen->set_title( iv_title ).
  ENDMETHOD.


  METHOD use_alv_status.
    mv_default_status = xsdbool( iv_value EQ space ).
    IF iv_value EQ abap_true.
      mo_screen->use_alv_status( mo_alv ).
    ELSE.
      mo_screen->use_default_status( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
