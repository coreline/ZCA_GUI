class ZCL_GUI_SCREEN_CONTROLLER definition
  public
  final
  create private .

public section.

  types:
    BEGIN OF mts_sscrfields,
        functxt_01 TYPE rsfunc_txt,
        functxt_02 TYPE rsfunc_txt,
        functxt_03 TYPE rsfunc_txt,
        functxt_04 TYPE rsfunc_txt,
        functxt_05 TYPE rsfunc_txt,
        functxt_06 TYPE rsfunc_txt,
        functxt_07 TYPE rsfunc_txt,
        functxt_08 TYPE rsfunc_txt,
        functxt_09 TYPE rsfunc_txt,
        functxt_10 TYPE rsfunc_txt,
      END OF mts_sscrfields .
  types:
    BEGIN OF mts_gui_screen,
        container TYPE REF TO cl_gui_custom_container,
        logic     TYPE REF TO zcl_gui_screen,
        popup     TYPE abap_bool,
        exit      TYPE abap_bool,
      END OF mts_gui_screen .

  data:
    mt_gui_screen TYPE TABLE OF mts_gui_screen WITH EMPTY KEY .

  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCL_GUI_SCREEN_CONTROLLER .
  methods PUSH_SCREEN
    importing
      !IO_LOGIC type ref to ZCL_GUI_SCREEN
      !IV_POPUP type ABAP_BOOL .
  methods POP_SCREEN .
  methods ON_USER_COMMAND
    importing
      !IV_UCOMM type SY-UCOMM
      !IV_EXIT type ABAP_BOOL default SPACE .
  methods ON_STATUS
    importing
      !IV_REPORT type SY-REPID .
  methods IS_INITIALIZED
    returning
      value(RV_VALUE) type ABAP_BOOL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mo_instance TYPE REF TO zcl_gui_screen_controller .
    DATA mr_sscrfields_ref TYPE REF TO mts_sscrfields .
    DATA mv_report TYPE syrepid .
    DATA mv_status TYPE sypfkey .

    METHODS constructor .
    METHODS get_screen
      RETURNING
        VALUE(rs_screen) TYPE mts_gui_screen .
    METHODS create_container
      RETURNING
        VALUE(ro_container) TYPE REF TO cl_gui_custom_container .
    METHODS get_excluded_alv
      RETURNING
        VALUE(rt_exclude) TYPE ucomm_it .
    METHODS set_exit_flag .
ENDCLASS.



CLASS ZCL_GUI_SCREEN_CONTROLLER IMPLEMENTATION.


  METHOD constructor.
    CALL FUNCTION 'Z_CA_GUI_SCREEN_GET_SSCRFIELDS'
      IMPORTING
        er_sscrfields_ref = mr_sscrfields_ref.
  ENDMETHOD.


  METHOD create_container.
    ro_container = NEW #(
       container_name = 'CONTAINER'
       repid          = mv_report
       dynnr          = sy-dynnr
    ).
    mt_gui_screen[ 1 ]-container = ro_container.
  ENDMETHOD.


  METHOD get_excluded_alv.
    DATA(ls_screen) = get_screen( ).
    DATA(lo_alv) = ls_screen-logic->mo_alv.
    CHECK lo_alv IS BOUND.

    IF lo_alv->offline( ) EQ '1'.
      rt_exclude = VALUE #( BASE rt_exclude ( '&FG_SUM' ) ).
      rt_exclude = VALUE #( BASE rt_exclude ( '&GRAPH' ) ). " Графика
    ENDIF.

    IF cl_gui_object=>www_active EQ 'X'.
      rt_exclude = VALUE #( BASE rt_exclude ( '&VGRID' ) ). " SAP List Viewer
      rt_exclude = VALUE #( BASE rt_exclude ( '&VEXCEL' ) ). " Microsoft Excel
    ENDIF.

    CALL METHOD lo_alv->get_actual_view
      IMPORTING
        e_view = DATA(lv_view).
    IF lv_view EQ cl_gui_alv_grid=>mc_fc_view_excel.
      rt_exclude = VALUE #( BASE rt_exclude ( '&XXL' ) ). " Электронная таблица
      rt_exclude = VALUE #( BASE rt_exclude ( '&ETA' ) ). " Сведения
      rt_exclude = VALUE #( BASE rt_exclude ( '&VEXCEL' ) ). " Microsoft Excel
    ELSEIF lv_view EQ cl_gui_alv_grid=>mc_fc_view_grid.
      rt_exclude = VALUE #( BASE rt_exclude ( '&VGRID' ) ). " SAP List Viewer
    ELSEIF lv_view EQ cl_gui_alv_grid=>mc_fc_view_crystal.
      rt_exclude = VALUE #( BASE rt_exclude ( '&FG_SUM' ) ).
      rt_exclude = VALUE #( BASE rt_exclude ( '&FG_SORT' ) ).
      rt_exclude = VALUE #( BASE rt_exclude ( '&ETA' ) ). " Сведения
      rt_exclude = VALUE #( BASE rt_exclude ( '&RNT_PREV' ) ). " Просмотр перед печатью
      rt_exclude = VALUE #( BASE rt_exclude ( '&GRAPH' ) ). " Графика
    ELSEIF lv_view EQ cl_gui_alv_grid=>mc_fc_view_lotus.
      rt_exclude = VALUE #( BASE rt_exclude ( '&ETA' ) ). " Сведения
    ENDIF.

    CALL METHOD lo_alv->get_variant
      IMPORTING
        es_variant = DATA(ls_variant)
        e_save     = DATA(lv_save).
    IF ls_variant IS INITIAL.
      rt_exclude = VALUE #( BASE rt_exclude ( '&OAD' ) ). " Выбрать формат...
      rt_exclude = VALUE #( BASE rt_exclude ( '&AVE' ) ). " Сохранить формат...
    ENDIF.
    IF lv_save IS INITIAL.
      rt_exclude = VALUE #( BASE rt_exclude ( '&AVE' ) ). " Сохранить формат...
    ENDIF.

    CALL METHOD lo_alv->get_filter_criteria
      IMPORTING
        et_filter = DATA(lt_filter).
    IF lt_filter IS INITIAL.
      rt_exclude = VALUE #( BASE rt_exclude ( '&ILD' ) ). " Удалить фильтр
    ENDIF.

    CALL METHOD lo_alv->get_backend_fieldcatalog
      IMPORTING
        et_fieldcatalog = DATA(lt_fieldcat).
    IF NOT line_exists( lt_fieldcat[ no_sum = space tech = space ] ).
      rt_exclude = VALUE #( BASE rt_exclude ( '&FG_SUM' ) ).
    ENDIF.

    CALL METHOD lo_alv->get_frontend_layout
      IMPORTING
        es_layout = DATA(ls_layout).
    IF ls_layout-no_totline EQ abap_true.
      rt_exclude = VALUE #( BASE rt_exclude ( '&FG_SUM' ) ).
    ENDIF.

    IF line_exists( rt_exclude[ table_line = '&FG_EXPORT' ] ).
      rt_exclude = VALUE #( BASE rt_exclude ( '&XXL' ) ). " Электронная таблица
      rt_exclude = VALUE #( BASE rt_exclude ( '%PC' ) ). " Локальный файл
    ENDIF.

    IF lcl_alv_helper=>is_complex( lo_alv ).
      rt_exclude = VALUE #( BASE rt_exclude ( '&XXL' ) ). " Электронная таблица
    ENDIF.


    IF line_exists( rt_exclude[ table_line = '&FG_SORT' ] ).
      rt_exclude = VALUE #( BASE rt_exclude ( '&OUP' ) ). " Сортировка по вос...
      rt_exclude = VALUE #( BASE rt_exclude ( '&ODN' ) ). " Сортировка по нис...
    ENDIF.

    IF line_exists( rt_exclude[ table_line = '&FG_FILTER' ] ).
      rt_exclude = VALUE #( BASE rt_exclude ( '&ILT' ) ). " Установить фильтр
      rt_exclude = VALUE #( BASE rt_exclude ( '&ILD' ) ). " Удалить фильтр
    ENDIF.

    IF line_exists( rt_exclude[ table_line = '&FG_SUM' ] ).
      rt_exclude = VALUE #( BASE rt_exclude ( '&UMC' ) ). " Итого
    ENDIF.

    IF line_exists( rt_exclude[ table_line = '&FG_VARIANT' ] ).
      rt_exclude = VALUE #( BASE rt_exclude ( '&OAD' ) ). " Выбрать формат...
      rt_exclude = VALUE #( BASE rt_exclude ( '&OL0' ) ). " Изменить формат...
    ENDIF.

    IF line_exists( rt_exclude[ table_line = '&FG_VIEW' ] ).
      rt_exclude = VALUE #( BASE rt_exclude ( '&VGRID' ) ). " SAP List Viewer
      rt_exclude = VALUE #( BASE rt_exclude ( '&VEXCEL' ) ). " Microsoft Excel
      rt_exclude = VALUE #( BASE rt_exclude ( '&RNT_PREV' ) ). " Просмотр перед печатью
    ENDIF.

    IF line_exists( rt_exclude[ table_line = '&FG_SUBTOT' ] ).
      rt_exclude = VALUE #( BASE rt_exclude ( '&SUM' ) ). " Промежуточные суммы
    ENDIF.

    IF NOT cl_alv_check_third_party=>is_supported( cl_alv_bds=>mc_excel_frontend ).
      rt_exclude = VALUE #( BASE rt_exclude ( '&VEXCEL' ) ). " Microsoft Excel
    ENDIF.

    SORT rt_exclude.
    DELETE ADJACENT DUPLICATES FROM rt_exclude.

  ENDMETHOD.


  METHOD get_instance.
    IF mo_instance IS NOT BOUND.
      mo_instance = NEW #( ).
    ENDIF.
    ro_instance = mo_instance.
  ENDMETHOD.


  METHOD get_screen.
    rs_screen = mt_gui_screen[ 1 ].
  ENDMETHOD.


  METHOD is_initialized.
    rv_value = xsdbool( VALUE #( mt_gui_screen[ 1 ]-container OPTIONAL ) IS BOUND ).
  ENDMETHOD.


  METHOD on_status.
    mv_report = iv_report.

    DATA(ls_screen) = get_screen( ).
    IF NOT is_initialized( ).
      ls_screen-container = create_container( ).
      ls_screen-logic->raise_init( ls_screen-container ).
    ENDIF.
    ls_screen-logic->raise_pbo( ).

    mv_status = COND #( WHEN ls_screen-logic->mo_alv IS BOUND
      THEN 'SALV_TABLE_STANDARD' ELSE 'GUI_STATUS_DEFAULT'
    ).
    mr_sscrfields_ref->* = ls_screen-logic->ms_sscrfields.
    IF ls_screen-logic->mv_title IS NOT INITIAL.
      SET TITLEBAR 'GUI_TITLE_DEFAULT' OF PROGRAM mv_report
        WITH ls_screen-logic->mv_title.
    ENDIF.
    IF ls_screen-logic->mv_suppress_status IS INITIAL.
      DATA(lt_excluding) = get_excluded_alv( ).
      lt_excluding = VALUE ucomm_it( BASE lt_excluding
        FOR wa IN ls_screen-logic->mt_functions
        WHERE ( enabled EQ space )
          ( wa-ucomm )
      ).
      SET PF-STATUS mv_status OF PROGRAM mv_report
        EXCLUDING lt_excluding.
    ENDIF.
  ENDMETHOD.


  METHOD on_user_command.
    DATA(ls_screen) = get_screen( ).
    READ TABLE ls_screen-logic->mt_functions ASSIGNING FIELD-SYMBOL(<ls_func>)
      WITH TABLE KEY ucomm = iv_ucomm.
    DATA(lv_command) = COND #( WHEN sy-subrc EQ 0
      THEN <ls_func>-command ELSE iv_ucomm
    ).
    CHECK lv_command IS NOT INITIAL.
    CHECK ls_screen-exit IS INITIAL.
    IF iv_exit IS INITIAL.
      ls_screen-logic->raise_pai( lv_command ).
    ELSE.
      set_exit_flag( ).
      ls_screen-logic->raise_exit( lv_command ).
    ENDIF.
  ENDMETHOD.


  METHOD pop_screen.
    get_screen( )-container->free( ).
    DELETE mt_gui_screen INDEX 1.
    IF mt_gui_screen[] IS NOT INITIAL.
      mt_gui_screen[ 1 ]-container->set_visible( abap_true ).
    ENDIF.
  ENDMETHOD.


  METHOD push_screen.
    DATA ls_params TYPE mts_gui_screen.
    ls_params-logic = io_logic.
    ls_params-popup = iv_popup.
    IF mt_gui_screen[] IS NOT INITIAL.
      mt_gui_screen[ 1 ]-container->set_visible( space ).
    ENDIF.
    INSERT ls_params INTO mt_gui_screen INDEX 1.
  ENDMETHOD.


  METHOD set_exit_flag.
    mt_gui_screen[ 1 ]-exit = abap_true.
  ENDMETHOD.
ENDCLASS.
