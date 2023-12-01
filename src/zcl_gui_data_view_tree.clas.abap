CLASS zcl_gui_data_view_tree DEFINITION
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

    TYPES:
      BEGIN OF mts_component,
        dref TYPE REF TO data,
        name TYPE string,
        flat TYPE string,
        type TYPE REF TO cl_abap_datadescr,
      END OF mts_component .
    TYPES:
      mtht_component TYPE HASHED TABLE OF mts_component WITH UNIQUE KEY dref .
    TYPES:
      BEGIN OF mts_tree,
        data_ref TYPE REF TO data,
        flag_out TYPE flag,
        value    TYPE string,
        text     TYPE string,
        node     TYPE REF TO cl_salv_node,
      END OF mts_tree .
    TYPES:
      mtt_tree TYPE TABLE OF mts_tree WITH UNIQUE SORTED KEY ref COMPONENTS data_ref .

    DATA mo_container TYPE REF TO cl_gui_splitter_container .
    DATA mo_tree TYPE REF TO cl_salv_tree .
    DATA mv_compress_flag TYPE abap_bool .
    DATA mv_pretty_mode TYPE mtv_pretty_mode .
    DATA mv_root_name TYPE string .
    DATA mo_data TYPE REF TO data .
    DATA mht_component TYPE mtht_component .
    DATA mt_tree_table TYPE mtt_tree .

    METHODS describe_data
      IMPORTING
        !io_data TYPE REF TO data .
    METHODS create_tree_view .
    METHODS add_tree_node
      IMPORTING
        !iv_root TYPE salv_de_node_key OPTIONAL
        !io_data TYPE REF TO data .
    METHODS get_node_descr
      IMPORTING
        !io_data       TYPE REF TO data
      EXPORTING
        VALUE(ev_name) TYPE string
        VALUE(ev_text) TYPE string .
    METHODS pretty_name
      IMPORTING
        !iv_value       TYPE string
      RETURNING
        VALUE(rv_value) TYPE string .
    METHODS snake_to_camel
      IMPORTING
        !iv_value       TYPE string
      RETURNING
        VALUE(rv_value) TYPE string .
ENDCLASS.



CLASS ZCL_GUI_DATA_VIEW_TREE IMPLEMENTATION.


  METHOD add_tree_node.
    DATA ls_row_data TYPE mts_tree.
    DATA lo_node TYPE REF TO cl_salv_node.
    FIELD-SYMBOLS <l_data> TYPE any.
    FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_row> TYPE any.
    FIELD-SYMBOLS <ls_row_data> TYPE mts_tree.

    ASSIGN io_data->* TO <l_data>.
    ASSERT sy-subrc EQ 0.

    CALL METHOD get_node_descr
      EXPORTING
        io_data = io_data
      IMPORTING
        ev_name = DATA(lv_name)
        ev_text = DATA(lv_text).

    ls_row_data = VALUE #( data_ref = io_data text = lv_text ).

    DESCRIBE FIELD <l_data> TYPE DATA(lv_type).
    CASE lv_type.
      WHEN cl_abap_typedescr=>typekind_table.
        ASSIGN io_data->* TO <lt_table>.
        LOOP AT <lt_table> ASSIGNING <ls_row>.
          add_tree_node( iv_root = iv_root io_data = REF #( <ls_row> ) ).
        ENDLOOP.

      WHEN cl_abap_typedescr=>typekind_struct1
        OR cl_abap_typedescr=>typekind_struct2.
        lo_node = mo_tree->get_nodes( )->add_node(
            related_node   = iv_root
            relationship   = cl_gui_column_tree=>relat_last_child
            data_row       = ls_row_data
            collapsed_icon = CONV #( icon_closed_folder )
            expanded_icon  = CONV #( icon_open_folder )
            row_style      = if_salv_c_tree_style=>intensified
            text           = CONV #( lv_name )
        ).
        DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE <l_data> TO FIELD-SYMBOL(<l_field>).
          IF sy-subrc NE 0.
            EXIT.
          ENDIF.
          add_tree_node( iv_root = lo_node->get_key( ) io_data = REF #( <l_field> ) ).
        ENDDO.

      WHEN OTHERS.
        ls_row_data-flag_out = abap_true.
        ls_row_data-value = |{ <l_data> }|.

        lo_node = mo_tree->get_nodes( )->add_node(
            related_node = iv_root
            relationship = cl_gui_column_tree=>relat_last_child
            data_row     = ls_row_data
            row_style    = if_salv_c_tree_style=>intensified
            text         = CONV #( lv_name )
        ).
    ENDCASE.

    READ TABLE mt_tree_table ASSIGNING FIELD-SYMBOL(<ls_tree_table>)
      WITH TABLE KEY ref COMPONENTS data_ref = io_data.
    IF sy-subrc EQ 0.
      <ls_tree_table>-node = lo_node.
    ENDIF.
  ENDMETHOD.


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

    TRY.
        CALL METHOD cl_salv_tree=>factory
          EXPORTING
            r_container = mo_container->get_container( row = 1 column = 1 )
          IMPORTING
            r_salv_tree = mo_tree
          CHANGING
            t_table     = mt_tree_table.

        mo_tree->get_tree_settings( )->set_hierarchy_header( CONV #( TEXT-001 ) ).            " Поле
        mo_tree->get_columns( )->get_column( 'FLAG_OUT' )->set_technical( if_salv_c_bool_sap=>false ).
        mo_tree->get_columns( )->get_column( 'VALUE' )->set_short_text( CONV #( TEXT-002 ) ). " Значение
        mo_tree->get_columns( )->get_column( 'TEXT' )->set_short_text( CONV #( TEXT-003 ) ).  " Описание
      CATCH cx_salv_error INTO DATA(lx_salv).
        RAISE EXCEPTION TYPE zcx_gui_data_view
          EXPORTING
            previous = lx_salv.
    ENDTRY.

    set_edit( abap_false ).
  ENDMETHOD.


  METHOD create_tree_view.
    mo_tree->get_nodes( )->delete_all( ).
    me->add_tree_node( mo_data ).
    mo_tree->get_nodes( )->expand_all( ).
    mo_tree->get_columns( )->set_optimize( abap_true ).
    mo_tree->get_functions( )->set_all( abap_false ).
    mo_tree->display( ).
  ENDMETHOD.


  METHOD describe_data.
    DATA lo_typedescr TYPE REF TO cl_abap_typedescr.
    DATA lo_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lt_component TYPE cl_abap_structdescr=>component_table.
    DATA ls_component TYPE abap_componentdescr.
    FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_struct> TYPE any.
    FIELD-SYMBOLS <l_field> TYPE any.
    FIELD-SYMBOLS <ls_component> TYPE LINE OF cl_abap_structdescr=>component_table.

    CHECK io_data IS BOUND.

    lo_typedescr = cl_abap_typedescr=>describe_by_data_ref( io_data ).

    " Таблица
    IF lo_typedescr->type_kind EQ cl_abap_typedescr=>typekind_table.
      ASSIGN io_data->* TO <lt_table>.
      LOOP AT <lt_table> ASSIGNING <ls_struct>.
        describe_data( REF #( <ls_struct> ) ).
      ENDLOOP.
      RETURN.
    ENDIF.

    " Структуры
    CHECK lo_typedescr->type_kind EQ cl_abap_typedescr=>typekind_struct1
       OR lo_typedescr->type_kind EQ cl_abap_typedescr=>typekind_struct2.
    ASSIGN io_data->* TO <ls_struct>.
    ASSERT sy-subrc EQ 0.

    lo_structdescr ?= lo_typedescr.
    lt_component = lo_structdescr->get_components( ).

    LOOP AT lt_component INTO ls_component
      WHERE as_include EQ abap_true.
      lo_structdescr ?= ls_component-type.
      APPEND LINES OF lo_structdescr->get_components( ) TO lt_component.
    ENDLOOP.

    LOOP AT lt_component INTO ls_component
      WHERE name IS NOT INITIAL OR suffix IS NOT INITIAL.
      IF ls_component-name IS INITIAL.
        ls_component-name = ls_component-suffix.
      ENDIF.
      ASSIGN COMPONENT ls_component-name OF STRUCTURE <ls_struct> TO <l_field>.
      ASSERT sy-subrc EQ 0.

      INSERT VALUE #(
          dref = REF #( <l_field> )
          name = ls_component-name
          type = ls_component-type
          flat = lo_structdescr->get_relative_name( )
      ) INTO TABLE mht_component.
    ENDLOOP.

    " Для глубокой вложенности вызываем рекурсию
    LOOP AT lt_component INTO ls_component
      WHERE as_include IS INITIAL.
      ASSIGN COMPONENT ls_component-name OF STRUCTURE <ls_struct> TO <l_field>.
      ASSERT sy-subrc EQ 0.

      lo_typedescr = cl_abap_typedescr=>describe_by_data( <l_field> ).
      CHECK lo_typedescr->type_kind EQ cl_abap_typedescr=>typekind_table
         OR lo_typedescr->type_kind EQ cl_abap_typedescr=>typekind_struct1
         OR lo_typedescr->type_kind EQ cl_abap_typedescr=>typekind_struct2.
      describe_data( REF #( <l_field> ) ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_node_descr.
    TYPES:
      BEGIN OF lts_ddic,
        ddlanguage TYPE ddlanguage,
        ddtext     TYPE as4text,
      END OF lts_ddic.
    DATA lt_text TYPE TABLE OF lts_ddic.
    DATA ls_text TYPE lts_ddic.
    DATA lv_name TYPE string.

    CLEAR: ev_name, ev_text.

    IF io_data EQ mo_data.
      lv_name = mv_root_name.
    ELSE.
      READ TABLE mht_component ASSIGNING FIELD-SYMBOL(<ls_compontne>)
        WITH TABLE KEY dref = io_data.
      IF sy-subrc EQ 0.
        lv_name = <ls_compontne>-name.
        IF <ls_compontne>-type->is_ddic_type( ) EQ abap_true.
          DATA(lv_relative_name) = <ls_compontne>-type->get_relative_name( ).
          CASE <ls_compontne>-type->type_kind.
            WHEN <ls_compontne>-type->kind_table.
              SELECT ddlanguage ddtext
                INTO CORRESPONDING FIELDS OF TABLE lt_text
                FROM dd40t
                WHERE typename EQ lv_relative_name
                  AND ddtext NE space.
            WHEN cl_abap_typedescr=>typekind_struct1
              OR cl_abap_typedescr=>typekind_struct2.
              SELECT ddlanguage ddtext
                INTO CORRESPONDING FIELDS OF TABLE lt_text
                FROM dd02t
                WHERE tabname EQ lv_relative_name
                  AND ddtext NE space.
            WHEN OTHERS.
              IF lv_relative_name IS NOT INITIAL.
                SELECT ddlanguage ddtext
                  INTO CORRESPONDING FIELDS OF TABLE lt_text
                  FROM dd04t
                  WHERE rollname EQ lv_relative_name
                    AND ddtext NE space.
              ELSEIF <ls_compontne>-flat IS NOT INITIAL.
                SELECT ddlanguage ddtext
                  INTO CORRESPONDING FIELDS OF TABLE lt_text
                  FROM dd03t
                  WHERE tabname EQ <ls_compontne>-flat
                    AND fieldname EQ <ls_compontne>-name
                    AND ddtext NE space.
              ENDIF.
          ENDCASE.
          DO 1 TIMES.
            READ TABLE lt_text INTO ls_text
              WITH KEY ddlanguage = sy-langu.
            CHECK sy-subrc NE 0.

            READ TABLE lt_text INTO ls_text
              WITH KEY ddlanguage = 'E'.
            CHECK sy-subrc NE 0.

            READ TABLE lt_text INTO ls_text INDEX 1.
          ENDDO.
          IF sy-subrc EQ 0.
            ev_text = ls_text-ddtext.
          ENDIF.
        ENDIF.
      ELSE.
        lv_name = abap_undefined.
      ENDIF.
    ENDIF.

    ev_name = pretty_name( lv_name ).
  ENDMETHOD.


  METHOD pretty_name.
    CASE mv_pretty_mode.
      WHEN mcs_pretty_mode-camel_case.
        rv_value = snake_to_camel( iv_value ).
      WHEN mcs_pretty_mode-snake_case.
        rv_value = to_upper( iv_value ).
      WHEN OTHERS.
        rv_value = iv_value.
    ENDCASE.
  ENDMETHOD.


  METHOD snake_to_camel.
    DATA lv_curr TYPE c VALUE '_'.
    DATA lv_prev TYPE c.
    DATA lv_text TYPE string.

    DO strlen( iv_value ) TIMES.
      lv_prev = lv_curr.
      lv_curr = substring( val = iv_value off = sy-index - 1 len = 1 ).
      CHECK lv_curr NE '_'.
      lv_curr = COND #( WHEN lv_prev EQ '_'
          THEN to_upper( lv_curr )
          ELSE to_lower( lv_curr ) ).
      lv_text = lv_text && lv_curr.
    ENDDO.

    rv_value = condense( lv_text ).
  ENDMETHOD.


  METHOD zif_gui_data_view~apply_changes.
    RAISE EXCEPTION TYPE zcx_gui_data_view
      MESSAGE e001(zca_gui). " Режим редактирования не поддерживается
  ENDMETHOD.


  METHOD zif_gui_data_view~display.
    mv_root_name = iv_root_name.
    mo_data = REF #( c_data ).
    describe_data( mo_data ).
    create_tree_view( ).
  ENDMETHOD.


  METHOD zif_gui_data_view~refresh.
    create_tree_view( ).
  ENDMETHOD.


  METHOD zif_gui_data_view~set_compress.
    mv_compress_flag = iv_value.
  ENDMETHOD.


  METHOD zif_gui_data_view~set_edit.
    IF iv_value EQ abap_true.
      RAISE EXCEPTION TYPE zcx_gui_data_view
        MESSAGE e001(zca_gui). " Режим редактирования не поддерживается
    ENDIF.
  ENDMETHOD.


  METHOD zif_gui_data_view~set_pretty.
    CASE iv_value.
      WHEN mcs_pretty_mode-camel_case.
        mv_pretty_mode = iv_value.
      WHEN mcs_pretty_mode-snake_case.
        mv_pretty_mode = iv_value.
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
