*"* use this source file for the definition and implementation of *
"* local helper classes, interface definitions and type *
"* declarations

*&---------------------------------------------------------------------*
*& DEFINITION
*&---------------------------------------------------------------------*
CLASS lcl_alv_helper DEFINITION INHERITING FROM cl_gui_alv_grid.
  PUBLIC SECTION.
    CLASS-METHODS is_complex
      IMPORTING
        io_grid         TYPE REF TO cl_gui_alv_grid
      RETURNING
        VALUE(rv_value) TYPE abap_bool.

    CLASS-METHODS register_application_events
      IMPORTING
        io_grid TYPE REF TO cl_gui_alv_grid.
ENDCLASS.

CLASS lcl_alv_helper IMPLEMENTATION.
  METHOD is_complex.
    DATA: lr_structdescr   TYPE REF TO cl_abap_structdescr,
          lr_datadescr     TYPE REF TO cl_abap_datadescr,
          lr_tabledescr    TYPE REF TO cl_abap_tabledescr,
          lr_structdescr_c TYPE REF TO cl_abap_structdescr.
    FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.
    ASSIGN io_grid->mt_outtab->* TO <lt_table>.

    lr_datadescr ?= cl_abap_tabledescr=>describe_by_data( <lt_table> ).
    CASE lr_datadescr->type_kind.
      WHEN cl_abap_datadescr=>typekind_table.
        lr_tabledescr ?= lr_datadescr.
        lr_structdescr ?= lr_tabledescr->get_table_line_type( ).
      WHEN OTHERS.
        lr_structdescr ?= lr_datadescr.
    ENDCASE.

    IF lr_structdescr->struct_kind EQ cl_abap_structdescr=>structkind_nested.
      DATA(lt_components) = lr_structdescr->get_components( ).
      LOOP AT lt_components INTO DATA(ls_component).
        IF ls_component-type->kind EQ cl_abap_typedescr=>kind_struct.
          "1 level works "color-table kind = T! lr_structdescr_c ?= ls_component-type.
          IF lr_structdescr_c->struct_kind EQ cl_abap_structdescr=>structkind_nested.
            rv_value = abap_true.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD register_application_events.
    DATA lt_events_ex TYPE cntl_events.

    CALL METHOD io_grid->get_registered_events
      IMPORTING
        events = DATA(lt_events).

    lt_events_ex = VALUE #( FOR wa IN lt_events ( eventid = wa-eventid ) ).

    CALL METHOD io_grid->set_registered_events_ex
      EXPORTING
        eventtab                  = lt_events_ex
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
        OTHERS                    = 4.
  ENDMETHOD.
ENDCLASS.
