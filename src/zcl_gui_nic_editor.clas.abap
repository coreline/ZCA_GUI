class ZCL_GUI_NIC_EDITOR definition
  public
  final
  create public .

public section.

  interfaces ZIF_GUI_TEXT_VIEW .

  aliases DISPLAY
    for ZIF_GUI_TEXT_VIEW~DISPLAY .
  aliases FREE
    for ZIF_GUI_TEXT_VIEW~FREE .
  aliases REFRESH
    for ZIF_GUI_TEXT_VIEW~REFRESH .
  aliases SET_VISIBLE
    for ZIF_GUI_TEXT_VIEW~SET_VISIBLE .

  events SAVE
    exporting
      value(IV_HTML) type STRING .

  methods CONSTRUCTOR
    importing
      !IO_PARENT type ref to CL_GUI_CONTAINER
      !IV_EDITOR_WIDTH type I optional
      !IV_EDITOR_HEIGHT type I optional
    raising
      ZCX_GUI_TEXT_VIEW .
  methods GET_CONTENT
    returning
      value(RV_VALUE) type STRING .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_html_viewer TYPE REF TO cl_gui_html_viewer .
    DATA mo_parent_container TYPE REF TO cl_gui_container .
    DATA:
      mv_assigned_url TYPE c LENGTH 2048 .
    DATA mv_editor_source_template TYPE string .
    DATA mt_editor_source TYPE soli_tab .
    DATA mv_auto_width TYPE abap_bool .
    DATA mv_auto_height TYPE abap_bool .
    DATA mv_editor_width TYPE i .
    DATA mv_editor_height TYPE i .
    DATA mv_content TYPE string .

    METHODS init_editor_source
      RAISING
        zcx_gui_text_view .
    METHODS on_sapevent
      FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING
        !action
        !frame
        !getdata
        !postdata
        !query_table .
    METHODS init_editor_template .
    METHODS get_control_size
      IMPORTING
        !io_control     TYPE REF TO cl_gui_control
        !iv_side        TYPE csequence
      RETURNING
        VALUE(rv_value) TYPE i .
ENDCLASS.



CLASS ZCL_GUI_NIC_EDITOR IMPLEMENTATION.


  METHOD constructor.
    mo_parent_container = io_parent.
    mv_editor_width = iv_editor_width.
    mv_editor_height = iv_editor_height.
    mv_auto_width = xsdbool( iv_editor_width EQ 0 ).
    mv_auto_height = xsdbool( iv_editor_height EQ 0 ) .

    CREATE OBJECT mo_html_viewer
      EXPORTING
        parent             = mo_parent_container
      EXCEPTIONS
        cntl_error         = 1
        cntl_install_error = 2
        dp_install_error   = 3
        dp_error           = 4
        OTHERS             = 5.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_text_view.
    ENDIF.

    init_editor_template( ).
    IF mv_editor_source_template IS INITIAL.
      RAISE EXCEPTION TYPE zcx_gui_text_view.
    ENDIF.
  ENDMETHOD.


  METHOD get_content.
    rv_value = mv_content.
  ENDMETHOD.


  METHOD get_control_size.
    io_control->set_metric( cl_gui_control=>metric_pixel ).
    CASE iv_side.
      WHEN 'WIDTH'.
        CALL METHOD io_control->get_width
          IMPORTING
            width      = rv_value
          EXCEPTIONS
            cntl_error = 1
            OTHERS     = 2.
      WHEN 'HEIGHT'.
        CALL METHOD io_control->get_height
          IMPORTING
            height     = rv_value
          EXCEPTIONS
            cntl_error = 1
            OTHERS     = 2.
    ENDCASE.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_text_view.
    ENDIF.

    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_gui_text_view.
    ENDIF.
  ENDMETHOD.


  METHOD init_editor_source.
    IF mv_auto_width EQ abap_true.
      CLEAR mv_editor_width.
    ENDIF.
    IF mv_auto_height EQ abap_true.
      CLEAR mv_editor_height.
    ENDIF.

    IF mv_editor_width IS INITIAL.
      mv_editor_width = get_control_size(
        io_control = mo_html_viewer
        iv_side    = 'WIDTH'
      ).
      IF mv_editor_width IS INITIAL.
        mv_editor_width = get_control_size(
          io_control = mo_parent_container
          iv_side    = 'WIDTH'
        ).
      ENDIF.
      mv_editor_width -= 10.
    ENDIF.

    IF mv_editor_height IS INITIAL.
      mv_editor_height = get_control_size(
        io_control = mo_html_viewer
        iv_side    = 'HEIGHT'
      ).
      IF mv_editor_height IS INITIAL.
        mv_editor_height = get_control_size(
          io_control = mo_parent_container
          iv_side    = 'HEIGHT'
        ).
      ENDIF.
      mv_editor_height -= 60.
    ENDIF.

    DATA(lv_html) = mv_editor_source_template.
    lv_html = replace( val = lv_html sub = '&EditorWidth&' with = |{ mv_editor_width }px| ).
    lv_html = replace( val = lv_html sub = '&EditorHeight&' with = |{ mv_editor_height }px| ).
    lv_html = replace( val = lv_html sub = '&TextToReplace&' with = escape( val = mv_content format = cl_abap_format=>e_html_text ) ).
    mt_editor_source = cl_bcs_convert=>string_to_soli( iv_string = lv_html ).
  ENDMETHOD.


  METHOD init_editor_template.
    DATA(lv_nic_script_base64) = ||
      && |dmFyIGJrRXh0ZW5kPWZ1bmN0aW9uKCl7dmFyIHQ9YXJndW1lbnRzO2Zvcih2YXIgZSBpbiAxPT10Lmxlbmd0aCYmKHQ9W3RoaXMsdFswXV0pLHRbMV0pdFsw|
      && |XVtlXT10WzFdW2VdO3JldHVybiB0WzBdfTtmdW5jdGlvbiBia0NsYXNzKCl7fWJrQ2xhc3MucHJvdG90eXBlLmNvbnN0cnVjdD1mdW5jdGlvbigpe30sYmtD|
      && |bGFzcy5leHRlbmQ9ZnVuY3Rpb24odCl7dmFyIGU9ZnVuY3Rpb24oKXtpZihhcmd1bWVudHNbMF0hPT1ia0NsYXNzKXJldHVybiB0aGlzLmNvbnN0cnVjdC5h|
      && |cHBseSh0aGlzLGFyZ3VtZW50cyl9LG49bmV3IHRoaXMoYmtDbGFzcyk7cmV0dXJuIGJrRXh0ZW5kKG4sdCksZS5wcm90b3R5cGU9bixlLmV4dGVuZD10aGlz|
      && |LmV4dGVuZCxlfTt2YXIgYmtFbGVtZW50PWJrQ2xhc3MuZXh0ZW5kKHtjb25zdHJ1Y3Q6ZnVuY3Rpb24odCxlKXtyZXR1cm4ic3RyaW5nIj09dHlwZW9mIHQm|
      && |Jih0PShlfHxkb2N1bWVudCkuY3JlYXRlRWxlbWVudCh0KSksdD0kQksodCl9LGFwcGVuZFRvOmZ1bmN0aW9uKHQpe3JldHVybiB0LmFwcGVuZENoaWxkKHRo|
      && |aXMpLHRoaXN9LGFwcGVuZEJlZm9yZTpmdW5jdGlvbih0KXtyZXR1cm4gdC5wYXJlbnROb2RlLmluc2VydEJlZm9yZSh0aGlzLHQpLHRoaXN9LGFkZEV2ZW50|
      && |OmZ1bmN0aW9uKHQsZSl7cmV0dXJuIGJrTGliLmFkZEV2ZW50KHRoaXMsdCxlKSx0aGlzfSxzZXRDb250ZW50OmZ1bmN0aW9uKHQpe3JldHVybiB0aGlzLmlu|
      && |bmVySFRNTD10LHRoaXN9LHBvczpmdW5jdGlvbigpe3ZhciB0PWN1cnRvcD0wO29iaj10aGlzO2lmKG9iai5vZmZzZXRQYXJlbnQpZG97dCs9b2JqLm9mZnNl|
      && |dExlZnQsY3VydG9wKz1vYmoub2Zmc2V0VG9wfXdoaWxlKG9iaj1vYmoub2Zmc2V0UGFyZW50KTt2YXIgZT13aW5kb3cub3BlcmE/MDpwYXJzZUludCh0aGlz|
      && |LmdldFN0eWxlKCJib3JkZXItd2lkdGgiKXx8dGhpcy5zdHlsZS5ib3JkZXIpfHwwO3JldHVyblt0K2UsY3VydG9wK2UrdGhpcy5vZmZzZXRIZWlnaHRdfSxu|
      && |b1NlbGVjdDpmdW5jdGlvbigpe3JldHVybiBia0xpYi5ub1NlbGVjdCh0aGlzKSx0aGlzfSxwYXJlbnRUYWc6ZnVuY3Rpb24odCl7dmFyIGU9dGhpcztkb3tp|
      && |ZihlJiZlLm5vZGVOYW1lJiZlLm5vZGVOYW1lLnRvVXBwZXJDYXNlKCk9PXQpcmV0dXJuIGU7ZT1lLnBhcmVudE5vZGV9d2hpbGUoZSk7cmV0dXJuITF9LGhh|
      && |c0NsYXNzOmZ1bmN0aW9uKHQpe3JldHVybiB0aGlzLmNsYXNzTmFtZS5tYXRjaChuZXcgUmVnRXhwKCIoXFxzfF4pbmljRWRpdC0iK3QrIihcXHN8JCkiKSl9|
      && |LGFkZENsYXNzOmZ1bmN0aW9uKHQpe3JldHVybiB0aGlzLmhhc0NsYXNzKHQpfHwodGhpcy5jbGFzc05hbWUrPSIgbmljRWRpdC0iK3QpLHRoaXN9LHJlbW92|
      && |ZUNsYXNzOmZ1bmN0aW9uKHQpe3JldHVybiB0aGlzLmhhc0NsYXNzKHQpJiYodGhpcy5jbGFzc05hbWU9dGhpcy5jbGFzc05hbWUucmVwbGFjZShuZXcgUmVn|
      && |RXhwKCIoXFxzfF4pbmljRWRpdC0iK3QrIihcXHN8JCkiKSwiICIpKSx0aGlzfSxzZXRTdHlsZTpmdW5jdGlvbih0KXt2YXIgZT10aGlzLnN0eWxlO2Zvcih2|
      && |YXIgbiBpbiB0KXN3aXRjaChuKXtjYXNlImZsb2F0IjplLmNzc0Zsb2F0PWUuc3R5bGVGbG9hdD10W25dO2JyZWFrO2Nhc2Uib3BhY2l0eSI6ZS5vcGFjaXR5|
      && |PXRbbl0sZS5maWx0ZXI9ImFscGhhKG9wYWNpdHk9IitNYXRoLnJvdW5kKDEwMCp0W25dKSsiKSI7YnJlYWs7Y2FzZSJjbGFzc05hbWUiOnRoaXMuY2xhc3NO|
      && |YW1lPXRbbl07YnJlYWs7ZGVmYXVsdDplW25dPXRbbl19cmV0dXJuIHRoaXN9LGdldFN0eWxlOmZ1bmN0aW9uKHQsZSl7dmFyIG49ZXx8ZG9jdW1lbnQuZGVm|
      && |YXVsdFZpZXc7aWYoMT09dGhpcy5ub2RlVHlwZSlyZXR1cm4gbiYmbi5nZXRDb21wdXRlZFN0eWxlP24uZ2V0Q29tcHV0ZWRTdHlsZSh0aGlzLG51bGwpLmdl|
      && |dFByb3BlcnR5VmFsdWUodCk6dGhpcy5jdXJyZW50U3R5bGVbYmtMaWIuY2FtZWxpemUodCldfSxyZW1vdmU6ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5wYXJl|
      && |bnROb2RlLnJlbW92ZUNoaWxkKHRoaXMpLHRoaXN9LHNldEF0dHJpYnV0ZXM6ZnVuY3Rpb24odCl7Zm9yKHZhciBlIGluIHQpdGhpc1tlXT10W2VdO3JldHVy|
      && |biB0aGlzfX0pLGJrTGliPXtpc01TSUU6LTEhPW5hdmlnYXRvci5hcHBWZXJzaW9uLmluZGV4T2YoIk1TSUUiKSxhZGRFdmVudDpmdW5jdGlvbih0LGUsbil7|
      && |dC5hZGRFdmVudExpc3RlbmVyP3QuYWRkRXZlbnRMaXN0ZW5lcihlLG4sITEpOnQuYXR0YWNoRXZlbnQoIm9uIitlLG4pfSx0b0FycmF5OmZ1bmN0aW9uKHQp|
      && |e2Zvcih2YXIgZT10Lmxlbmd0aCxuPW5ldyBBcnJheShlKTtlLS07KW5bZV09dFtlXTtyZXR1cm4gbn0sbm9TZWxlY3Q6ZnVuY3Rpb24odCl7dC5zZXRBdHRy|
      && |aWJ1dGUmJiJpbnB1dCIhPXQubm9kZU5hbWUudG9Mb3dlckNhc2UoKSYmInRleHRhcmVhIiE9dC5ub2RlTmFtZS50b0xvd2VyQ2FzZSgpJiZ0LnNldEF0dHJp|
      && |YnV0ZSgidW5zZWxlY3RhYmxlIiwib24iKTtmb3IodmFyIGU9MDtlPHQuY2hpbGROb2Rlcy5sZW5ndGg7ZSsrKWJrTGliLm5vU2VsZWN0KHQuY2hpbGROb2Rl|
      && |c1tlXSl9LGNhbWVsaXplOmZ1bmN0aW9uKHQpe3JldHVybiB0LnJlcGxhY2UoL1wtKC4pL2csKGZ1bmN0aW9uKHQsZSl7cmV0dXJuIGUudG9VcHBlckNhc2Uo|
      && |KX0pKX0saW5BcnJheTpmdW5jdGlvbih0LGUpe3JldHVybiBudWxsIT1ia0xpYi5zZWFyY2godCxlKX0sc2VhcmNoOmZ1bmN0aW9uKHQsZSl7Zm9yKHZhciBu|
      && |PTA7bjx0Lmxlbmd0aDtuKyspaWYodFtuXT09ZSlyZXR1cm4gbjtyZXR1cm4gbnVsbH0sY2FuY2VsRXZlbnQ6ZnVuY3Rpb24odCl7cmV0dXJuKHQ9dHx8d2lu|
      && |ZG93LmV2ZW50KS5wcmV2ZW50RGVmYXVsdCYmdC5zdG9wUHJvcGFnYXRpb24mJih0LnByZXZlbnREZWZhdWx0KCksdC5zdG9wUHJvcGFnYXRpb24oKSksITF9|
      && |LGRvbUxvYWQ6W10sZG9tTG9hZGVkOmZ1bmN0aW9uKCl7aWYoIWFyZ3VtZW50cy5jYWxsZWUuZG9uZSlmb3IoYXJndW1lbnRzLmNhbGxlZS5kb25lPSEwLGk9|
      && |MDtpPGJrTGliLmRvbUxvYWQubGVuZ3RoO2krKylia0xpYi5kb21Mb2FkW2ldKCl9LG9uRG9tTG9hZGVkOmZ1bmN0aW9uKHQpe3RoaXMuZG9tTG9hZC5wdXNo|
      && |KHQpLGRvY3VtZW50LmFkZEV2ZW50TGlzdGVuZXI/ZG9jdW1lbnQuYWRkRXZlbnRMaXN0ZW5lcigiRE9NQ29udGVudExvYWRlZCIsYmtMaWIuZG9tTG9hZGVk|
      && |LG51bGwpOmJrTGliLmlzTVNJRSYmKGRvY3VtZW50LndyaXRlKCI8c3R5bGU+Lm5pY0VkaXQtbWFpbiBwIHsgbWFyZ2luOiAwOyB9PC9zdHlsZT48c2NyaXB0|
      && |IGlkPV9faWVfb25sb2FkIGRlZmVyICIrKCJodHRwczoiPT1sb2NhdGlvbi5wcm90b2NvbD8ic3JjPSdqYXZhc2NyaXB0OnZvaWQoMCknIjoic3JjPS8vMCIp|
      && |KyI+PFwvc2NyaXB0PiIpLCRCSygiX19pZV9vbmxvYWQiKS5vbnJlYWR5c3RhdGVjaGFuZ2U9ZnVuY3Rpb24oKXsiY29tcGxldGUiPT10aGlzLnJlYWR5U3Rh|
      && |dGUmJmJrTGliLmRvbUxvYWRlZCgpfSksd2luZG93Lm9ubG9hZD1ia0xpYi5kb21Mb2FkZWR9fTtmdW5jdGlvbiAkQksodCl7cmV0dXJuInN0cmluZyI9PXR5|
      && |cGVvZiB0JiYodD1kb2N1bWVudC5nZXRFbGVtZW50QnlJZCh0KSksdCYmIXQuYXBwZW5kVG8/YmtFeHRlbmQodCxia0VsZW1lbnQucHJvdG90eXBlKTp0fXZh|
      && |ciBia0V2ZW50PXthZGRFdmVudDpmdW5jdGlvbih0LGUpe3JldHVybiBlJiYodGhpcy5ldmVudExpc3Q9dGhpcy5ldmVudExpc3R8fHt9LHRoaXMuZXZlbnRM|
      && |aXN0W3RdPXRoaXMuZXZlbnRMaXN0W3RdfHxbXSx0aGlzLmV2ZW50TGlzdFt0XS5wdXNoKGUpKSx0aGlzfSxmaXJlRXZlbnQ6ZnVuY3Rpb24oKXt2YXIgdD1i|
      && |a0xpYi50b0FycmF5KGFyZ3VtZW50cyksZT10LnNoaWZ0KCk7aWYodGhpcy5ldmVudExpc3QmJnRoaXMuZXZlbnRMaXN0W2VdKWZvcih2YXIgbj0wO248dGhp|
      && |cy5ldmVudExpc3RbZV0ubGVuZ3RoO24rKyl0aGlzLmV2ZW50TGlzdFtlXVtuXS5hcHBseSh0aGlzLHQpfX07ZnVuY3Rpb24gX18odCl7cmV0dXJuIHR9RnVu|
      && |Y3Rpb24ucHJvdG90eXBlLmNsb3N1cmU9ZnVuY3Rpb24oKXt2YXIgdD10aGlzLGU9YmtMaWIudG9BcnJheShhcmd1bWVudHMpLG49ZS5zaGlmdCgpO3JldHVy|
      && |biBmdW5jdGlvbigpe2lmKHZvaWQgMCE9PWJrTGliKXJldHVybiB0LmFwcGx5KG4sZS5jb25jYXQoYmtMaWIudG9BcnJheShhcmd1bWVudHMpKSl9fSxGdW5j|
      && |dGlvbi5wcm90b3R5cGUuY2xvc3VyZUxpc3RlbmVyPWZ1bmN0aW9uKCl7dmFyIHQ9dGhpcyxlPWJrTGliLnRvQXJyYXkoYXJndW1lbnRzKSxuPWUuc2hpZnQo|
      && |KTtyZXR1cm4gZnVuY3Rpb24oaSl7aWYoKGk9aXx8d2luZG93LmV2ZW50KS50YXJnZXQpdmFyIHM9aS50YXJnZXQ7ZWxzZSBzPWkuc3JjRWxlbWVudDtyZXR1|
      && |cm4gdC5hcHBseShuLFtpLHNdLmNvbmNhdChlKSl9fTt2YXIgbmljRWRpdG9yQ29uZmlnPWJrQ2xhc3MuZXh0ZW5kKHtidXR0b25zOntib2xkOntuYW1lOl9f|
      && |KCJDbGljayB0byBCb2xkIiksY29tbWFuZDoiQm9sZCIsdGFnczpbIkIiLCJTVFJPTkciXSxjc3M6eyJmb250LXdlaWdodCI6ImJvbGQifSxrZXk6ImIifSxp|
      && |dGFsaWM6e25hbWU6X18oIkNsaWNrIHRvIEl0YWxpYyIpLGNvbW1hbmQ6Ikl0YWxpYyIsdGFnczpbIkVNIiwiSSJdLGNzczp7ImZvbnQtc3R5bGUiOiJpdGFs|
      && |aWMifSxrZXk6ImkifSx1bmRlcmxpbmU6e25hbWU6X18oIkNsaWNrIHRvIFVuZGVybGluZSIpLGNvbW1hbmQ6IlVuZGVybGluZSIsdGFnczpbIlUiXSxjc3M6|
      && |eyJ0ZXh0LWRlY29yYXRpb24iOiJ1bmRlcmxpbmUifSxrZXk6InUifSxsZWZ0OntuYW1lOl9fKCJMZWZ0IEFsaWduIiksY29tbWFuZDoianVzdGlmeWxlZnQi|
      && |LG5vQWN0aXZlOiEwfSxjZW50ZXI6e25hbWU6X18oIkNlbnRlciBBbGlnbiIpLGNvbW1hbmQ6Imp1c3RpZnljZW50ZXIiLG5vQWN0aXZlOiEwfSxyaWdodDp7|
      && |bmFtZTpfXygiUmlnaHQgQWxpZ24iKSxjb21tYW5kOiJqdXN0aWZ5cmlnaHQiLG5vQWN0aXZlOiEwfSxqdXN0aWZ5OntuYW1lOl9fKCJKdXN0aWZ5IEFsaWdu|
      && |IiksY29tbWFuZDoianVzdGlmeWZ1bGwiLG5vQWN0aXZlOiEwfSxvbDp7bmFtZTpfXygiSW5zZXJ0IE9yZGVyZWQgTGlzdCIpLGNvbW1hbmQ6Imluc2VydG9y|
      && |ZGVyZWRsaXN0Iix0YWdzOlsiT0wiXX0sdWw6e25hbWU6X18oIkluc2VydCBVbm9yZGVyZWQgTGlzdCIpLGNvbW1hbmQ6Imluc2VydHVub3JkZXJlZGxpc3Qi|
      && |LHRhZ3M6WyJVTCJdfSxzdWJzY3JpcHQ6e25hbWU6X18oIkNsaWNrIHRvIFN1YnNjcmlwdCIpLGNvbW1hbmQ6InN1YnNjcmlwdCIsdGFnczpbIlNVQiJdfSxz|
      && |dXBlcnNjcmlwdDp7bmFtZTpfXygiQ2xpY2sgdG8gU3VwZXJzY3JpcHQiKSxjb21tYW5kOiJzdXBlcnNjcmlwdCIsdGFnczpbIlNVUCJdfSxzdHJpa2V0aHJv|
      && |dWdoOntuYW1lOl9fKCJDbGljayB0byBTdHJpa2UgVGhyb3VnaCIpLGNvbW1hbmQ6InN0cmlrZVRocm91Z2giLGNzczp7InRleHQtZGVjb3JhdGlvbiI6Imxp|
      && |bmUtdGhyb3VnaCJ9fSxyZW1vdmVmb3JtYXQ6e25hbWU6X18oIlJlbW92ZSBGb3JtYXR0aW5nIiksY29tbWFuZDoicmVtb3ZlZm9ybWF0Iixub0FjdGl2ZToh|
      && |MH0saW5kZW50OntuYW1lOl9fKCJJbmRlbnQgVGV4dCIpLGNvbW1hbmQ6ImluZGVudCIsbm9BY3RpdmU6ITB9LG91dGRlbnQ6e25hbWU6X18oIlJlbW92ZSBJ|
      && |bmRlbnQiKSxjb21tYW5kOiJvdXRkZW50Iixub0FjdGl2ZTohMH0saHI6e25hbWU6X18oIkhvcml6b250YWwgUnVsZSIpLGNvbW1hbmQ6Imluc2VydEhvcml6|
      && |b250YWxSdWxlIixub0FjdGl2ZTohMH19LGljb25zUGF0aDoiZGF0YTppbWFnZS9naWY7YmFzZTY0LFIwbEdPRGxoMUFFU0FQY0FBQUFBQUJFUkVTSWlJak16|
      && |TXpObUluZFZJa1IzSWtSRVJGVlZWV1ptWm5kM2Q5MGlJdTR6TTd0RUVidFZJcXAzTTkxbUFPNTNFY3htTTd0M1JJaG1kKzVFUk81RVZlNVZWZjlWVmY5bVp2|
      && |OW1kKzUzZC85M2QxV0lJbFdJUkdhSVZYZTdSR2FJWnJ1SU03dVpNLytxTS8vTU00aUlSS3FJVllpN1JJaXFWWm03VmJ1cVZabVpkNGk3WnN5WlZkMlpWZTZa|
      && |VmN5cVJNeVpadDJaWnQycVpzeXFkOTJxZDh5N2QrNjdacG5NWnQzTVZlN01adTdkWnYvZFp1N01keEV6dXhGRXV4RlZ1ek5tcWpObXV6TjN1MFIzdXlKbXpE|
      && |TjN6RVIzekpsM2lQOTNpR2FabVVTWnFsV0l1MmFJcWxXSXpFU0kzVldJM1ZXWjNXYUl6R2FaekhlWnpHYUkzV2FaM1hlWjNWV3EzV2FxM1hlcTNWV3E3bWFx|
      && |N25lcTdtYTcvNGlJaUptWm1abXFpTHVxbVlpcXFxcXFxcnU3dS8rSWlOMnFpTXk3aU4yN2lNeTdtZDI3bWU2cWlPNnFtZTY3bWYrN3U1bk1pTHZNbWJ2ZHU5|
      && |M01pTzdNaU4zTXF1N01xdi9NdSs3ZHU4enV1Ly91cW9pWjNZaXF6Sm1xeklpcTNabTczYXE3M1lpcTdvaTc3cG03N3FxNzdydk0zWWpNN3BuZC82ck03cnZN|
      && |N3J2ZDdzek16TXpNM2N6ZDNkM2QzZi9Nek83ZHpPN2QzZi9kM2YvL3pPN3UzZi91M2N6ZDdzemQvOTNkLzkzdTd0M3UvKzd1N3YvdTd1N3UvKzcvLy8vLy93|
      && |QUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFB|
      && |QUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFB|
      && |QUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFB|
      && |QUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFBQUFB|
      && |QUFBQUFBQUFBQUFBQUFBQUFBQUNINUJBRUFBS0VBTEFBQUFBRFVBUklBQUFqK0FFTUpIRWl3b01HRENCTXFYTWl3b2NPSEVDTktuRWl4b3NXTEdETnEzTWl4|
      && |bzBlTGhyWlFlVEp5Sk1rK29CaFdlbE9qeHB0S0gyUEtuRW5USUtoRGd3N2hITFFwWTZBL1U0b01HVXAwYUpFcGltb3FwZGdva0orblVLUDZhYlRVWVVxQ1A0|
      && |TVdMWG8wNmRJdG5zS0tEYXRKNU5XRGxkVFlVSFBuamhvWk12SlVuVXQzb2FkQ1NoY2x1blNKcDhZL2ZFR0IraVQyRStGTmlxZ2txc3Y0WUNCT1l5Tjc0dVNI|
      && |Y2FkSkJpZkJnVVFROENYQmhNTWE5b1JZOFZJcW5qQ3A1bnRwRWFoRklqdmRvRlB3am93M2VYcVVLSkhuemhzWmJPZ3FHSzRnUVFJRUNBNGNHQ0Nnc2N4Q2VH|
      && |c2VTcFJ6eTltTFRTNXRNVHhGTENqU2lCWCtGWEhPY0ZHaFJSRVpnVkZRWm1BWUJHWUsrdkgwbEV5YyszR1dMUEZET1NNb1FwdUE0a1VvbVhoQkNFS2RjTUFC|
      && |WmdOTmtrRUdaekFZU25iYmZkSmRXTjl0RXQ1NEV3bmk0WWVDYkxIRkZWZElFUVVVU0J3aDBCT3BxYmFhYTVoczBrY2FlNHd3MEVwbzRFQUNDU1hvaGdZYWJV|
      && |MEFFMEdNaERHY0dXRWtFQVluQ0JGbm5ISElKWGNBUms1Q0dhVnlDNW54cEFKTkR2ZmtjY2hoV1ZDVlZrcHBVSFNNa0ltY1FXcGVPYVZCaVN3eXlDV0hiS0Zk|
      && |RjFya3VRVkJaSUpwNWtCRmFFZlloWU9CWitjUUIzRlNoaGxtbEFGR1FtRkVDZ1luNjNFWmtTZUREQkxkUUlVb2dwY1piU0prSkVHY1dFclFmSURZRjBjT3JK|
      && |S3hYMy8rR1NraVNDWUFlaUVJZ2h3b29jU0NBam1vZ1JJWndIRlZvTnQ1UXVoaG14eEtrU0NLTkt0SUl0QkdTd2doSGhxeElpZ3h1dGlhSnA1VVFzY2VNZHd3|
      && |a0Z0NWtLQWpEaVhnb0FJSUtOelJra0ZhUGhyS2V3bG9WQnlZeWpFblFBQUJlS1FBZkkxQnQrbEhtMHgzU2NGYndKWkZGMTFnY2VCRVEyZ1hWbmVqYWJnSkZv|
      && |a2d5bVo4b1pTQlFFSnBKdENlbzB4R05NaGR0eEprSG5vUkpUbW1RYWpheDJvT0xlU2dINnlrbWdGR0dHQThRaW5QWnBSOGtLeWdDRElnUXFBb2VBWUhHVXpp|
      && |SzlNVm5CVXhXTVo2VXJHR0dHc3NFYlBSSmpLdGgzK0llTVVmMW9iQ29zV2J2R2pJdHp2Y2NGVWxNcmh4d1F2bS9vZ0dDRG1BZ01iK0d4UEFtNEM4SHI5NWtK|
      && |WitLbmRBZXhnOUhJcVhaUzcweUpvSjNWdDR2Z093K2FTYkhEM011SjhHRGFMWHROcHRva1VXVW1qeHgzVURUYjVjNVFOTmJiSFY0RzF5Q1JhQmFEMFFJeUlQ|
      && |Wk9wQmp5TlF4dTViSDQyUnkzekM3RW1xcTdhZ2ZBdExrRUZERFM3SUpSQW9rWEx5Q0pKaFBISjlwTmR4b2pQUGovNE1SdEEyYVpBcjB3OUNmWjNyR3NKdThl|
      && |eTFMd3VLSXQ4Uk10WWxmMmhTVmhBcnhxNGhKcGR3Qk50TzBJbUJ2T0VORzlqQUJkalNGalNzQUFVcnNNRWJIbUFRajRVdk9mTENTSmlXSTRCOUNjNGcxT3FK|
      && |bTFhM2tQZGtzQzdUOHNSRE5qR0loNDF3QUt5YkhyUzJBSzFwYllFUVdwRENJcVR3aDRwTWJRcitRS1NDRUlFSVJOb0p3U0NQT0U3MlFzR0loUVJPYUpDU0ZL|
      && |V0dFd3BDYkNFVEdnRkR2UWl5eFZNZGp3eXNXbDRLVWxDSFBGZ2lEeTRZQ0tnZUFZWURLS2s0QndERDd6Z1dDdXBsYjN2ejBoNzJVRmZIRFB4S1FSeTRRTlFL|
      && |OHNNZ0RwR0lSa1NpOXJUSGlDWWF4RHFmS1pvbi91Q0pMV3dpRVlLNUF2L001cjhZQ1hBUE94aUJFd2hTZ3p1ZzRRSWJzTTBFWkhBSEpyaHlCaE9zSUFKRWxn|
      && |QTNKcVJNaG9NaGN5NGlDRUlNd3BJVHFTVWRENEpMeXUyeUlNVmMzVEVKZ2ltSHNMQ0ZlMklJQ3dseENZRnNJbXhTa01JbEVpRUZMQ0JUZFREc0lFR21KcUp5|
      && |WXVHYzZBVEVFUTBDaGpBbHdKRUo0WVJ5VG9pUWtJMXNVcGYrRUlRbkRrWVFSUkRDS3hCcEp6Smh4Z2xBQUdJSllreUJEL0pReGp6UXdEMWdZQVFvRG1BR1Jq|
      && |VHFBS0JRVHhqVXFBRDF1TEdKY0pTakFvWTVFRkJjZ0FNWXdBQUhCa2xJN1pSVFJPaE01em9Kc3NoRndwTWdXNkFmSmowQkNyQmNZUk04OWNRVmdOQS90RjND|
      && |RERjQTVRZ2M4UVNDc05JM1FPcmJCTjdnU2lhOElKWUY4Wmlsd0RBQUJFQ1JJZ0lMNjdTbU5RZ1BGU1JFUFNuVG54RHlDREUxcHBtaE1NTUdWOWNjZ2lUcmFH|
      && |cDFheWh5MHNKUWFJY1FWc2hDRDdscEJSKzY5S1V4UGFjNk9Vb2NPQW9BQUpBRkFEMEZ3aDRZTmhGVG11SWRjbjRua0JHUnFDQ0VTTVN0NUNvbHVpSkVvQVQ1|
      && |bUh5T0J3Zy9tT0MxSnZEK0FTVjQ0QU1hMEVDRkF2bWRSdzAzenpRaGJsNFJWWUJnVWlLWVIzUjBad2VaeEVsVHVsSUp0ZTZ3NWt6c1lnc0NDcHRLOUNCWG1G|
      && |OVlmT29KS1NDR1cxSWdLaWN0NW9neGpHRUVNWWlCR1R6UlZBUEtvQVlNbE1FRGJzTUNKckJBRFRXZ1lGWVJZQ2xPZEhDakJ3bWNNanY0VzA0NXExblJLc1JZ|
      && |b3prUTRSbXpnM1VWMVFHQVo1QzU2aExDL1NxSWhjT0pFRUdFTFNKSGU3QTRGWktJTEdnaENRNW9nQU1vd01kUWlIaGY0MVRFUzdlUTJIUCtZYVlERVZvWmx2|
      && |TkJVc2t4RkZ3ZEFDTk9Wb2lVeWZJQVg0V1RJQmJUTW9yZWJyTDhHV01LVE9BQ0Y4aVdCN1Ixd1pBRWtzUXdsS0VNUmxKQUdNVHdnUTk0UUF3RDBXMytHN0hV|
      && |MWpqNk5qTVh5SUNDTktBQkRGVEF1YUVZZ293UlcrTWJIeVNqald4eEtLSUFDa29LdGJ1ZWlJSW1vUENaVGZ4Z1JaclFrQ0g2WUlaSW1PRUVZL2dNRVFxeWto|
      && |cE13Q1crc2NFTFhvQUdOVXpBRGhXMHBVQTZPRm1KS0hpc1pRM2JqRGVDZ0FHUWxEdzF1WVNKa2lDQk51Q2hEYjJtaUo2dE05eGlDNFlLZmk1SW93Z2lnQmlP|
      && |NlcrOVUwNFRqWVlRZWNZeEp2TDBLaE1QOEFpQ3p1SGJjNmpCbGJOTUJ1b21NUUZIWWtRSVdpQUhQYlNnQTJnT1JaZS9QQ294MHpzQjNTYUljdVVzeUFwWUlB|
      && |TVdZSUJ6aDIxc1l5TWJ4d1RKcUtCREFZVVFrYWhFVVRnUmlsSVVoVWR6a2dzaTZrTWZudUFFTG5nY0ZKdEd5MVQrWnpEcXE2TGgwd2NKd3dGVUc0WU81cnNn|
      && |dVF4bkIvbVZZWXZFWEYvNzRoZklscG1RbTgrY1h3QXdpTTkvWHZQT2lnZ2lRODk1MFEyQ0JSNzJHZ1lSaUFBTUhFQ1FvZE04NksxVEJCWkFzYnl1dCtBVEI2|
      && |L2dBSVFtZ0ZZcnhJcFlQRWhsaFl6dGYvSDNwZ1BoRDdqWFVJUFpabGtTbFdHSUIrUWdCNjYzZ0FEVE8vZVJGc2U0UjF4bjM0R01HaWdZVUlFTVZFRGdXZCs2|
      && |MTVjSDltUkw1QWdlTHVmRHN4bnhpRnVjQ0pyNGdoTkdQL290SUFJUlh3QjVRdUR5QmduKzVnSEJnVmR4RUxBZS9yNGMxdzVKRW5LNjJCaGcwdVFTaEloNElo|
      && |cUFoK0ZHb0FIQ2xqRW93TTM4T1lDOUNnanYyTXE5UENxSTVIT2ZQZkZ4ZTRMK0RIZUNGR0lMQStQSVk4SkM5elhvSU11VndKbENETkIzd2NnQjhFemYraFVB|
      && |YWhCSXhObk9WMW04dnhuQUdZRVF2UG5nOW56UjV4QkdVSUJCY0lCQWtJQkE4QU1NMklBQ0FYcGZFSUVTT0lHcWh4Q1Y4QUEvSWw5Q29oRmxnSE5LQjJBWFFW|
      && |Y1l0blEySjNOS0YxbENaNEk1bHhESjBqSDZBblNRbFlJZXFITUswWGxSWUFVT0FIVlJCd0hJWnhBakdBQW9HSGwrTjNrV0FuMGU0VmxYWURrOXMwSGQxMW0v|
      && |NUlJbkdJTUhNWUpCU0JCTjhSUWlZQWt1SUFKYkNCVlV3UkFHb0R4K0IzOEVnUVZYa0FsOGNBbFhFSDRFQVFrV3dGTFRzd0Q4RjJPU04zbGZOd1ZHcUJRc29q|
      && |OThPQmFDMFY0SXdRWVA4R2xiaG51R09CT2hJcUk0U3dGc085Z0FiVUFSUlFBdEJWZHNWVkFGSEhLSUJmRjliT2dSQlZBSGRTQUNJaUFKRVNFR0hjQnU3eFp2|
      && |QThFSFYxQk5WK0FKbWJCMUNRRUpmQVFLL1Fjb2tqaUpnbEdKbDBnVGhrQUZSUENMd0JpTVQyQUltRmlNeHJpSUtlWUFqMGdSZjRBRlFqRVVRaENOMGlpTlRY|
      && |Q01kVkVBQmVBQ05GQ0lEeUVHQkVBQUJvQ0tOZEdNenpnRTAzaU8xV2lONnJpTzdOaU83dmlPOENnUkFRRUFPdz09IixidXR0b25MaXN0Olsic2F2ZSIsImJv|
      && |bGQiLCJpdGFsaWMiLCJ1bmRlcmxpbmUiLCJsZWZ0IiwiY2VudGVyIiwicmlnaHQiLCJqdXN0aWZ5Iiwib2wiLCJ1bCIsImZvbnRTaXplIiwiZm9udEZhbWls|
      && |eSIsImZvbnRGb3JtYXQiLCJpbmRlbnQiLCJvdXRkZW50IiwiaW1hZ2UiLCJ1cGxvYWQiLCJsaW5rIiwidW5saW5rIiwiZm9yZWNvbG9yIiwiYmdjb2xvciJd|
      && |LGljb25MaXN0Ont4aHRtbDoxLGJnY29sb3I6Mixmb3JlY29sb3I6Myxib2xkOjQsY2VudGVyOjUsaHI6NixpbmRlbnQ6NyxpdGFsaWM6OCxqdXN0aWZ5Ojks|
      && |bGVmdDoxMCxvbDoxMSxvdXRkZW50OjEyLHJlbW92ZWZvcm1hdDoxMyxyaWdodDoxNCxzYXZlOjI1LHN0cmlrZXRocm91Z2g6MTYsc3Vic2NyaXB0OjE3LHN1|
      && |cGVyc2NyaXB0OjE4LHVsOjE5LHVuZGVybGluZToyMCxpbWFnZToyMSxsaW5rOjIyLHVubGluazoyMyxjbG9zZToyNCxhcnJvdzoyNn19KSxuaWNFZGl0b3Jz|
      && |PXtuaWNQbHVnaW5zOltdLGVkaXRvcnM6W10scmVnaXN0ZXJQbHVnaW46ZnVuY3Rpb24odCxlKXt0aGlzLm5pY1BsdWdpbnMucHVzaCh7cDp0LG86ZX0pfSxh|
      && |bGxUZXh0QXJlYXM6ZnVuY3Rpb24odCl7Zm9yKHZhciBlPWRvY3VtZW50LmdldEVsZW1lbnRzQnlUYWdOYW1lKCJ0ZXh0YXJlYSIpLG49MDtuPGUubGVuZ3Ro|
      && |O24rKyluaWNFZGl0b3JzLmVkaXRvcnMucHVzaChuZXcgbmljRWRpdG9yKHQpLnBhbmVsSW5zdGFuY2UoZVtuXSkpO3JldHVybiBuaWNFZGl0b3JzLmVkaXRv|
      && |cnN9LGZpbmRFZGl0b3I6ZnVuY3Rpb24odCl7Zm9yKHZhciBlPW5pY0VkaXRvcnMuZWRpdG9ycyxuPTA7bjxlLmxlbmd0aDtuKyspaWYoZVtuXS5pbnN0YW5j|
      && |ZUJ5SWQodCkpcmV0dXJuIGVbbl0uaW5zdGFuY2VCeUlkKHQpfX0sbmljRWRpdG9yPWJrQ2xhc3MuZXh0ZW5kKHtjb25zdHJ1Y3Q6ZnVuY3Rpb24odCl7dGhp|
      && |cy5vcHRpb25zPW5ldyBuaWNFZGl0b3JDb25maWcsYmtFeHRlbmQodGhpcy5vcHRpb25zLHQpLHRoaXMubmljSW5zdGFuY2VzPW5ldyBBcnJheSx0aGlzLmxv|
      && |YWRlZFBsdWdpbnM9bmV3IEFycmF5O2Zvcih2YXIgZT1uaWNFZGl0b3JzLm5pY1BsdWdpbnMsbj0wO248ZS5sZW5ndGg7bisrKXRoaXMubG9hZGVkUGx1Z2lu|
      && |cy5wdXNoKG5ldyBlW25dLnAodGhpcyxlW25dLm8pKTtuaWNFZGl0b3JzLmVkaXRvcnMucHVzaCh0aGlzKSxia0xpYi5hZGRFdmVudChkb2N1bWVudC5ib2R5|
      && |LCJtb3VzZWRvd24iLHRoaXMuc2VsZWN0Q2hlY2suY2xvc3VyZUxpc3RlbmVyKHRoaXMpKX0scGFuZWxJbnN0YW5jZTpmdW5jdGlvbih0LGUpe3Q9dGhpcy5j|
      && |aGVja1JlcGxhY2UoJEJLKHQpKTt2YXIgbj1uZXcgYmtFbGVtZW50KCJESVYiKS5zZXRTdHlsZSh7d2lkdGg6KHBhcnNlSW50KHQuZ2V0U3R5bGUoIndpZHRo|
      && |IikpfHx0LmNsaWVudFdpZHRoKSsicHgifSkuYXBwZW5kQmVmb3JlKHQpO3JldHVybiB0aGlzLnNldFBhbmVsKG4pLHRoaXMuYWRkSW5zdGFuY2UodCxlKX0s|
      && |Y2hlY2tSZXBsYWNlOmZ1bmN0aW9uKHQpe3ZhciBlPW5pY0VkaXRvcnMuZmluZEVkaXRvcih0KTtyZXR1cm4gZSYmKGUucmVtb3ZlSW5zdGFuY2UodCksZS5y|
      && |ZW1vdmVQYW5lbCgpKSx0fSxhZGRJbnN0YW5jZTpmdW5jdGlvbih0LGUpe2lmKCh0PXRoaXMuY2hlY2tSZXBsYWNlKCRCSyh0KSkpLmNvbnRlbnRFZGl0YWJs|
      && |ZXx8d2luZG93Lm9wZXJhKXZhciBuPW5ldyBuaWNFZGl0b3JJbnN0YW5jZSh0LGUsdGhpcyk7ZWxzZSBuPW5ldyBuaWNFZGl0b3JJRnJhbWVJbnN0YW5jZSh0|
      && |LGUsdGhpcyk7cmV0dXJuIHRoaXMubmljSW5zdGFuY2VzLnB1c2gobiksdGhpc30scmVtb3ZlSW5zdGFuY2U6ZnVuY3Rpb24odCl7dD0kQksodCk7Zm9yKHZh|
      && |ciBlPXRoaXMubmljSW5zdGFuY2VzLG49MDtuPGUubGVuZ3RoO24rKyllW25dLmU9PXQmJihlW25dLnJlbW92ZSgpLHRoaXMubmljSW5zdGFuY2VzLnNwbGlj|
      && |ZShuLDEpKX0scmVtb3ZlUGFuZWw6ZnVuY3Rpb24odCl7dGhpcy5uaWNQYW5lbCYmKHRoaXMubmljUGFuZWwucmVtb3ZlKCksdGhpcy5uaWNQYW5lbD1udWxs|
      && |KX0saW5zdGFuY2VCeUlkOmZ1bmN0aW9uKHQpe3Q9JEJLKHQpO2Zvcih2YXIgZT10aGlzLm5pY0luc3RhbmNlcyxuPTA7bjxlLmxlbmd0aDtuKyspaWYoZVtu|
      && |XS5lPT10KXJldHVybiBlW25dfSxzZXRQYW5lbDpmdW5jdGlvbih0KXtyZXR1cm4gdGhpcy5uaWNQYW5lbD1uZXcgbmljRWRpdG9yUGFuZWwoJEJLKHQpLHRo|
      && |aXMub3B0aW9ucyx0aGlzKSx0aGlzLmZpcmVFdmVudCgicGFuZWwiLHRoaXMubmljUGFuZWwpLHRoaXN9LG5pY0NvbW1hbmQ6ZnVuY3Rpb24odCxlKXt0aGlz|
      && |LnNlbGVjdGVkSW5zdGFuY2UmJnRoaXMuc2VsZWN0ZWRJbnN0YW5jZS5uaWNDb21tYW5kKHQsZSl9LGdldEljb246ZnVuY3Rpb24odCxlKXt2YXIgbj10aGlz|
      && |Lm9wdGlvbnMuaWNvbkxpc3RbdF0saT1lLmljb25GaWxlcz9lLmljb25GaWxlc1t0XToiIjtyZXR1cm57YmFja2dyb3VuZEltYWdlOiJ1cmwoJyIrKG4/dGhp|
      && |cy5vcHRpb25zLmljb25zUGF0aDppKSsiJykiLGJhY2tncm91bmRQb3NpdGlvbjoobj8tMTgqKG4tMSk6MCkrInB4IDBweCJ9fSxzZWxlY3RDaGVjazpmdW5j|
      && |dGlvbih0LGUpe2Rve2lmKGUuY2xhc3NOYW1lJiYtMSE9ZS5jbGFzc05hbWUuaW5kZXhPZigibmljRWRpdCIpKXJldHVybiExfXdoaWxlKGU9ZS5wYXJlbnRO|
      && |b2RlKTtyZXR1cm4gdGhpcy5maXJlRXZlbnQoImJsdXIiLHRoaXMuc2VsZWN0ZWRJbnN0YW5jZSxlKSx0aGlzLmxhc3RTZWxlY3RlZEluc3RhbmNlPXRoaXMu|
      && |c2VsZWN0ZWRJbnN0YW5jZSx0aGlzLnNlbGVjdGVkSW5zdGFuY2U9bnVsbCwhMX19KTtuaWNFZGl0b3I9bmljRWRpdG9yLmV4dGVuZChia0V2ZW50KTt2YXIg|
      && |bmljRWRpdG9ySW5zdGFuY2U9YmtDbGFzcy5leHRlbmQoe2lzU2VsZWN0ZWQ6ITEsY29uc3RydWN0OmZ1bmN0aW9uKHQsZSxuKXt0aGlzLm5lPW4sdGhpcy5l|
      && |bG09dGhpcy5lPXQsdGhpcy5vcHRpb25zPWV8fHt9LG5ld1g9cGFyc2VJbnQodC5nZXRTdHlsZSgid2lkdGgiKSl8fHQuY2xpZW50V2lkdGgsbmV3WT1wYXJz|
      && |ZUludCh0LmdldFN0eWxlKCJoZWlnaHQiKSl8fHQuY2xpZW50SGVpZ2h0LHRoaXMuaW5pdGlhbEhlaWdodD1uZXdZLTg7dmFyIGk9InRleHRhcmVhIj09dC5u|
      && |b2RlTmFtZS50b0xvd2VyQ2FzZSgpO2lmKGl8fHRoaXMub3B0aW9ucy5oYXNQYW5lbCl7dmFyIHM9YmtMaWIuaXNNU0lFJiYhKHZvaWQgMCE9PWRvY3VtZW50|
      && |LmJvZHkuc3R5bGUubWF4SGVpZ2h0JiYiQ1NTMUNvbXBhdCI9PWRvY3VtZW50LmNvbXBhdE1vZGUpLG89e3dpZHRoOm5ld1grInB4Iixib3JkZXI6IjFweCBz|
      && |b2xpZCAjY2NjIixib3JkZXJUb3A6MCxvdmVyZmxvd1k6ImF1dG8iLG92ZXJmbG93WDoiaGlkZGVuIn07b1tzPyJoZWlnaHQiOiJtYXhIZWlnaHQiXT10aGlz|
      && |Lm5lLm9wdGlvbnMubWF4SGVpZ2h0P3RoaXMubmUub3B0aW9ucy5tYXhIZWlnaHQrInB4IjpudWxsLHRoaXMuZWRpdG9yQ29udGFpbj1uZXcgYmtFbGVtZW50|
      && |KCJESVYiKS5zZXRTdHlsZShvKS5hcHBlbmRCZWZvcmUodCk7dmFyIGE9bmV3IGJrRWxlbWVudCgiRElWIikuc2V0U3R5bGUoe3dpZHRoOm5ld1gtOCsicHgi|
      && |LG1hcmdpbjoiNHB4IixtaW5IZWlnaHQ6bmV3WSsicHgifSkuYWRkQ2xhc3MoIm1haW4iKS5hcHBlbmRUbyh0aGlzLmVkaXRvckNvbnRhaW4pO2lmKHQuc2V0|
      && |U3R5bGUoe2Rpc3BsYXk6Im5vbmUifSksYS5pbm5lckhUTUw9dC5pbm5lckhUTUwsaSl7YS5zZXRDb250ZW50KHQudmFsdWUpLHRoaXMuY29weUVsbT10O3Zh|
      && |ciByPXQucGFyZW50VGFnKCJGT1JNIik7ciYmYmtMaWIuYWRkRXZlbnQociwic3VibWl0Iix0aGlzLnNhdmVDb250ZW50LmNsb3N1cmUodGhpcykpfWEuc2V0|
      && |U3R5bGUocz97aGVpZ2h0Om5ld1krInB4In06e292ZXJmbG93OiJoaWRkZW4ifSksdGhpcy5lbG09YX10aGlzLm5lLmFkZEV2ZW50KCJibHVyIix0aGlzLmJs|
      && |dXIuY2xvc3VyZSh0aGlzKSksdGhpcy5pbml0KCksdGhpcy5ibHVyKCl9LGluaXQ6ZnVuY3Rpb24oKXt0aGlzLmVsbS5zZXRBdHRyaWJ1dGUoImNvbnRlbnRF|
      && |ZGl0YWJsZSIsInRydWUiKSx0aGlzLmVsbS5zZXRBdHRyaWJ1dGUoImlkIiwibWFpbl9lZGl0b3IiKSwiIj09dGhpcy5nZXRDb250ZW50KCkmJnRoaXMuc2V0|
      && |Q29udGVudCgiPGJyIC8+IiksdGhpcy5pbnN0YW5jZURvYz1kb2N1bWVudC5kZWZhdWx0Vmlldyx0aGlzLmVsbS5hZGRFdmVudCgibW91c2Vkb3duIix0aGlz|
      && |LnNlbGVjdGVkLmNsb3N1cmVMaXN0ZW5lcih0aGlzKSkuYWRkRXZlbnQoImtleXByZXNzIix0aGlzLmtleURvd24uY2xvc3VyZUxpc3RlbmVyKHRoaXMpKS5h|
      && |ZGRFdmVudCgiZm9jdXMiLHRoaXMuc2VsZWN0ZWQuY2xvc3VyZSh0aGlzKSkuYWRkRXZlbnQoImJsdXIiLHRoaXMuYmx1ci5jbG9zdXJlKHRoaXMpKS5hZGRF|
      && |dmVudCgia2V5dXAiLHRoaXMuc2VsZWN0ZWQuY2xvc3VyZSh0aGlzKSksdGhpcy5uZS5maXJlRXZlbnQoImFkZCIsdGhpcyl9LHJlbW92ZTpmdW5jdGlvbigp|
      && |e3RoaXMuc2F2ZUNvbnRlbnQoKSwodGhpcy5jb3B5RWxtfHx0aGlzLm9wdGlvbnMuaGFzUGFuZWwpJiYodGhpcy5lZGl0b3JDb250YWluLnJlbW92ZSgpLHRo|
      && |aXMuZS5zZXRTdHlsZSh7ZGlzcGxheToiYmxvY2sifSksdGhpcy5uZS5yZW1vdmVQYW5lbCgpKSx0aGlzLmRpc2FibGUoKSx0aGlzLm5lLmZpcmVFdmVudCgi|
      && |cmVtb3ZlIix0aGlzKX0sZGlzYWJsZTpmdW5jdGlvbigpe3RoaXMuZWxtLnNldEF0dHJpYnV0ZSgiY29udGVudEVkaXRhYmxlIiwiZmFsc2UiKX0sZ2V0U2Vs|
      && |OmZ1bmN0aW9uKCl7cmV0dXJuIHdpbmRvdy5nZXRTZWxlY3Rpb24/d2luZG93LmdldFNlbGVjdGlvbigpOmRvY3VtZW50LnNlbGVjdGlvbn0sZ2V0Um5nOmZ1|
      && |bmN0aW9uKCl7dmFyIHQ9dGhpcy5nZXRTZWwoKTtpZih0JiYwIT09dC5yYW5nZUNvdW50KXJldHVybiB0LnJhbmdlQ291bnQ+MD90LmdldFJhbmdlQXQoMCk6|
      && |dC5jcmVhdGVSYW5nZSgpfSxzZWxSbmc6ZnVuY3Rpb24odCxlKXt3aW5kb3cuZ2V0U2VsZWN0aW9uPyhlLnJlbW92ZUFsbFJhbmdlcygpLGUuYWRkUmFuZ2Uo|
      && |dCkpOnQuc2VsZWN0KCl9LHNlbEVsbTpmdW5jdGlvbigpe3ZhciB0PXRoaXMuZ2V0Um5nKCk7aWYodCl7aWYodC5zdGFydENvbnRhaW5lcil7dmFyIGU9dC5z|
      && |dGFydENvbnRhaW5lcjtpZigxPT10LmNsb25lQ29udGVudHMoKS5jaGlsZE5vZGVzLmxlbmd0aClmb3IodmFyIG49MDtuPGUuY2hpbGROb2Rlcy5sZW5ndGg7|
      && |bisrKXt2YXIgaT1lLmNoaWxkTm9kZXNbbl0ub3duZXJEb2N1bWVudC5jcmVhdGVSYW5nZSgpO2lmKGkuc2VsZWN0Tm9kZShlLmNoaWxkTm9kZXNbbl0pLDEh|
      && |PXQuY29tcGFyZUJvdW5kYXJ5UG9pbnRzKFJhbmdlLlNUQVJUX1RPX1NUQVJULGkpJiYtMSE9dC5jb21wYXJlQm91bmRhcnlQb2ludHMoUmFuZ2UuRU5EX1RP|
      && |X0VORCxpKSlyZXR1cm4gJEJLKGUuY2hpbGROb2Rlc1tuXSl9cmV0dXJuICRCSyhlKX1yZXR1cm4gJEJLKCJDb250cm9sIj09dGhpcy5nZXRTZWwoKS50eXBl|
      && |P3QuaXRlbSgwKTp0LnBhcmVudEVsZW1lbnQoKSl9fSxzYXZlUm5nOmZ1bmN0aW9uKCl7dGhpcy5zYXZlZFJhbmdlPXRoaXMuZ2V0Um5nKCksdGhpcy5zYXZl|
      && |ZFNlbD10aGlzLmdldFNlbCgpfSxyZXN0b3JlUm5nOmZ1bmN0aW9uKCl7dGhpcy5zYXZlZFJhbmdlJiZ0aGlzLnNlbFJuZyh0aGlzLnNhdmVkUmFuZ2UsdGhp|
      && |cy5zYXZlZFNlbCl9LGtleURvd246ZnVuY3Rpb24odCxlKXt0LmN0cmxLZXkmJnRoaXMubmUuZmlyZUV2ZW50KCJrZXkiLHRoaXMsdCl9LHNlbGVjdGVkOmZ1|
      && |bmN0aW9uKHQsZSl7aWYoZXx8KGU9dGhpcy5zZWxFbG0pfHwoZT10aGlzLnNlbEVsbSgpKSwhdC5jdHJsS2V5KXt2YXIgbj10aGlzLm5lLnNlbGVjdGVkSW5z|
      && |dGFuY2U7biE9dGhpcyYmKG4mJnRoaXMubmUuZmlyZUV2ZW50KCJibHVyIixuLGUpLHRoaXMubmUuc2VsZWN0ZWRJbnN0YW5jZT10aGlzLHRoaXMubmUuZmly|
      && |ZUV2ZW50KCJmb2N1cyIsbixlKSksdGhpcy5uZS5maXJlRXZlbnQoInNlbGVjdGVkIixuLGUpLHRoaXMuaXNGb2N1c2VkPSEwLHRoaXMuZWxtLmFkZENsYXNz|
      && |KCJzZWxlY3RlZCIpfXJldHVybiExfSxibHVyOmZ1bmN0aW9uKCl7dGhpcy5pc0ZvY3VzZWQ9ITEsdGhpcy5lbG0ucmVtb3ZlQ2xhc3MoInNlbGVjdGVkIil9|
      && |LHNhdmVDb250ZW50OmZ1bmN0aW9uKCl7KHRoaXMuY29weUVsbXx8dGhpcy5vcHRpb25zLmhhc1BhbmVsKSYmKHRoaXMubmUuZmlyZUV2ZW50KCJzYXZlIix0|
      && |aGlzKSx0aGlzLmNvcHlFbG0/dGhpcy5jb3B5RWxtLnZhbHVlPXRoaXMuZ2V0Q29udGVudCgpOnRoaXMuZS5pbm5lckhUTUw9dGhpcy5nZXRDb250ZW50KCkp|
      && |fSxnZXRFbG06ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5lbG19LGdldENvbnRlbnQ6ZnVuY3Rpb24oKXtyZXR1cm4gdGhpcy5jb250ZW50PXRoaXMuZ2V0RWxt|
      && |KCkuaW5uZXJIVE1MLHRoaXMubmUuZmlyZUV2ZW50KCJnZXQiLHRoaXMpLHRoaXMuY29udGVudH0sc2V0Q29udGVudDpmdW5jdGlvbih0KXt0aGlzLmNvbnRl|
      && |bnQ9dCx0aGlzLm5lLmZpcmVFdmVudCgic2V0Iix0aGlzKSx0aGlzLmVsbS5pbm5lckhUTUw9dGhpcy5jb250ZW50fSxuaWNDb21tYW5kOmZ1bmN0aW9uKHQs|
      && |ZSl7ZG9jdW1lbnQuZXhlY0NvbW1hbmQodCwhMSxlKX19KSxuaWNFZGl0b3JJRnJhbWVJbnN0YW5jZT1uaWNFZGl0b3JJbnN0YW5jZS5leHRlbmQoe3NhdmVk|
      && |U3R5bGVzOltdLGluaXQ6ZnVuY3Rpb24oKXt2YXIgdD10aGlzLmVsbS5pbm5lckhUTUwucmVwbGFjZSgvXlxzK3xccyskL2csIiIpO3RoaXMuZWxtLmlubmVy|
      && |SFRNTD0iIix0fHwodD0iPGJyIC8+IiksdGhpcy5pbml0aWFsQ29udGVudD10LHRoaXMuZWxtRnJhbWU9bmV3IGJrRWxlbWVudCgiaWZyYW1lIikuc2V0QXR0|
      && |cmlidXRlcyh7c3JjOiJqYXZhc2NyaXB0OjsiLGZyYW1lQm9yZGVyOjAsYWxsb3dUcmFuc3BhcmVuY3k6InRydWUiLHNjcm9sbGluZzoibm8ifSkuc2V0U3R5|
      && |bGUoe2hlaWdodDoiMTAwcHgiLHdpZHRoOiIxMDAlIn0pLmFkZENsYXNzKCJmcmFtZSIpLmFwcGVuZFRvKHRoaXMuZWxtKSx0aGlzLmNvcHlFbG0mJnRoaXMu|
      && |ZWxtRnJhbWUuc2V0U3R5bGUoe3dpZHRoOnRoaXMuZWxtLm9mZnNldFdpZHRoLTQrInB4In0pO2ZvcihpdG0gaW5bImZvbnQtc2l6ZSIsImZvbnQtZmFtaWx5|
      && |IiwiZm9udC13ZWlnaHQiLCJjb2xvciJdKXRoaXMuc2F2ZWRTdHlsZXNbYmtMaWIuY2FtZWxpemUoaXRtKV09dGhpcy5lbG0uZ2V0U3R5bGUoaXRtKTtzZXRU|
      && |aW1lb3V0KHRoaXMuaW5pdEZyYW1lLmNsb3N1cmUodGhpcyksNTApfSxkaXNhYmxlOmZ1bmN0aW9uKCl7dGhpcy5lbG0uaW5uZXJIVE1MPXRoaXMuZ2V0Q29u|
      && |dGVudCgpfSxpbml0RnJhbWU6ZnVuY3Rpb24oKXt2YXIgdD0kQksodGhpcy5lbG1GcmFtZS5jb250ZW50V2luZG93LmRvY3VtZW50KTt0LmRlc2lnbk1vZGU9|
      && |Im9uIix0Lm9wZW4oKTt2YXIgZT10aGlzLm5lLm9wdGlvbnMuZXh0ZXJuYWxDU1M7dC53cml0ZSgiPGh0bWw+PGhlYWQ+IisoZT8nPGxpbmsgaHJlZj0iJytl|
      && |KyciIHJlbD0ic3R5bGVzaGVldCIgdHlwZT0idGV4dC9jc3MiIC8+JzoiIikrJzwvaGVhZD48Ym9keSBpZD0ibmljRWRpdENvbnRlbnQiIHN0eWxlPSJtYXJn|
      && |aW46IDAgIWltcG9ydGFudDsgYmFja2dyb3VuZC1jb2xvcjogdHJhbnNwYXJlbnQgIWltcG9ydGFudDsiPicrdGhpcy5pbml0aWFsQ29udGVudCsiPC9ib2R5|
      && |PjwvaHRtbD4iKSx0LmNsb3NlKCksdGhpcy5mcmFtZURvYz10LHRoaXMuZnJhbWVXaW49JEJLKHRoaXMuZWxtRnJhbWUuY29udGVudFdpbmRvdyksdGhpcy5m|
      && |cmFtZUNvbnRlbnQ9JEJLKHRoaXMuZnJhbWVXaW4uZG9jdW1lbnQuYm9keSkuc2V0U3R5bGUodGhpcy5zYXZlZFN0eWxlcyksdGhpcy5pbnN0YW5jZURvYz10|
      && |aGlzLmZyYW1lV2luLmRvY3VtZW50LmRlZmF1bHRWaWV3LHRoaXMuaGVpZ2h0VXBkYXRlKCksdGhpcy5mcmFtZURvYy5hZGRFdmVudCgibW91c2Vkb3duIix0|
      && |aGlzLnNlbGVjdGVkLmNsb3N1cmVMaXN0ZW5lcih0aGlzKSkuYWRkRXZlbnQoImtleXVwIix0aGlzLmhlaWdodFVwZGF0ZS5jbG9zdXJlTGlzdGVuZXIodGhp|
      && |cykpLmFkZEV2ZW50KCJrZXlkb3duIix0aGlzLmtleURvd24uY2xvc3VyZUxpc3RlbmVyKHRoaXMpKS5hZGRFdmVudCgia2V5dXAiLHRoaXMuc2VsZWN0ZWQu|
      && |Y2xvc3VyZSh0aGlzKSksdGhpcy5uZS5maXJlRXZlbnQoImFkZCIsdGhpcyl9LGdldEVsbTpmdW5jdGlvbigpe3JldHVybiB0aGlzLmZyYW1lQ29udGVudH0s|
      && |c2V0Q29udGVudDpmdW5jdGlvbih0KXt0aGlzLmNvbnRlbnQ9dCx0aGlzLm5lLmZpcmVFdmVudCgic2V0Iix0aGlzKSx0aGlzLmZyYW1lQ29udGVudC5pbm5l|
      && |ckhUTUw9dGhpcy5jb250ZW50LHRoaXMuaGVpZ2h0VXBkYXRlKCl9LGdldFNlbDpmdW5jdGlvbigpe3JldHVybiB0aGlzLmZyYW1lV2luP3RoaXMuZnJhbWVX|
      && |aW4uZ2V0U2VsZWN0aW9uKCk6dGhpcy5mcmFtZURvYy5zZWxlY3Rpb259LGhlaWdodFVwZGF0ZTpmdW5jdGlvbigpe3RoaXMuZWxtRnJhbWUuc3R5bGUuaGVp|
      && |Z2h0PU1hdGgubWF4KHRoaXMuZnJhbWVDb250ZW50Lm9mZnNldEhlaWdodCx0aGlzLmluaXRpYWxIZWlnaHQpKyJweCJ9LG5pY0NvbW1hbmQ6ZnVuY3Rpb24o|
      && |dCxlKXt0aGlzLmZyYW1lRG9jLmV4ZWNDb21tYW5kKHQsITEsZSksc2V0VGltZW91dCh0aGlzLmhlaWdodFVwZGF0ZS5jbG9zdXJlKHRoaXMpLDEwMCl9fSks|
      && |bmljRWRpdG9yUGFuZWw9YmtDbGFzcy5leHRlbmQoe2NvbnN0cnVjdDpmdW5jdGlvbih0LGUsbil7dGhpcy5lbG09dCx0aGlzLm9wdGlvbnM9ZSx0aGlzLm5l|
      && |PW4sdGhpcy5wYW5lbEJ1dHRvbnM9bmV3IEFycmF5LHRoaXMuYnV0dG9uTGlzdD1ia0V4dGVuZChbXSx0aGlzLm5lLm9wdGlvbnMuYnV0dG9uTGlzdCksdGhp|
      && |cy5wYW5lbENvbnRhaW49bmV3IGJrRWxlbWVudCgiRElWIikuc2V0U3R5bGUoe292ZXJmbG93OiJoaWRkZW4iLHdpZHRoOiIxMDAlIixib3JkZXI6IjFweCBz|
      && |b2xpZCAjY2NjY2NjIixiYWNrZ3JvdW5kQ29sb3I6IiNlZmVmZWYifSkuYWRkQ2xhc3MoInBhbmVsQ29udGFpbiIpLHRoaXMucGFuZWxFbG09bmV3IGJrRWxl|
      && |bWVudCgiRElWIikuc2V0U3R5bGUoe21hcmdpbjoiMnB4IixtYXJnaW5Ub3A6IjBweCIsem9vbToxLG92ZXJmbG93OiJoaWRkZW4ifSkuYWRkQ2xhc3MoInBh|
      && |bmVsIikuYXBwZW5kVG8odGhpcy5wYW5lbENvbnRhaW4pLHRoaXMucGFuZWxDb250YWluLmFwcGVuZFRvKHQpO3ZhciBpPXRoaXMubmUub3B0aW9ucyxzPWku|
      && |YnV0dG9ucztmb3IoYnV0dG9uIGluIHMpdGhpcy5hZGRCdXR0b24oYnV0dG9uLGksITApO3RoaXMucmVvcmRlcigpLHQubm9TZWxlY3QoKX0sYWRkQnV0dG9u|
      && |OmZ1bmN0aW9uKGJ1dHRvbk5hbWUsb3B0aW9ucyxub09yZGVyKXt2YXIgYnV0dG9uPW9wdGlvbnMuYnV0dG9uc1tidXR0b25OYW1lXSx0eXBlPWJ1dHRvbi50|
      && |eXBlP2V2YWwoIih0eXBlb2YoIitidXR0b24udHlwZSsnKSA9PSAidW5kZWZpbmVkIikgPyBudWxsIDogJytidXR0b24udHlwZSsiOyIpOm5pY0VkaXRvckJ1|
      && |dHRvbixoYXNCdXR0b249YmtMaWIuaW5BcnJheSh0aGlzLmJ1dHRvbkxpc3QsYnV0dG9uTmFtZSk7dHlwZSYmKGhhc0J1dHRvbnx8dGhpcy5uZS5vcHRpb25z|
      && |LmZ1bGxQYW5lbCkmJih0aGlzLnBhbmVsQnV0dG9ucy5wdXNoKG5ldyB0eXBlKHRoaXMucGFuZWxFbG0sYnV0dG9uTmFtZSxvcHRpb25zLHRoaXMubmUpKSxo|
      && |YXNCdXR0b258fHRoaXMuYnV0dG9uTGlzdC5wdXNoKGJ1dHRvbk5hbWUpKX0sZmluZEJ1dHRvbjpmdW5jdGlvbih0KXtmb3IodmFyIGU9MDtlPHRoaXMucGFu|
      && |ZWxCdXR0b25zLmxlbmd0aDtlKyspaWYodGhpcy5wYW5lbEJ1dHRvbnNbZV0ubmFtZT09dClyZXR1cm4gdGhpcy5wYW5lbEJ1dHRvbnNbZV19LHJlb3JkZXI6|
      && |ZnVuY3Rpb24oKXtmb3IodmFyIHQ9dGhpcy5idXR0b25MaXN0LGU9MDtlPHQubGVuZ3RoO2UrKyl7dmFyIG49dGhpcy5maW5kQnV0dG9uKHRbZV0pO24mJnRo|
      && |aXMucGFuZWxFbG0uYXBwZW5kQ2hpbGQobi5tYXJnaW4pfX0scmVtb3ZlOmZ1bmN0aW9uKCl7dGhpcy5lbG0ucmVtb3ZlKCl9fSksbmljRWRpdG9yQnV0dG9u|
      && |PWJrQ2xhc3MuZXh0ZW5kKHtjb25zdHJ1Y3Q6ZnVuY3Rpb24odCxlLG4saSl7dGhpcy5vcHRpb25zPW4uYnV0dG9uc1tlXSx0aGlzLm5hbWU9ZSx0aGlzLm5l|
      && |PWksdGhpcy5lbG09dCx0aGlzLm1hcmdpbj1uZXcgYmtFbGVtZW50KCJESVYiKS5zZXRTdHlsZSh7ZmxvYXQ6ImxlZnQiLG1hcmdpblRvcDoiMnB4In0pLmFw|
      && |cGVuZFRvKHQpLHRoaXMuY29udGFpbj1uZXcgYmtFbGVtZW50KCJESVYiKS5zZXRTdHlsZSh7d2lkdGg6IjIwcHgiLGhlaWdodDoiMjBweCJ9KS5hZGRDbGFz|
      && |cygiYnV0dG9uQ29udGFpbiIpLmFwcGVuZFRvKHRoaXMubWFyZ2luKSx0aGlzLmJvcmRlcj1uZXcgYmtFbGVtZW50KCJESVYiKS5zZXRTdHlsZSh7YmFja2dy|
      && |b3VuZENvbG9yOiIjZWZlZmVmIixib3JkZXI6IjFweCBzb2xpZCAjZWZlZmVmIn0pLmFwcGVuZFRvKHRoaXMuY29udGFpbiksdGhpcy5idXR0b249bmV3IGJr|
      && |RWxlbWVudCgiRElWIikuc2V0U3R5bGUoe3dpZHRoOiIxOHB4IixoZWlnaHQ6IjE4cHgiLG92ZXJmbG93OiJoaWRkZW4iLHpvb206MSxjdXJzb3I6InBvaW50|
      && |ZXIifSkuYWRkQ2xhc3MoImJ1dHRvbiIpLnNldFN0eWxlKHRoaXMubmUuZ2V0SWNvbihlLG4pKS5hcHBlbmRUbyh0aGlzLmJvcmRlciksdGhpcy5idXR0b24u|
      && |YWRkRXZlbnQoIm1vdXNlb3ZlciIsdGhpcy5ob3Zlck9uLmNsb3N1cmUodGhpcykpLmFkZEV2ZW50KCJtb3VzZW91dCIsdGhpcy5ob3Zlck9mZi5jbG9zdXJl|
      && |KHRoaXMpKS5hZGRFdmVudCgibW91c2Vkb3duIix0aGlzLm1vdXNlQ2xpY2suY2xvc3VyZSh0aGlzKSkubm9TZWxlY3QoKSx3aW5kb3cub3BlcmF8fCh0aGlz|
      && |LmJ1dHRvbi5vbm1vdXNlZG93bj10aGlzLmJ1dHRvbi5vbmNsaWNrPWJrTGliLmNhbmNlbEV2ZW50KSxpLmFkZEV2ZW50KCJzZWxlY3RlZCIsdGhpcy5lbmFi|
      && |bGUuY2xvc3VyZSh0aGlzKSkuYWRkRXZlbnQoImJsdXIiLHRoaXMuZGlzYWJsZS5jbG9zdXJlKHRoaXMpKS5hZGRFdmVudCgia2V5Iix0aGlzLmtleS5jbG9z|
      && |dXJlKHRoaXMpKSx0aGlzLmRpc2FibGUoKSx0aGlzLmluaXQoKX0saW5pdDpmdW5jdGlvbigpe30saGlkZTpmdW5jdGlvbigpe3RoaXMuY29udGFpbi5zZXRT|
      && |dHlsZSh7ZGlzcGxheToibm9uZSJ9KX0sdXBkYXRlU3RhdGU6ZnVuY3Rpb24oKXt0aGlzLmlzRGlzYWJsZWQ/dGhpcy5zZXRCZygpOnRoaXMuaXNIb3Zlcj90|
      && |aGlzLnNldEJnKCJob3ZlciIpOnRoaXMuaXNBY3RpdmU/dGhpcy5zZXRCZygiYWN0aXZlIik6dGhpcy5zZXRCZygpfSxzZXRCZzpmdW5jdGlvbih0KXtzd2l0|
      && |Y2godCl7Y2FzZSJob3ZlciI6dmFyIGU9e2JvcmRlcjoiMXB4IHNvbGlkICM2NjYiLGJhY2tncm91bmRDb2xvcjoiI2RkZCJ9O2JyZWFrO2Nhc2UiYWN0aXZl|
      && |IjplPXtib3JkZXI6IjFweCBzb2xpZCAjNjY2IixiYWNrZ3JvdW5kQ29sb3I6IiNjY2MifTticmVhaztkZWZhdWx0OmU9e2JvcmRlcjoiMXB4IHNvbGlkICNl|
      && |ZmVmZWYiLGJhY2tncm91bmRDb2xvcjoiI2VmZWZlZiJ9fXRoaXMuYm9yZGVyLnNldFN0eWxlKGUpLmFkZENsYXNzKCJidXR0b24tIit0KX0sY2hlY2tOb2Rl|
      && |czpmdW5jdGlvbih0KXt2YXIgZT10O2Rve2lmKHRoaXMub3B0aW9ucy50YWdzJiZia0xpYi5pbkFycmF5KHRoaXMub3B0aW9ucy50YWdzLGUubm9kZU5hbWUp|
      && |KXJldHVybiB0aGlzLmFjdGl2YXRlKCksITB9d2hpbGUoZT1lLnBhcmVudE5vZGUmJiJuaWNFZGl0IiE9ZS5jbGFzc05hbWUpO2ZvcihlPSRCSyh0KTszPT1l|
      && |Lm5vZGVUeXBlOyllPSRCSyhlLnBhcmVudE5vZGUpO2lmKHRoaXMub3B0aW9ucy5jc3MpZm9yKGl0bSBpbiB0aGlzLm9wdGlvbnMuY3NzKWlmKGUuZ2V0U3R5|
      && |bGUoaXRtLHRoaXMubmUuc2VsZWN0ZWRJbnN0YW5jZS5pbnN0YW5jZURvYyk9PXRoaXMub3B0aW9ucy5jc3NbaXRtXSlyZXR1cm4gdGhpcy5hY3RpdmF0ZSgp|
      && |LCEwO3JldHVybiB0aGlzLmRlYWN0aXZhdGUoKSwhMX0sYWN0aXZhdGU6ZnVuY3Rpb24oKXt0aGlzLmlzRGlzYWJsZWR8fCh0aGlzLmlzQWN0aXZlPSEwLHRo|
      && |aXMudXBkYXRlU3RhdGUoKSx0aGlzLm5lLmZpcmVFdmVudCgiYnV0dG9uQWN0aXZhdGUiLHRoaXMpKX0sZGVhY3RpdmF0ZTpmdW5jdGlvbigpe3RoaXMuaXNB|
      && |Y3RpdmU9ITEsdGhpcy51cGRhdGVTdGF0ZSgpLHRoaXMuaXNEaXNhYmxlZHx8dGhpcy5uZS5maXJlRXZlbnQoImJ1dHRvbkRlYWN0aXZhdGUiLHRoaXMpfSxl|
      && |bmFibGU6ZnVuY3Rpb24odCxlKXt0aGlzLmlzRGlzYWJsZWQ9ITEsdGhpcy5jb250YWluLnNldFN0eWxlKHtvcGFjaXR5OjF9KS5hZGRDbGFzcygiYnV0dG9u|
      && |RW5hYmxlZCIpLHRoaXMudXBkYXRlU3RhdGUoKSx0aGlzLmNoZWNrTm9kZXMoZSl9LGRpc2FibGU6ZnVuY3Rpb24odCxlKXt0aGlzLmlzRGlzYWJsZWQ9ITAs|
      && |dGhpcy5jb250YWluLnNldFN0eWxlKHtvcGFjaXR5Oi42fSkucmVtb3ZlQ2xhc3MoImJ1dHRvbkVuYWJsZWQiKSx0aGlzLnVwZGF0ZVN0YXRlKCl9LHRvZ2ds|
      && |ZUFjdGl2ZTpmdW5jdGlvbigpe3RoaXMuaXNBY3RpdmU/dGhpcy5kZWFjdGl2YXRlKCk6dGhpcy5hY3RpdmF0ZSgpfSxob3Zlck9uOmZ1bmN0aW9uKCl7dGhp|
      && |cy5pc0Rpc2FibGVkfHwodGhpcy5pc0hvdmVyPSEwLHRoaXMudXBkYXRlU3RhdGUoKSx0aGlzLm5lLmZpcmVFdmVudCgiYnV0dG9uT3ZlciIsdGhpcykpfSxo|
      && |b3Zlck9mZjpmdW5jdGlvbigpe3RoaXMuaXNIb3Zlcj0hMSx0aGlzLnVwZGF0ZVN0YXRlKCksdGhpcy5uZS5maXJlRXZlbnQoImJ1dHRvbk91dCIsdGhpcyl9|
      && |LG1vdXNlQ2xpY2s6ZnVuY3Rpb24oKXt0aGlzLm9wdGlvbnMuY29tbWFuZCYmKHRoaXMubmUubmljQ29tbWFuZCh0aGlzLm9wdGlvbnMuY29tbWFuZCx0aGlz|
      && |Lm9wdGlvbnMuY29tbWFuZEFyZ3MpLHRoaXMub3B0aW9ucy5ub0FjdGl2ZXx8dGhpcy50b2dnbGVBY3RpdmUoKSksdGhpcy5uZS5maXJlRXZlbnQoImJ1dHRv|
      && |bkNsaWNrIix0aGlzKX0sa2V5OmZ1bmN0aW9uKHQsZSl7dGhpcy5vcHRpb25zLmtleSYmZS5jdHJsS2V5JiZTdHJpbmcuZnJvbUNoYXJDb2RlKGUua2V5Q29k|
      && |ZXx8ZS5jaGFyQ29kZSkudG9Mb3dlckNhc2UoKT09dGhpcy5vcHRpb25zLmtleSYmKHRoaXMubW91c2VDbGljaygpLGUucHJldmVudERlZmF1bHQmJmUucHJl|
      && |dmVudERlZmF1bHQoKSl9fSksbmljUGx1Z2luPWJrQ2xhc3MuZXh0ZW5kKHtjb25zdHJ1Y3Q6ZnVuY3Rpb24odCxlKXt0aGlzLm9wdGlvbnM9ZSx0aGlzLm5l|
      && |PXQsdGhpcy5uZS5hZGRFdmVudCgicGFuZWwiLHRoaXMubG9hZFBhbmVsLmNsb3N1cmUodGhpcykpLHRoaXMuaW5pdCgpfSxsb2FkUGFuZWw6ZnVuY3Rpb24o|
      && |dCl7dmFyIGU9dGhpcy5vcHRpb25zLmJ1dHRvbnM7Zm9yKHZhciBuIGluIGUpdC5hZGRCdXR0b24obix0aGlzLm9wdGlvbnMpO3QucmVvcmRlcigpfSxpbml0|
      && |OmZ1bmN0aW9uKCl7fX0pLG5pY1BhbmVPcHRpb25zPXt9LG5pY0VkaXRvclBhbmU9YmtDbGFzcy5leHRlbmQoe2NvbnN0cnVjdDpmdW5jdGlvbih0LGUsbixp|
      && |KXt0aGlzLm5lPWUsdGhpcy5lbG09dCx0aGlzLnBvcz10LnBvcygpLHRoaXMuY29udGFpbj1uZXcgYmtFbGVtZW50KCJkaXYiKS5zZXRTdHlsZSh7ekluZGV4|
      && |OiI5OTk5OSIsb3ZlcmZsb3c6ImhpZGRlbiIscG9zaXRpb246ImFic29sdXRlIixsZWZ0OnRoaXMucG9zWzBdKyJweCIsdG9wOnRoaXMucG9zWzFdKyJweCJ9|
      && |KSx0aGlzLnBhbmU9bmV3IGJrRWxlbWVudCgiZGl2Iikuc2V0U3R5bGUoe2ZvbnRTaXplOiIxMnB4Iixib3JkZXI6IjFweCBzb2xpZCAjY2NjIixvdmVyZmxv|
      && |dzoiaGlkZGVuIixwYWRkaW5nOiI0cHgiLHRleHRBbGlnbjoibGVmdCIsYmFja2dyb3VuZENvbG9yOiIjZmZmZmM5In0pLmFkZENsYXNzKCJwYW5lIikuc2V0|
      && |U3R5bGUobikuYXBwZW5kVG8odGhpcy5jb250YWluKSxpJiYhaS5vcHRpb25zLm5vQ2xvc2UmJih0aGlzLmNsb3NlPW5ldyBia0VsZW1lbnQoImRpdiIpLnNl|
      && |dFN0eWxlKHtmbG9hdDoicmlnaHQiLGhlaWdodDoiMTZweCIsd2lkdGg6IjE2cHgiLGN1cnNvcjoicG9pbnRlciJ9KS5zZXRTdHlsZSh0aGlzLm5lLmdldElj|
      && |b24oImNsb3NlIixuaWNQYW5lT3B0aW9ucykpLmFkZEV2ZW50KCJtb3VzZWRvd24iLGkucmVtb3ZlUGFuZS5jbG9zdXJlKHRoaXMpKS5hcHBlbmRUbyh0aGlz|
      && |LnBhbmUpKSx0aGlzLmNvbnRhaW4ubm9TZWxlY3QoKS5hcHBlbmRUbyhkb2N1bWVudC5ib2R5KSx0aGlzLnBvc2l0aW9uKCksdGhpcy5pbml0KCl9LGluaXQ6|
      && |ZnVuY3Rpb24oKXt9LHBvc2l0aW9uOmZ1bmN0aW9uKCl7aWYodGhpcy5uZS5uaWNQYW5lbCl7dmFyIHQ9dGhpcy5uZS5uaWNQYW5lbC5lbG0sZT10LnBvcygp|
      && |WzBdK3BhcnNlSW50KHQuZ2V0U3R5bGUoIndpZHRoIikpLShwYXJzZUludCh0aGlzLnBhbmUuZ2V0U3R5bGUoIndpZHRoIikpKzgpO2U8dGhpcy5wb3NbMF0m|
      && |JnRoaXMuY29udGFpbi5zZXRTdHlsZSh7bGVmdDplKyJweCJ9KX19LHRvZ2dsZTpmdW5jdGlvbigpe3RoaXMuaXNWaXNpYmxlPSF0aGlzLmlzVmlzaWJsZSx0|
      && |aGlzLmNvbnRhaW4uc2V0U3R5bGUoe2Rpc3BsYXk6dGhpcy5pc1Zpc2libGU/ImJsb2NrIjoibm9uZSJ9KX0scmVtb3ZlOmZ1bmN0aW9uKCl7dGhpcy5jb250|
      && |YWluJiYodGhpcy5jb250YWluLnJlbW92ZSgpLHRoaXMuY29udGFpbj1udWxsKX0sYXBwZW5kOmZ1bmN0aW9uKHQpe3QuYXBwZW5kVG8odGhpcy5wYW5lKX0s|
      && |c2V0Q29udGVudDpmdW5jdGlvbih0KXt0aGlzLnBhbmUuc2V0Q29udGVudCh0KX19KSxuaWNFZGl0b3JBZHZhbmNlZEJ1dHRvbj1uaWNFZGl0b3JCdXR0b24u|
      && |ZXh0ZW5kKHtpbml0OmZ1bmN0aW9uKCl7dGhpcy5uZS5hZGRFdmVudCgic2VsZWN0ZWQiLHRoaXMucmVtb3ZlUGFuZS5jbG9zdXJlKHRoaXMpKS5hZGRFdmVu|
      && |dCgiYmx1ciIsdGhpcy5yZW1vdmVQYW5lLmNsb3N1cmUodGhpcykpfSxtb3VzZUNsaWNrOmZ1bmN0aW9uKCl7dGhpcy5pc0Rpc2FibGVkfHwodGhpcy5wYW5l|
      && |JiZ0aGlzLnBhbmUucGFuZT90aGlzLnJlbW92ZVBhbmUoKToodGhpcy5wYW5lPW5ldyBuaWNFZGl0b3JQYW5lKHRoaXMuY29udGFpbix0aGlzLm5lLHt3aWR0|
      && |aDp0aGlzLndpZHRofHwiMjcwcHgiLGJhY2tncm91bmRDb2xvcjoiI2ZmZiJ9LHRoaXMpLHRoaXMuYWRkUGFuZSgpLHRoaXMubmUuc2VsZWN0ZWRJbnN0YW5j|
      && |ZS5zYXZlUm5nKCkpKX0sYWRkRm9ybTpmdW5jdGlvbih0LGUpe2ZvcihpdG0gaW4gdGhpcy5mb3JtPW5ldyBia0VsZW1lbnQoImZvcm0iKS5hZGRFdmVudCgi|
      && |c3VibWl0Iix0aGlzLnN1Ym1pdC5jbG9zdXJlTGlzdGVuZXIodGhpcykpLHRoaXMucGFuZS5hcHBlbmQodGhpcy5mb3JtKSx0aGlzLmlucHV0cz17fSx0KXt2|
      && |YXIgbj10W2l0bV0saT0iIjtlJiYoaT1lLmdldEF0dHJpYnV0ZShpdG0pKSxpfHwoaT1uLnZhbHVlfHwiIik7dmFyIHM9dFtpdG1dLnR5cGU7aWYoInRpdGxl|
      && |Ij09cyluZXcgYmtFbGVtZW50KCJkaXYiKS5zZXRDb250ZW50KG4udHh0KS5zZXRTdHlsZSh7Zm9udFNpemU6IjE0cHgiLGZvbnRXZWlnaHQ6ImJvbGQiLHBh|
      && |ZGRpbmc6IjBweCIsbWFyZ2luOiIycHggMCJ9KS5hcHBlbmRUbyh0aGlzLmZvcm0pO2Vsc2V7dmFyIG89bmV3IGJrRWxlbWVudCgiZGl2Iikuc2V0U3R5bGUo|
      && |e292ZXJmbG93OiJoaWRkZW4iLGNsZWFyOiJib3RoIn0pLmFwcGVuZFRvKHRoaXMuZm9ybSk7c3dpdGNoKG4udHh0JiZuZXcgYmtFbGVtZW50KCJsYWJlbCIp|
      && |LnNldEF0dHJpYnV0ZXMoe2ZvcjppdG19KS5zZXRDb250ZW50KG4udHh0KS5zZXRTdHlsZSh7bWFyZ2luOiIycHggNHB4Iixmb250U2l6ZToiMTNweCIsd2lk|
      && |dGg6IjUwcHgiLGxpbmVIZWlnaHQ6IjIwcHgiLHRleHRBbGlnbjoicmlnaHQiLGZsb2F0OiJsZWZ0In0pLmFwcGVuZFRvKG8pLHMpe2Nhc2UidGV4dCI6dGhp|
      && |cy5pbnB1dHNbaXRtXT1uZXcgYmtFbGVtZW50KCJpbnB1dCIpLnNldEF0dHJpYnV0ZXMoe2lkOml0bSx2YWx1ZTppLHR5cGU6InRleHQifSkuc2V0U3R5bGUo|
      && |e21hcmdpbjoiMnB4IDAiLGZvbnRTaXplOiIxM3B4IixmbG9hdDoibGVmdCIsaGVpZ2h0OiIyMHB4Iixib3JkZXI6IjFweCBzb2xpZCAjY2NjIixvdmVyZmxv|
      && |dzoiaGlkZGVuIn0pLnNldFN0eWxlKG4uc3R5bGUpLmFwcGVuZFRvKG8pO2JyZWFrO2Nhc2Uic2VsZWN0Ijpmb3Iob3B0IGluIHRoaXMuaW5wdXRzW2l0bV09|
      && |bmV3IGJrRWxlbWVudCgic2VsZWN0Iikuc2V0QXR0cmlidXRlcyh7aWQ6aXRtfSkuc2V0U3R5bGUoe2JvcmRlcjoiMXB4IHNvbGlkICNjY2MiLGZsb2F0OiJs|
      && |ZWZ0IixtYXJnaW46IjJweCAwIn0pLmFwcGVuZFRvKG8pLG4ub3B0aW9ucyluZXcgYmtFbGVtZW50KCJvcHRpb24iKS5zZXRBdHRyaWJ1dGVzKHt2YWx1ZTpv|
      && |cHQsc2VsZWN0ZWQ6b3B0PT1pPyJzZWxlY3RlZCI6IiJ9KS5zZXRDb250ZW50KG4ub3B0aW9uc1tvcHRdKS5hcHBlbmRUbyh0aGlzLmlucHV0c1tpdG1dKTti|
      && |cmVhaztjYXNlImNvbnRlbnQiOnRoaXMuaW5wdXRzW2l0bV09bmV3IGJrRWxlbWVudCgidGV4dGFyZWEiKS5zZXRBdHRyaWJ1dGVzKHtpZDppdG19KS5zZXRT|
      && |dHlsZSh7Ym9yZGVyOiIxcHggc29saWQgI2NjYyIsZmxvYXQ6ImxlZnQifSkuc2V0U3R5bGUobi5zdHlsZSkuYXBwZW5kVG8obyksdGhpcy5pbnB1dHNbaXRt|
      && |XS52YWx1ZT1pfX19bmV3IGJrRWxlbWVudCgiaW5wdXQiKS5zZXRBdHRyaWJ1dGVzKHt0eXBlOiJzdWJtaXQifSkuc2V0U3R5bGUoe2JhY2tncm91bmRDb2xv|
      && |cjoiI2VmZWZlZiIsYm9yZGVyOiIxcHggc29saWQgI2NjYyIsbWFyZ2luOiIzcHggMCIsZmxvYXQ6ImxlZnQiLGNsZWFyOiJib3RoIn0pLmFwcGVuZFRvKHRo|
      && |aXMuZm9ybSksdGhpcy5mb3JtLm9uc3VibWl0PWJrTGliLmNhbmNlbEV2ZW50fSxzdWJtaXQ6ZnVuY3Rpb24oKXt9LGZpbmRFbG06ZnVuY3Rpb24odCxlLG4p|
      && |e2Zvcih2YXIgaT10aGlzLm5lLnNlbGVjdGVkSW5zdGFuY2UuZ2V0RWxtKCkuZ2V0RWxlbWVudHNCeVRhZ05hbWUodCkscz0wO3M8aS5sZW5ndGg7cysrKWlm|
      && |KGlbc10uZ2V0QXR0cmlidXRlKGUpPT1uKXJldHVybiAkQksoaVtzXSl9LHJlbW92ZVBhbmU6ZnVuY3Rpb24oKXt0aGlzLnBhbmUmJih0aGlzLnBhbmUucmVt|
      && |b3ZlKCksdGhpcy5wYW5lPW51bGwsdGhpcy5uZS5zZWxlY3RlZEluc3RhbmNlLnJlc3RvcmVSbmcoKSl9fSksbmljQnV0dG9uVGlwcz1ia0NsYXNzLmV4dGVu|
      && |ZCh7Y29uc3RydWN0OmZ1bmN0aW9uKHQpe3RoaXMubmU9dCx0LmFkZEV2ZW50KCJidXR0b25PdmVyIix0aGlzLnNob3cuY2xvc3VyZSh0aGlzKSkuYWRkRXZl|
      && |bnQoImJ1dHRvbk91dCIsdGhpcy5oaWRlLmNsb3N1cmUodGhpcykpfSxzaG93OmZ1bmN0aW9uKHQpe3RoaXMudGltZXI9c2V0VGltZW91dCh0aGlzLmNyZWF0|
      && |ZS5jbG9zdXJlKHRoaXMsdCksNDAwKX0sY3JlYXRlOmZ1bmN0aW9uKHQpe3RoaXMudGltZXI9bnVsbCx0aGlzLnBhbmV8fCh0aGlzLnBhbmU9bmV3IG5pY0Vk|
      && |aXRvclBhbmUodC5idXR0b24sdGhpcy5uZSx7Zm9udFNpemU6IjEycHgiLG1hcmdpblRvcDoiNXB4In0pLHRoaXMucGFuZS5zZXRDb250ZW50KHQub3B0aW9u|
      && |cy5uYW1lKSl9LGhpZGU6ZnVuY3Rpb24odCl7dGhpcy50aW1lciYmY2xlYXJUaW1lb3V0KHRoaXMudGltZXIpLHRoaXMucGFuZSYmKHRoaXMucGFuZT10aGlz|
      && |LnBhbmUucmVtb3ZlKCkpfX0pO25pY0VkaXRvcnMucmVnaXN0ZXJQbHVnaW4obmljQnV0dG9uVGlwcyk7dmFyIG5pY1NlbGVjdE9wdGlvbnM9e2J1dHRvbnM6|
      && |e2ZvbnRTaXplOntuYW1lOl9fKCJTZWxlY3QgRm9udCBTaXplIiksdHlwZToibmljRWRpdG9yRm9udFNpemVTZWxlY3QiLGNvbW1hbmQ6ImZvbnRzaXplIn0s|
      && |Zm9udEZhbWlseTp7bmFtZTpfXygiU2VsZWN0IEZvbnQgRmFtaWx5IiksdHlwZToibmljRWRpdG9yRm9udEZhbWlseVNlbGVjdCIsY29tbWFuZDoiZm9udG5h|
      && |bWUifSxmb250Rm9ybWF0OntuYW1lOl9fKCJTZWxlY3QgRm9udCBGb3JtYXQiKSx0eXBlOiJuaWNFZGl0b3JGb250Rm9ybWF0U2VsZWN0Iixjb21tYW5kOiJm|
      && |b3JtYXRCbG9jayJ9fX0sbmljRWRpdG9yU2VsZWN0PWJrQ2xhc3MuZXh0ZW5kKHtjb25zdHJ1Y3Q6ZnVuY3Rpb24odCxlLG4saSl7dGhpcy5vcHRpb25zPW4u|
      && |YnV0dG9uc1tlXSx0aGlzLmVsbT10LHRoaXMubmU9aSx0aGlzLm5hbWU9ZSx0aGlzLnNlbE9wdGlvbnM9bmV3IEFycmF5LHRoaXMubWFyZ2luPW5ldyBia0Vs|
      && |ZW1lbnQoImRpdiIpLnNldFN0eWxlKHtmbG9hdDoibGVmdCIsbWFyZ2luOiIycHggMXB4IDAgMXB4In0pLmFwcGVuZFRvKHRoaXMuZWxtKSx0aGlzLmNvbnRh|
      && |aW49bmV3IGJrRWxlbWVudCgiZGl2Iikuc2V0U3R5bGUoe3dpZHRoOiI5MHB4IixoZWlnaHQ6IjIwcHgiLGN1cnNvcjoicG9pbnRlciIsb3ZlcmZsb3c6Imhp|
      && |ZGRlbiJ9KS5hZGRDbGFzcygic2VsZWN0Q29udGFpbiIpLmFkZEV2ZW50KCJjbGljayIsdGhpcy50b2dnbGUuY2xvc3VyZSh0aGlzKSkuYXBwZW5kVG8odGhp|
      && |cy5tYXJnaW4pLHRoaXMuaXRlbXM9bmV3IGJrRWxlbWVudCgiZGl2Iikuc2V0U3R5bGUoe292ZXJmbG93OiJoaWRkZW4iLHpvb206MSxib3JkZXI6IjFweCBz|
      && |b2xpZCAjY2NjIixwYWRkaW5nTGVmdDoiM3B4IixiYWNrZ3JvdW5kQ29sb3I6IiNmZmYifSkuYXBwZW5kVG8odGhpcy5jb250YWluKSx0aGlzLmNvbnRyb2w9|
      && |bmV3IGJrRWxlbWVudCgiZGl2Iikuc2V0U3R5bGUoe292ZXJmbG93OiJoaWRkZW4iLGZsb2F0OiJyaWdodCIsaGVpZ2h0OiIxOHB4Iix3aWR0aDoiMTZweCJ9|
      && |KS5hZGRDbGFzcygic2VsZWN0Q29udHJvbCIpLnNldFN0eWxlKHRoaXMubmUuZ2V0SWNvbigiYXJyb3ciLG4pKS5hcHBlbmRUbyh0aGlzLml0ZW1zKSx0aGlz|
      && |LnR4dD1uZXcgYmtFbGVtZW50KCJkaXYiKS5zZXRTdHlsZSh7b3ZlcmZsb3c6ImhpZGRlbiIsZmxvYXQ6ImxlZnQiLHdpZHRoOiI2NnB4IixoZWlnaHQ6IjE0|
      && |cHgiLG1hcmdpblRvcDoiMXB4Iixmb250RmFtaWx5OiJzYW5zLXNlcmlmIix0ZXh0QWxpZ246ImNlbnRlciIsZm9udFNpemU6IjEycHgifSkuYWRkQ2xhc3Mo|
      && |InNlbGVjdFR4dCIpLmFwcGVuZFRvKHRoaXMuaXRlbXMpLHdpbmRvdy5vcGVyYXx8KHRoaXMuY29udGFpbi5vbm1vdXNlZG93bj10aGlzLmNvbnRyb2wub25t|
      && |b3VzZWRvd249dGhpcy50eHQub25tb3VzZWRvd249YmtMaWIuY2FuY2VsRXZlbnQpLHRoaXMubWFyZ2luLm5vU2VsZWN0KCksdGhpcy5uZS5hZGRFdmVudCgi|
      && |c2VsZWN0ZWQiLHRoaXMuZW5hYmxlLmNsb3N1cmUodGhpcykpLmFkZEV2ZW50KCJibHVyIix0aGlzLmRpc2FibGUuY2xvc3VyZSh0aGlzKSksdGhpcy5kaXNh|
      && |YmxlKCksdGhpcy5pbml0KCl9LGRpc2FibGU6ZnVuY3Rpb24oKXt0aGlzLmlzRGlzYWJsZWQ9ITAsdGhpcy5jbG9zZSgpLHRoaXMuY29udGFpbi5zZXRTdHls|
      && |ZSh7b3BhY2l0eTouNn0pfSxlbmFibGU6ZnVuY3Rpb24odCl7dGhpcy5pc0Rpc2FibGVkPSExLHRoaXMuY2xvc2UoKSx0aGlzLmNvbnRhaW4uc2V0U3R5bGUo|
      && |e29wYWNpdHk6MX0pfSxzZXREaXNwbGF5OmZ1bmN0aW9uKHQpe3RoaXMudHh0LnNldENvbnRlbnQodCl9LHRvZ2dsZTpmdW5jdGlvbigpe3RoaXMuaXNEaXNh|
      && |YmxlZHx8KHRoaXMucGFuZT90aGlzLmNsb3NlKCk6dGhpcy5vcGVuKCkpfSxvcGVuOmZ1bmN0aW9uKCl7dGhpcy5wYW5lPW5ldyBuaWNFZGl0b3JQYW5lKHRo|
      && |aXMuaXRlbXMsdGhpcy5uZSx7d2lkdGg6Ijg4cHgiLHBhZGRpbmc6IjBweCIsYm9yZGVyVG9wOjAsYm9yZGVyTGVmdDoiMXB4IHNvbGlkICNjY2MiLGJvcmRl|
      && |clJpZ2h0OiIxcHggc29saWQgI2NjYyIsYm9yZGVyQm90dG9tOiIwcHgiLGJhY2tncm91bmRDb2xvcjoiI2ZmZiJ9KTtmb3IodmFyIHQ9MDt0PHRoaXMuc2Vs|
      && |T3B0aW9ucy5sZW5ndGg7dCsrKXt2YXIgZT10aGlzLnNlbE9wdGlvbnNbdF0sbj1uZXcgYmtFbGVtZW50KCJkaXYiKS5zZXRTdHlsZSh7b3ZlcmZsb3c6Imhp|
      && |ZGRlbiIsYm9yZGVyQm90dG9tOiIxcHggc29saWQgI2NjYyIsd2lkdGg6Ijg4cHgiLHRleHRBbGlnbjoibGVmdCIsb3ZlcmZsb3c6ImhpZGRlbiIsY3Vyc29y|
      && |OiJwb2ludGVyIn0pLGk9bmV3IGJrRWxlbWVudCgiZGl2Iikuc2V0U3R5bGUoe3BhZGRpbmc6IjBweCA0cHgifSkuc2V0Q29udGVudChlWzFdKS5hcHBlbmRU|
      && |byhuKS5ub1NlbGVjdCgpO2kuYWRkRXZlbnQoImNsaWNrIix0aGlzLnVwZGF0ZS5jbG9zdXJlKHRoaXMsZVswXSkpLmFkZEV2ZW50KCJtb3VzZW92ZXIiLHRo|
      && |aXMub3Zlci5jbG9zdXJlKHRoaXMsaSkpLmFkZEV2ZW50KCJtb3VzZW91dCIsdGhpcy5vdXQuY2xvc3VyZSh0aGlzLGkpKS5zZXRBdHRyaWJ1dGVzKCJpZCIs|
      && |ZVswXSksdGhpcy5wYW5lLmFwcGVuZChuKSx3aW5kb3cub3BlcmF8fChpLm9ubW91c2Vkb3duPWJrTGliLmNhbmNlbEV2ZW50KX19LGNsb3NlOmZ1bmN0aW9u|
      && |KCl7dGhpcy5wYW5lJiYodGhpcy5wYW5lPXRoaXMucGFuZS5yZW1vdmUoKSl9LG92ZXI6ZnVuY3Rpb24odCl7dC5zZXRTdHlsZSh7YmFja2dyb3VuZENvbG9y|
      && |OiIjY2NjIn0pfSxvdXQ6ZnVuY3Rpb24odCl7dC5zZXRTdHlsZSh7YmFja2dyb3VuZENvbG9yOiIjZmZmIn0pfSxhZGQ6ZnVuY3Rpb24odCxlKXt0aGlzLnNl|
      && |bE9wdGlvbnMucHVzaChuZXcgQXJyYXkodCxlKSl9LHVwZGF0ZTpmdW5jdGlvbih0KXt0aGlzLm5lLm5pY0NvbW1hbmQodGhpcy5vcHRpb25zLmNvbW1hbmQs|
      && |dCksdGhpcy5jbG9zZSgpfX0pLG5pY0VkaXRvckZvbnRTaXplU2VsZWN0PW5pY0VkaXRvclNlbGVjdC5leHRlbmQoe3NlbDp7MToiMSZuYnNwOyg4cHQpIiwy|
      && |OiIyJm5ic3A7KDEwcHQpIiwzOiIzJm5ic3A7KDEycHQpIiw0OiI0Jm5ic3A7KDE0cHQpIiw1OiI1Jm5ic3A7KDE4cHQpIiw2OiI2Jm5ic3A7KDI0cHQpIn0s|
      && |aW5pdDpmdW5jdGlvbigpe2ZvcihpdG0gaW4gdGhpcy5zZXREaXNwbGF5KCJGb250Jm5ic3A7U2l6ZS4uLiIpLHRoaXMuc2VsKXRoaXMuYWRkKGl0bSwnPGZv|
      && |bnQgc2l6ZT0iJytpdG0rJyI+Jyt0aGlzLnNlbFtpdG1dKyI8L2ZvbnQ+Iil9fSksbmljRWRpdG9yRm9udEZhbWlseVNlbGVjdD1uaWNFZGl0b3JTZWxlY3Qu|
      && |ZXh0ZW5kKHtzZWw6e2FyaWFsOiJBcmlhbCIsImNvbWljIHNhbnMgbXMiOiJDb21pYyBTYW5zIiwiY291cmllciBuZXciOiJDb3VyaWVyIE5ldyIsZ2Vvcmdp|
      && |YToiR2VvcmdpYSIsaGVsdmV0aWNhOiJIZWx2ZXRpY2EiLGltcGFjdDoiSW1wYWN0IiwidGltZXMgbmV3IHJvbWFuIjoiVGltZXMiLCJ0cmVidWNoZXQgbXMi|
      && |OiJUcmVidWNoZXQiLHZlcmRhbmE6IlZlcmRhbmEifSxpbml0OmZ1bmN0aW9uKCl7Zm9yKGl0bSBpbiB0aGlzLnNldERpc3BsYXkoIkZvbnQmbmJzcDtGYW1p|
      && |bHkuLi4iKSx0aGlzLnNlbCl0aGlzLmFkZChpdG0sJzxmb250IGZhY2U9IicraXRtKyciPicrdGhpcy5zZWxbaXRtXSsiPC9mb250PiIpfX0pLG5pY0VkaXRv|
      && |ckZvbnRGb3JtYXRTZWxlY3Q9bmljRWRpdG9yU2VsZWN0LmV4dGVuZCh7c2VsOntwOiJQYXJhZ3JhcGgiLHByZToiUHJlIixoNjoiSGVhZGluZyZuYnNwOzYi|
      && |LGg1OiJIZWFkaW5nJm5ic3A7NSIsaDQ6IkhlYWRpbmcmbmJzcDs0IixoMzoiSGVhZGluZyZuYnNwOzMiLGgyOiJIZWFkaW5nJm5ic3A7MiIsaDE6IkhlYWRp|
      && |bmcmbmJzcDsxIn0saW5pdDpmdW5jdGlvbigpe2ZvcihpdG0gaW4gdGhpcy5zZXREaXNwbGF5KCJGb250Jm5ic3A7Rm9ybWF0Li4uIiksdGhpcy5zZWwpe3Zh|
      && |ciB0PWl0bS50b1VwcGVyQ2FzZSgpO3RoaXMuYWRkKCI8Iit0KyI+IiwiPCIraXRtKycgc3R5bGU9InBhZGRpbmc6IDBweDsgbWFyZ2luOiAwcHg7Ij4nK3Ro|
      && |aXMuc2VsW2l0bV0rIjwvIit0KyI+Iil9fX0pO25pY0VkaXRvcnMucmVnaXN0ZXJQbHVnaW4obmljUGx1Z2luLG5pY1NlbGVjdE9wdGlvbnMpO3ZhciBuaWNM|
      && |aW5rT3B0aW9ucz17YnV0dG9uczp7bGluazp7bmFtZToiQWRkIExpbmsiLHR5cGU6Im5pY0xpbmtCdXR0b24iLHRhZ3M6WyJBIl19LHVubGluazp7bmFtZToi|
      && |UmVtb3ZlIExpbmsiLGNvbW1hbmQ6InVubGluayIsbm9BY3RpdmU6ITB9fX0sbmljTGlua0J1dHRvbj1uaWNFZGl0b3JBZHZhbmNlZEJ1dHRvbi5leHRlbmQo|
      && |e2FkZFBhbmU6ZnVuY3Rpb24oKXt0aGlzLmxuPXRoaXMubmUuc2VsZWN0ZWRJbnN0YW5jZS5zZWxFbG0oKS5wYXJlbnRUYWcoIkEiKSx0aGlzLmFkZEZvcm0o|
      && |eyIiOnt0eXBlOiJ0aXRsZSIsdHh0OiJBZGQvRWRpdCBMaW5rIn0saHJlZjp7dHlwZToidGV4dCIsdHh0OiJVUkwiLHZhbHVlOiJodHRwOi8vIixzdHlsZTp7|
      && |d2lkdGg6IjE1MHB4In19LHRpdGxlOnt0eXBlOiJ0ZXh0Iix0eHQ6IlRpdGxlIn0sdGFyZ2V0Ont0eXBlOiJzZWxlY3QiLHR4dDoiT3BlbiBJbiIsb3B0aW9u|
      && |czp7IiI6IkN1cnJlbnQgV2luZG93IixfYmxhbms6Ik5ldyBXaW5kb3cifSxzdHlsZTp7d2lkdGg6IjEwMHB4In19fSx0aGlzLmxuKX0sc3VibWl0OmZ1bmN0|
      && |aW9uKHQpe3ZhciBlPXRoaXMuaW5wdXRzLmhyZWYudmFsdWU7aWYoImh0dHA6Ly8iPT1lfHwiIj09ZSlyZXR1cm4gYWxlcnQoIllvdSBtdXN0IGVudGVyIGEg|
      && |VVJMIHRvIENyZWF0ZSBhIExpbmsiKSwhMTtpZih0aGlzLnJlbW92ZVBhbmUoKSwhdGhpcy5sbil7dmFyIG49ImphdmFzY3JpcHQ6bmljVGVtcCgpOyI7dGhp|
      && |cy5uZS5uaWNDb21tYW5kKCJjcmVhdGVsaW5rIixuKSx0aGlzLmxuPXRoaXMuZmluZEVsbSgiQSIsImhyZWYiLG4pfXRoaXMubG4mJnRoaXMubG4uc2V0QXR0|
      && |cmlidXRlcyh7aHJlZjp0aGlzLmlucHV0cy5ocmVmLnZhbHVlLHRpdGxlOnRoaXMuaW5wdXRzLnRpdGxlLnZhbHVlLHRhcmdldDp0aGlzLmlucHV0cy50YXJn|
      && |ZXQub3B0aW9uc1t0aGlzLmlucHV0cy50YXJnZXQuc2VsZWN0ZWRJbmRleF0udmFsdWV9KX19KTtuaWNFZGl0b3JzLnJlZ2lzdGVyUGx1Z2luKG5pY1BsdWdp|
      && |bixuaWNMaW5rT3B0aW9ucyk7dmFyIG5pY0NvbG9yT3B0aW9ucz17YnV0dG9uczp7Zm9yZWNvbG9yOntuYW1lOl9fKCJDaGFuZ2UgVGV4dCBDb2xvciIpLHR5|
      && |cGU6Im5pY0VkaXRvckNvbG9yQnV0dG9uIixub0Nsb3NlOiEwfSxiZ2NvbG9yOntuYW1lOl9fKCJDaGFuZ2UgQmFja2dyb3VuZCBDb2xvciIpLHR5cGU6Im5p|
      && |Y0VkaXRvckJnQ29sb3JCdXR0b24iLG5vQ2xvc2U6ITB9fX0sbmljRWRpdG9yQ29sb3JCdXR0b249bmljRWRpdG9yQWR2YW5jZWRCdXR0b24uZXh0ZW5kKHth|
      && |ZGRQYW5lOmZ1bmN0aW9uKCl7dmFyIHQ9ezA6IjAwIiwxOiIzMyIsMjoiNjYiLDM6Ijk5Iiw0OiJDQyIsNToiRkYifSxlPW5ldyBia0VsZW1lbnQoIkRJViIp|
      && |LnNldFN0eWxlKHt3aWR0aDoiMjcwcHgifSk7Zm9yKHZhciBuIGluIHQpZm9yKHZhciBpIGluIHQpZm9yKHZhciBzIGluIHQpe3ZhciBvPSIjIit0W25dK3Rb|
      && |c10rdFtpXSxhPW5ldyBia0VsZW1lbnQoIkRJViIpLnNldFN0eWxlKHtjdXJzb3I6InBvaW50ZXIiLGhlaWdodDoiMTVweCIsZmxvYXQ6ImxlZnQifSkuYXBw|
      && |ZW5kVG8oZSkscj1uZXcgYmtFbGVtZW50KCJESVYiKS5zZXRTdHlsZSh7Ym9yZGVyOiIycHggc29saWQgIitvfSkuYXBwZW5kVG8oYSksbD1uZXcgYmtFbGVt|
      && |ZW50KCJESVYiKS5zZXRTdHlsZSh7YmFja2dyb3VuZENvbG9yOm8sb3ZlcmZsb3c6ImhpZGRlbiIsd2lkdGg6IjExcHgiLGhlaWdodDoiMTFweCJ9KS5hZGRF|
      && |dmVudCgiY2xpY2siLHRoaXMuY29sb3JTZWxlY3QuY2xvc3VyZSh0aGlzLG8pKS5hZGRFdmVudCgibW91c2VvdmVyIix0aGlzLm9uLmNsb3N1cmUodGhpcyxy|
      && |KSkuYWRkRXZlbnQoIm1vdXNlb3V0Iix0aGlzLm9mZi5jbG9zdXJlKHRoaXMscixvKSkuYXBwZW5kVG8ocik7d2luZG93Lm9wZXJhfHwoYS5vbm1vdXNlZG93|
      && |bj1sLm9ubW91c2Vkb3duPWJrTGliLmNhbmNlbEV2ZW50KX10aGlzLnBhbmUuYXBwZW5kKGUubm9TZWxlY3QoKSl9LGNvbG9yU2VsZWN0OmZ1bmN0aW9uKHQp|
      && |e3RoaXMubmUubmljQ29tbWFuZCgiZm9yZUNvbG9yIix0KSx0aGlzLnJlbW92ZVBhbmUoKX0sb246ZnVuY3Rpb24odCl7dC5zZXRTdHlsZSh7Ym9yZGVyOiIy|
      && |cHggc29saWQgIzAwMCJ9KX0sb2ZmOmZ1bmN0aW9uKHQsZSl7dC5zZXRTdHlsZSh7Ym9yZGVyOiIycHggc29saWQgIitlfSl9fSksbmljRWRpdG9yQmdDb2xv|
      && |ckJ1dHRvbj1uaWNFZGl0b3JDb2xvckJ1dHRvbi5leHRlbmQoe2NvbG9yU2VsZWN0OmZ1bmN0aW9uKHQpe3RoaXMubmUubmljQ29tbWFuZCgiaGlsaXRlQ29s|
      && |b3IiLHQpLHRoaXMucmVtb3ZlUGFuZSgpfX0pO25pY0VkaXRvcnMucmVnaXN0ZXJQbHVnaW4obmljUGx1Z2luLG5pY0NvbG9yT3B0aW9ucyk7dmFyIG5pY0lt|
      && |YWdlT3B0aW9ucz17YnV0dG9uczp7aW1hZ2U6e25hbWU6IkFkZCBJbWFnZSIsdHlwZToibmljSW1hZ2VCdXR0b24iLHRhZ3M6WyJJTUciXX19fSxuaWNJbWFn|
      && |ZUJ1dHRvbj1uaWNFZGl0b3JBZHZhbmNlZEJ1dHRvbi5leHRlbmQoe2FkZFBhbmU6ZnVuY3Rpb24oKXt0aGlzLmltPXRoaXMubmUuc2VsZWN0ZWRJbnN0YW5j|
      && |ZS5zZWxFbG0oKS5wYXJlbnRUYWcoIklNRyIpLHRoaXMuYWRkRm9ybSh7IiI6e3R5cGU6InRpdGxlIix0eHQ6IkFkZC9FZGl0IEltYWdlIn0sc3JjOnt0eXBl|
      && |OiJ0ZXh0Iix0eHQ6IlVSTCIsdmFsdWU6Imh0dHA6Ly8iLHN0eWxlOnt3aWR0aDoiMTUwcHgifX0sYWx0Ont0eXBlOiJ0ZXh0Iix0eHQ6IkFsdCBUZXh0Iixz|
      && |dHlsZTp7d2lkdGg6IjEwMHB4In19LGFsaWduOnt0eXBlOiJzZWxlY3QiLHR4dDoiQWxpZ24iLG9wdGlvbnM6e25vbmU6IkRlZmF1bHQiLGxlZnQ6IkxlZnQi|
      && |LHJpZ2h0OiJSaWdodCJ9fX0sdGhpcy5pbSl9LHN1Ym1pdDpmdW5jdGlvbih0KXt2YXIgZT10aGlzLmlucHV0cy5zcmMudmFsdWU7aWYoIiI9PWV8fCJodHRw|
      && |Oi8vIj09ZSlyZXR1cm4gYWxlcnQoIllvdSBtdXN0IGVudGVyIGEgSW1hZ2UgVVJMIHRvIGluc2VydCIpLCExO2lmKHRoaXMucmVtb3ZlUGFuZSgpLCF0aGlz|
      && |LmltKXt2YXIgbj0iamF2YXNjcmlwdDpuaWNJbVRlbXAoKTsiO3RoaXMubmUubmljQ29tbWFuZCgiaW5zZXJ0SW1hZ2UiLG4pLHRoaXMuaW09dGhpcy5maW5k|
      && |RWxtKCJJTUciLCJzcmMiLG4pfXRoaXMuaW0mJnRoaXMuaW0uc2V0QXR0cmlidXRlcyh7c3JjOnRoaXMuaW5wdXRzLnNyYy52YWx1ZSxhbHQ6dGhpcy5pbnB1|
      && |dHMuYWx0LnZhbHVlLGFsaWduOnRoaXMuaW5wdXRzLmFsaWduLnZhbHVlfSl9fSk7bmljRWRpdG9ycy5yZWdpc3RlclBsdWdpbihuaWNQbHVnaW4sbmljSW1h|
      && |Z2VPcHRpb25zKTt2YXIgbmljU2F2ZU9wdGlvbnM9e2J1dHRvbnM6e3NhdmU6e25hbWU6X18oIlNhdmUgdGhpcyBjb250ZW50IiksdHlwZToibmljRWRpdG9y|
      && |U2F2ZUJ1dHRvbiJ9fX0sbmljRWRpdG9yU2F2ZUJ1dHRvbj1uaWNFZGl0b3JCdXR0b24uZXh0ZW5kKHtpbml0OmZ1bmN0aW9uKCl7dGhpcy5uZS5vcHRpb25z|
      && |Lm9uU2F2ZXx8dGhpcy5tYXJnaW4uc2V0U3R5bGUoe2Rpc3BsYXk6Im5vbmUifSl9LG1vdXNlQ2xpY2s6ZnVuY3Rpb24oKXt2YXIgdD10aGlzLm5lLm9wdGlv|
      && |bnMub25TYXZlLGU9dGhpcy5uZS5zZWxlY3RlZEluc3RhbmNlO3QoZS5nZXRDb250ZW50KCksZS5lbG0uaWQsZSl9fSk7bmljRWRpdG9ycy5yZWdpc3RlclBs|
      && |dWdpbihuaWNQbHVnaW4sbmljU2F2ZU9wdGlvbnMpO3ZhciBuaWNDb2RlT3B0aW9ucz17YnV0dG9uczp7eGh0bWw6e25hbWU6IkVkaXQgSFRNTCIsdHlwZToi|
      && |bmljQ29kZUJ1dHRvbiJ9fX0sbmljQ29kZUJ1dHRvbj1uaWNFZGl0b3JBZHZhbmNlZEJ1dHRvbi5leHRlbmQoe3dpZHRoOiIzNTBweCIsYWRkUGFuZTpmdW5j|
      && |dGlvbigpe3RoaXMuYWRkRm9ybSh7IiI6e3R5cGU6InRpdGxlIix0eHQ6IkVkaXQgSFRNTCJ9LGNvZGU6e3R5cGU6ImNvbnRlbnQiLHZhbHVlOnRoaXMubmUu|
      && |c2VsZWN0ZWRJbnN0YW5jZS5nZXRDb250ZW50KCksc3R5bGU6e3dpZHRoOiIzNDBweCIsaGVpZ2h0OiIyMDBweCJ9fX0pfSxzdWJtaXQ6ZnVuY3Rpb24odCl7|
      && |dmFyIGU9dGhpcy5pbnB1dHMuY29kZS52YWx1ZTt0aGlzLm5lLnNlbGVjdGVkSW5zdGFuY2Uuc2V0Q29udGVudChlKSx0aGlzLnJlbW92ZVBhbmUoKX19KTtu|
      && |aWNFZGl0b3JzLnJlZ2lzdGVyUGx1Z2luKG5pY1BsdWdpbixuaWNDb2RlT3B0aW9ucyk7|.

    DATA(lt_html) = VALUE string_table(
      ( `<html id="html">                                                                                                        ` )
      ( `<head>                                                                                                                  ` )
      ( `<meta charset="utf-8">                                                                                                  ` )
      ( `<title>NicEditor for ABAP</title>                                                                                       ` )
      ( `<script>                                                                                                                ` )
      ( `/* NicEdit - Micro Inline WYSIWYG                                                                                       ` )
      ( ` * Copyright 2007-2008 Brian Kirchoff                                                                                   ` )
      ( ` *                                                                                                                      ` )
      ( ` * NicEdit is distributed under the terms of the MIT license                                                            ` )
      ( ` * For more information visit http://nicedit.com/                                                                       ` )
      ( ` * Do not remove this copyright message                                                                                 ` )
      ( ` */                                                                                                                     ` )
      ( cl_http_utility=>decode_base64( lv_nic_script_base64 )                                                                     )
      ( ` </script>                                                                                                              ` )
      ( `                                                                                                                        ` )
      ( `<script type="text/javascript">                                                                                         ` )
      ( `function sendToSAP( html, eventName ) {                                                                                 ` )
      ( `    var FormForSAP        = document.createElement("form");                                                             ` )
      ( `        FormForSAP.action = "SAPEVENT:on_"+eventName;                                                                   ` )
      ( `        FormForSAP.method = "post";                                                                                     ` )
      ( `        FormForSAP.id     = "FormForSAP";                                                                               ` )
      ( `                                                                                                                        ` )
      ( `    var iterator = 1;                                                                                                   ` )
      ( `    while (html.length > 200) {                                                                                         ` )
      ( `        var content         = document.createElement("input");                                                          ` )
      ( `           content.type     = "hidden";                                                                                 ` )
      ( `           content.name     = "html" + iterator;                                                                        ` )
      ( `           content.id       = "html" + iterator;                                                                        ` )
      ( `           content.value    = html.slice(0,200);                                                                        ` )
      ( `           content.readonly = "readonly";                                                                               ` )
      ( `           FormForSAP.appendChild(content);                                                                             ` )
      ( `           html = html.substring(200, html.length + 1 );                                                                ` )
      ( `           iterator = iterator + 1;                                                                                     ` )
      ( `    }                                                                                                                   ` )
      ( `    if ( html.length <= 200) {                                                                                          ` )
      ( `           var content      = document.createElement("input");                                                          ` )
      ( `           content.type     = "hidden";                                                                                 ` )
      ( `           content.name     = "html" + iterator;                                                                        ` )
      ( `           content.id       = "html" + iterator;                                                                        ` )
      ( `           content.value    = html;                                                                                     ` )
      ( `           content.readonly = "readonly";                                                                               ` )
      ( `           FormForSAP.appendChild(content);                                                                             ` )
      ( `    }                                                                                                                   ` )
      ( `                                                                                                                        ` )
      ( `    var fDiv=document.getElementById("formDiv");                                                                        ` )
      ( `    fDiv.appendChild(FormForSAP);                                                                                       ` )
      ( `    FormForSAP.submit();                                                                                                ` )
      ( `}                                                                                                                       ` )
      ( `</script>                                                                                                               ` )
      ( `</head>                                                                                                                 ` )
      ( `<body>                                                                                                                  ` )
      ( `                                                                                                                        ` )
      ( `<div id="Editor" style="width:100%;height:100%">                                                                        ` )
      ( `<textarea id="htmlarea" style="height:&EditorHeight&;width:&EditorWidth&;">&TextToReplace&</textarea>                   ` )
      ( `<div id="formDiv" style="height:0px;top:00px"></div>                                                                    ` )
      ( `</div>                                                                                                                  ` )
      ( `                                                                                                                        ` )
      ( `<script type="text/javascript">                                                                                         ` )
      ( `var area;                                                                                                               ` )
      ( `                                                                                                                        ` )
      ( `function addArea() {                                                                                                    ` )
      ( `  area = new nicEditor({fullPanel : true, onSave : function(content, id, instance) {                                    ` )
      ( `    sendToSAP( content, 'save' );                                                                                       ` )
      ( `  } });                                                                                                                 ` )
      ( `  area.panelInstance('htmlarea',{hasPanel : true});                                                                     ` )
      ( `  window.addEventListener('blur', function() {                                                                          ` )
      ( `    var content = area.nicInstances[0].getContent();                                                                    ` )
      ( `    sendToSAP( content, 'blur' );                                                                                       ` )
      ( `  });                                                                                                                   ` )
      ( `}                                                                                                                       ` )
      ( `                                                                                                                        ` )
      ( `bkLib.onDomLoaded(function() { addArea(); });                                                                           ` )
      ( `</script>                                                                                                               ` )
      ( `                                                                                                                        ` )
      ( `</body>                                                                                                                 ` )
      ( `</html>                                                                                                                 ` )
    ).
    mv_editor_source_template = concat_lines_of( table = lt_html sep = cl_abap_char_utilities=>newline ).
  ENDMETHOD.


  METHOD on_sapevent.
    CHECK action EQ 'on_save'
       OR action EQ 'on_blur'.

    mv_content = REDUCE #( INIT s = || FOR wa IN query_table NEXT s = s && wa-value ).

    IF action EQ 'on_save'.
      RAISE EVENT save
        EXPORTING
          iv_html = mv_content.
    ENDIF.

    refresh( ).
  ENDMETHOD.


  METHOD zif_gui_text_view~display.
    mv_content = iv_content.
    refresh( ).
  ENDMETHOD.


  METHOD zif_gui_text_view~free.
    IF mo_html_viewer IS BOUND.
      mo_html_viewer->free( ).
    ENDIF.
    FREE mo_html_viewer.
  ENDMETHOD.


  METHOD zif_gui_text_view~refresh.
    init_editor_source( ).

    IF mv_assigned_url IS INITIAL.
      CALL METHOD mo_html_viewer->set_registered_events
        EXPORTING
          events = VALUE #( ( eventid = mo_html_viewer->m_id_sapevent appl_event = 'x' ) ).
      SET HANDLER on_sapevent FOR mo_html_viewer.
    ENDIF.
    CALL METHOD mo_html_viewer->load_data
      EXPORTING
        url                    = mv_assigned_url
        type                   = 'text'
        subtype                = 'html'
      IMPORTING
        assigned_url           = mv_assigned_url
      CHANGING
        data_table             = mt_editor_source
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
