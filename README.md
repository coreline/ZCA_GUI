# ZCA_GUI — библиотека для работы с GUI
## ZCL_GUI_SCREEN — класс для рисования экранов на чистом ООП
### Возможности:
- Создание экрана с контейнером и возможность его переиспользования
- Создание кнопок на GUI-статусе через ABAP код
- Определение PBO/PAI экрана через события класса
- Возможность вывода в полноэкранном режиме и в модальном окне
- Не требует создания экранов, GUI-заголовков/статусов, PBO/PAI модулей
- Класс можно использовать как через наследование, так и через композицию

**Пример экрана с выводом 2-х таблиц. С кнопкой, которая открывает новый экран в модальном окне.**
``` abap
CLASS lcl_app DEFINITION
  INHERITING FROM zcl_gui_screen.
  PUBLIC SECTION.
    METHODS constructor.
    
  PRIVATE SECTION.
    METHODS on_init FOR EVENT init OF zcl_gui_screen
      IMPORTING io_container.
    METHODS on_exit FOR EVENT exit OF zcl_gui_screen
      IMPORTING iv_command.
    METHODS on_pbo FOR EVENT pbo OF zcl_gui_screen.
    METHODS on_pai FOR EVENT pai OF zcl_gui_screen
      IMPORTING iv_command.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    set_title( 'ZCL_GUI_SCREEN - демо' ).
    SET HANDLER on_init FOR me.
    SET HANDLER on_exit FOR me.
    SET HANDLER on_pbo FOR me.
    SET HANDLER on_pai FOR me.
  ENDMETHOD.

  METHOD on_init. " Инициализация, первый вызов в PBO
    " Делим экран пополам и выводим в них таблицы
    DATA(lo_splitter) = NEW cl_gui_splitter_container(
      parent  = io_container
      rows    = 1
      columns = 2
    ).
    DATA(lo_box1) = lo_splitter->get_container( row = 1 column = 1 ).
    DATA(lo_box2) = lo_splitter->get_container( row = 1 column = 2 ).

    SELECT * FROM spfli
      INTO TABLE @DATA(lt_spfli).

    SELECT * FROM scarr
      INTO TABLE @DATA(lt_scarr).

    CALL METHOD cl_salv_table=>factory
      EXPORTING
        r_container  = lo_box1
      IMPORTING
        r_salv_table = DATA(lo_grid1)
      CHANGING
        t_table      = lt_spfli.

    CALL METHOD cl_salv_table=>factory
      EXPORTING
        r_container  = lo_box2
      IMPORTING
        r_salv_table = DATA(lo_grid2)
      CHANGING
        t_table      = lt_scarr.

    lo_grid1->display( ).
    lo_grid2->display( ).

    " Добавляем кнопку в GUI-статус
    create_function(
      iv_command = 'CREATE_POPUP'
      iv_icon    = icon_create
      iv_text    = 'Новое окно'
    ).
  ENDMETHOD.

  METHOD on_exit. " Выход, последний вызов в PAI
    MESSAGE iv_command TYPE rs_c_success.
  ENDMETHOD.

  METHOD on_pbo. " Логика PBO
  ENDMETHOD.

  METHOD on_pai. " Логика PAI
    CASE iv_command.
      WHEN 'CREATE_POPUP'.
        DATA(lo_screen) = NEW lcl_app( ).
        lo_screen->create_function(
          iv_command = 'CLOSE'
          iv_icon    = icon_close
          iv_text    = 'Закрыть'
        ).
        lo_screen->popup(
          iv_row    = 2
          iv_col    = 2
          iv_width  = 150
          iv_height = 20
        ).
      WHEN 'CLOSE'.
        SET SCREEN 0.
      WHEN OTHERS.
        MESSAGE iv_command TYPE rs_c_success.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_app( )->display( ).
```
![](https://raw.githubusercontent.com/coreline/ZCA_GUI/main/img/1.png)
![](https://raw.githubusercontent.com/coreline/ZCA_GUI/main/img/2.png)

## ZCL_GUI_SCREEN_ALV — экран с таблицей ALV
Интерфейс класса напоминает ZCL_GUI_SCREEN
Событие INIT передает в управление созданный экземпляр ZCL_GUI_SCREEN и предопределенные данные для его отображения, которые можно кастомизировать под конкретную задачу
**Пример быстрого вывода таблицы с данными**
``` abap
SELECT * FROM sbook 
  INTO TABLE @DATA(lt_data).
NEW zcl_gui_screen_alv( REF #( lt_data ) )->display( ).
``` 
![](https://raw.githubusercontent.com/coreline/ZCA_GUI/main/img/4.png)

**Пример с включением стандартных копок и добавлением Z-кнопок в GUI-статус**
``` abap
CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    METHODS run.

  PRIVATE SECTION.
    METHODS on_init FOR EVENT init OF zcl_gui_screen_alv
      IMPORTING io_grid ir_sets.
    METHODS on_pai FOR EVENT pai OF zcl_gui_screen_alv
      IMPORTING iv_command.
    METHODS on_double_click FOR EVENT double_click OF zcl_gui_screen_alv
      IMPORTING ir_data iv_column iv_row.

    DATA mo_screen TYPE REF TO zcl_gui_screen_alv.
    DATA mt_outtab TYPE TABLE OF spfli.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD constructor.
    SELECT * FROM spfli
      INTO TABLE @mt_outtab.

    mo_screen = NEW #( REF #( mt_outtab ) ).
    SET HANDLER on_init FOR mo_screen.
    SET HANDLER on_pai FOR mo_screen.
    SET HANDLER on_double_click FOR mo_screen.
    mo_screen->set_title( 'Удобный вывод ALV' ).
    mo_screen->display( ).
  ENDMETHOD.

  METHOD run.
  ENDMETHOD.

  METHOD on_init.
    " IO_GRID - здесь нам доступна созданная инстанция ALV
    " IR_SETS - здесь доступны настройки отображения ALV
    " - SAVE
    " - S_VARIANT
    " - S_LAYOUT
    " - T_TOOLBAR_EXCLUDING
    " - T_FIELDCATALOG
    " - T_SORT
    " - T_FILTER

    " Добавляем возможность сохранения варианта
    ir_sets->s_variant-report = sy-repid.
    ir_sets->s_variant-handle = 'MAIN'.

    " Включаем кнопки сохранения и обновления таблицы
    mo_screen->enable_function( zif_gui_screen=>mc_button_save ).
    mo_screen->enable_function( zif_gui_screen=>mc_button_refresh ).

    " Добавляем новую кнопку
    mo_screen->create_function(
      iv_command = 'NEW_BUTTON'
      iv_icon    = icon_abc
      iv_text    = 'Новая кнопка'
      iv_tooltip = 'Всплывающая подсказка'
    ).
  ENDMETHOD.

  METHOD on_pai.
    CASE iv_command.
      WHEN zif_gui_screen=>mc_button_refresh.
        mo_screen->refresh( ).
      WHEN OTHERS.
        MESSAGE |PAI: { iv_command }| TYPE rs_c_success.
    ENDCASE.
  ENDMETHOD.

  METHOD on_double_click.
    MESSAGE |Double Click: { iv_column }[{ iv_row }]| TYPE rs_c_success.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_app( )->run( ).
``` 
![](https://raw.githubusercontent.com/coreline/ZCA_GUI/main/img/3.png)

## ZCL_GUI_SCREEN_TEXTEDIT — экран с текстовым редактором
Интерфейс класса напоминает ZCL_GUI_SCREEN
Событие INIT передает в управление созданный экземпляр CL_GUI_TEXTEDIT

**Пример POPUP-окна с кнопкой в GUI-статусе**
``` abap
CLASS lcl_editor DEFINITION
  INHERITING FROM zcl_gui_screen_textedit.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_text TYPE csequence.

  PRIVATE SECTION.
    METHODS on_pai FOR EVENT pai OF zcl_gui_screen_textedit
      IMPORTING iv_command.
ENDCLASS.

CLASS lcl_editor IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_text ).
    create_function(
      iv_command = 'CLOSE'
      iv_icon    = icon_close
      iv_text    = 'Закрыть'
    ).
    SET HANDLER on_pai FOR me.
  ENDMETHOD.

  METHOD on_pai.
    IF iv_command EQ 'CLOSE'.
      SET SCREEN 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


START-OF-SELECTION.
  DATA(lo_editor) = NEW lcl_editor( 'Начальный текст' ).
  lo_editor->popup(
    iv_col    = 2
    iv_row    = 2
    iv_width  = 80
    iv_height = 20
  ).
  MESSAGE |{ lo_editor->get_text( ) }| TYPE rs_c_info.
``` 
![](https://raw.githubusercontent.com/coreline/ZCA_GUI/main/img/5.png)
![](https://raw.githubusercontent.com/coreline/ZCA_GUI/main/img/6.png)

## ZIF_GUI_DATA_VIEW — Интерфейс отображения данных
Интерфейс предназначен для отображения данных в различных представлениях. Принцип работы схож с принципом GUI-элементов, которые размещаются на контейнере.
### Реализации:
- ZCL_GUI_DATA_VIEW_XML — Класс для отображения данных в XML-представлении
- ZCL_GUI_DATA_VIEW_JSON — Класс для отображения данных в JSON-формате
- ZCL_GUI_DATA_VIEW_TREE — Класс для отображения данных в виде дерева
- ZCL_GUI_DATA_VIEW_COMPOSITE — Класс для отображения данных во всех перечисленных форматах

**Пример вывода таблицы с данными через класс ZCL_GUI_DATA_VIEW_COMPOSITE**
``` abap
CLASS lcl_app DEFINITION
  INHERITING FROM zcl_gui_screen.
  PUBLIC SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    METHODS on_init FOR EVENT init OF zcl_gui_screen
      IMPORTING io_container.

    DATA mo_viewer TYPE REF TO zif_gui_data_view.
    DATA mt_data TYPE TABLE OF spfli.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    SELECT *
      INTO TABLE mt_data
      FROM spfli.
    SET HANDLER on_init FOR me.
  ENDMETHOD.

  METHOD on_init.
    mo_viewer = NEW zcl_gui_data_view_composite( io_container ).
    CALL METHOD mo_viewer->display
      CHANGING
        c_data = mt_data.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_app( )->display( ).
``` 
![](https://raw.githubusercontent.com/coreline/ZCA_GUI/main/img/7.png)
![](https://raw.githubusercontent.com/coreline/ZCA_GUI/main/img/8.png)
![](https://raw.githubusercontent.com/coreline/ZCA_GUI/main/img/9.png)
![](https://raw.githubusercontent.com/coreline/ZCA_GUI/main/img/10.png)

## ZIF_GUI_TEXT_VIEW — Интерфейс отображения текста
Интерфейс предназначен для отображения текстовых данных. Принцип работы схож с принципом GUI-элементов, которые размещаются на контейнере.
### Реализации:
- ZCL_GUI_HTML_VIEWER — отображение текста в HTML браузере
- ZCL_GUI_NIC_EDITOR — отображение текста в HTML-редакторе Nic Editor

**Пример отображения текста в HTML-контейнере**
``` abap
CLASS lcl_app DEFINITION
  INHERITING FROM zcl_gui_screen.
  PUBLIC SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    METHODS on_init FOR EVENT init OF zcl_gui_screen
      IMPORTING io_container.

    DATA mo_viewer TYPE REF TO zif_gui_text_view.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    SET HANDLER on_init FOR me.
  ENDMETHOD.

  METHOD on_init.
    mo_viewer = NEW zcl_gui_html_viewer( io_container ).
    mo_viewer->display( 'Привет, мир!' ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_app( )->display( ).
``` 
![](https://raw.githubusercontent.com/coreline/ZCA_GUI/main/img/11.png)

**Пример отображения текста в HTML-редакторе Nic Editor**
``` abap
CLASS lcl_app DEFINITION
  INHERITING FROM zcl_gui_screen.
  PUBLIC SECTION.
    METHODS constructor.

  PRIVATE SECTION.
    METHODS on_init FOR EVENT init OF zcl_gui_screen
      IMPORTING io_container.

    DATA mo_viewer TYPE REF TO zif_gui_text_view.
ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    SET HANDLER on_init FOR me.
  ENDMETHOD.

  METHOD on_init.
    mo_viewer = NEW zcl_gui_nic_editor( io_container ).
    mo_viewer->display( 'Привет, мир!' ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_app( )->display( ).
``` 
![](https://raw.githubusercontent.com/coreline/ZCA_GUI/main/img/12.png)