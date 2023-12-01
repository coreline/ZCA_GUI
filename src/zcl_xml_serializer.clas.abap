"! Сериализация данных в XML
class ZCL_XML_SERIALIZER definition
  public
  final
  create public .

public section.

  types MTV_PRETTY_MODE type I .
  types:
    BEGIN OF mts_name_mapping,
        abap TYPE abap_compname,
        xml  TYPE string,
      END OF mts_name_mapping .
  types:
    mtht_name_mappings TYPE HASHED TABLE OF mts_name_mapping WITH UNIQUE KEY abap .

  constants MC_PRETTY_MODE_CAMEL_CASE type MTV_PRETTY_MODE value 1 ##NO_TEXT.
  constants MC_PRETTY_MODE_SNAKE_CASE type MTV_PRETTY_MODE value 2 ##NO_TEXT.
  constants MC_PRETTY_MODE_UPPER_CASE type MTV_PRETTY_MODE value 3 ##NO_TEXT.

    "! Сериализовать данные в XML
  class-methods SERIALIZE
    importing
      !I_DATA type ANY
      !IV_XML_DECL type STRING optional
      !IV_ROOT_TAG type STRING optional
      !IV_ARRAY_ITEM_TAG type STRING default 'item'
      !IV_NAMESPACE type STRING optional
      !IV_PRETTY_NAME type MTV_PRETTY_MODE default MC_PRETTY_MODE_SNAKE_CASE
      !IV_COMPRESS type ABAP_BOOL optional
      !IT_NAME_MAPPINGS type MTHT_NAME_MAPPINGS optional
    returning
      value(RV_XML) type STRING .
    "! Десериализовать данные из XML
    "!
    "! @raising   cx_ixml_parse_error | iXML Exception: Error While Parsing
  class-methods DESERIALIZE
    importing
      !IV_XML type STRING
      !IV_PRETTY_NAME type MTV_PRETTY_MODE optional
      !IT_NAME_MAPPINGS type MTHT_NAME_MAPPINGS optional
    exporting
      !E_DATA type ANY
    raising
      CX_IXML_PARSE_ERROR .
  PROTECTED SECTION.
    "! Сериализовать данные в XML
    METHODS serialize_internal
      IMPORTING
        !i_data            TYPE any
        !iv_xml_decl       TYPE string OPTIONAL
        !iv_root_tag       TYPE string OPTIONAL
        !iv_array_item_tag TYPE string OPTIONAL
        !iv_namespace      TYPE string OPTIONAL
        !iv_pretty_name    TYPE mtv_pretty_mode OPTIONAL
        !iv_compress       TYPE abap_bool OPTIONAL
        !it_name_mappings  TYPE mtht_name_mappings OPTIONAL
      RETURNING
        VALUE(rv_xml)      TYPE string .
    "! Десериализовать данные из XML
    "!
    "! @raising   cx_ixml_parse_error | iXML Exception: Error While Parsing
    METHODS deserialize_internal
      IMPORTING
        !iv_xml           TYPE string
        !iv_pretty_name   TYPE mtv_pretty_mode OPTIONAL
        !it_name_mappings TYPE mtht_name_mappings OPTIONAL
      EXPORTING
        !e_data           TYPE any
      RAISING
        cx_ixml_parse_error .

  PRIVATE SECTION.
    TYPES:
      BEGIN OF mts_xml_pair,
        key TYPE string,
        val TYPE string,
      END OF mts_xml_pair .
    TYPES:
      BEGIN OF mts_xml_base,
        xml_node  TYPE REF TO if_ixml_node,
        node_type TYPE i,
        value     TYPE string,
      END OF mts_xml_base .
    TYPES:
      BEGIN OF mts_xml_attr,
        attr_name TYPE string.
        INCLUDE TYPE mts_xml_base AS base.
  TYPES END OF mts_xml_attr .
    TYPES:
      BEGIN OF mts_xml_node,
        tag_name TYPE string,
        attrs    TYPE TABLE OF mts_xml_attr WITH KEY attr_name,
        childs   TYPE REF TO data.
        INCLUDE TYPE mts_xml_base AS base.
  TYPES END OF mts_xml_node .
    TYPES:
      mtt_xml_nodes TYPE TABLE OF mts_xml_node WITH KEY tag_name .
    TYPES:
      mtt_xml_attrs TYPE TABLE OF mts_xml_pair WITH KEY key .
    DATA mv_pretty_name TYPE mtv_pretty_mode .
    DATA mv_compress TYPE flag .
    DATA mv_array_item_tag TYPE string .
    DATA mht_name_mappings TYPE mtht_name_mappings.

    "! Установить значение XML в ABAP данные
    METHODS convert_node_to_data
      IMPORTING
        !is_node TYPE mts_xml_node
      EXPORTING
        !e_data  TYPE any .
    "! Строка атрибутов
    METHODS get_attribute_string
      IMPORTING
        !it_attr       TYPE mtt_xml_attrs
      RETURNING
        VALUE(rv_attr) TYPE string .
    "! Компоненты структуры
    METHODS get_structure_components
      IMPORTING
        !is_struct          TYPE any
      RETURNING
        VALUE(rt_component) TYPE abap_component_tab .
    "! Сформировать XML-тэг
    "!
    "! @parameter rt_xml | Таблица стрингов
    METHODS build_tag
      IMPORTING
        !iv_name      TYPE clike
        !i_value      TYPE any
        !it_attr      TYPE mtt_xml_attrs OPTIONAL
        !iv_indent    TYPE i DEFAULT 0
      RETURNING
        VALUE(rt_xml) TYPE string_table .
    "! Конвертирует имя в указанный формат
    METHODS get_pretty_name
      IMPORTING
        !iv_value       TYPE string
      RETURNING
        VALUE(rv_value) TYPE string .
    "! Преобразует значение в строку
    METHODS to_string
      IMPORTING
        !i_value        TYPE any
      RETURNING
        VALUE(rv_value) TYPE string .
    "! Конвертирует ABAP поле в XML имя тэга/аттрибута
    METHODS get_xml_name
      IMPORTING
        !iv_name        TYPE string
        !is_attr        TYPE abap_bool DEFAULT space
      RETURNING
        VALUE(rv_value) TYPE string .
    "! Конвертирует XML имя тэга/аттрибута в ABAP-поле
    METHODS get_abap_name
      IMPORTING
        !iv_name        TYPE string
        !is_data        TYPE any
        !is_attr        TYPE abap_bool DEFAULT space
      RETURNING
        VALUE(rv_value) TYPE string .
    "! Парсить XML-элемент
    "!
    "! @parameter io_node | Node of DOM
    METHODS parse_node
      IMPORTING
        !io_node       TYPE REF TO if_ixml_node
      RETURNING
        VALUE(rs_node) TYPE mts_xml_node .
    METHODS snake_to_camel
      IMPORTING
        !iv_value       TYPE string
      RETURNING
        VALUE(rv_value) TYPE string .
    METHODS camel_to_snake
      IMPORTING
        !iv_value       TYPE string
      RETURNING
        VALUE(rv_value) TYPE string .
ENDCLASS.



CLASS ZCL_XML_SERIALIZER IMPLEMENTATION.


  METHOD build_tag.
    CONSTANTS lc_indent_char TYPE string VALUE `  ` ##NO_TEXT.
    DATA lt_attr TYPE mtt_xml_attrs.
    DATA lv_value TYPE string.
    DATA lv_indent_string TYPE string.
    DATA lv_type TYPE c.
    FIELD-SYMBOLS <lt_table> TYPE ANY TABLE.
    FIELD-SYMBOLS <ls_field> TYPE abap_componentdescr.
    FIELD-SYMBOLS <l_value> TYPE any.

    CHECK mv_compress IS INITIAL
       OR i_value IS NOT INITIAL
       OR it_attr IS NOT INITIAL.
    CHECK iv_name NA '/'.

    lt_attr[] = it_attr[].
    lv_indent_string = repeat( val = lc_indent_char occ = iv_indent ).

    DESCRIBE FIELD i_value TYPE lv_type.
    IF lv_type EQ 'h'. " Internal table
      ASSIGN i_value TO <lt_table>.
      IF mv_array_item_tag IS INITIAL.
        LOOP AT <lt_table> ASSIGNING <l_value>.
          APPEND LINES OF build_tag(
              iv_name   = iv_name
              i_value   = <l_value>
              it_attr   = lt_attr
              iv_indent = iv_indent
          ) TO rt_xml.
        ENDLOOP.
      ELSE.
        APPEND |{ lv_indent_string }<{ iv_name }>| TO rt_xml.
        LOOP AT <lt_table> ASSIGNING <l_value>.
          APPEND LINES OF build_tag(
              iv_name   = mv_array_item_tag
              i_value   = <l_value>
              it_attr   = lt_attr
              iv_indent = iv_indent + 1
          ) TO rt_xml.
        ENDLOOP.
        APPEND |{ lv_indent_string }</{ iv_name }>| TO rt_xml.
      ENDIF.
      RETURN.
    ENDIF.

    IF lv_type CA 'uv'. " Struct: Float/Deep
      DATA(lt_component) = get_structure_components( i_value ).
      LOOP AT lt_component INTO DATA(ls_field) WHERE name CP '_*'.
        DELETE lt_component.
        ASSIGN COMPONENT ls_field-name OF STRUCTURE i_value TO <l_value>.
        CHECK mv_compress IS INITIAL OR <l_value> IS NOT INITIAL.
        APPEND VALUE #(
            key = get_xml_name( iv_name = ls_field-name is_attr = abap_true )
            val = to_string( <l_value> )
        ) TO lt_attr.
      ENDLOOP.
      IF lines( lt_component ) EQ 1 AND lines( lt_attr ) GE 1.
        READ TABLE lt_component INTO DATA(ls_component) INDEX 1.
        ASSIGN COMPONENT ls_component-name OF STRUCTURE i_value TO <l_value>.
        IF ls_component-name EQ 'VALUE'.
          APPEND LINES OF build_tag(
              iv_name   = iv_name
              i_value   = <l_value>
              it_attr   = lt_attr
              iv_indent = iv_indent
          ) TO rt_xml.
          RETURN.
        ENDIF.
      ENDIF.
      CHECK mv_compress IS INITIAL OR ( lines( lt_component ) + lines( lt_attr ) GT 0 ).

      IF lt_component IS INITIAL.
        APPEND |{ lv_indent_string }<{ iv_name }{ get_attribute_string( lt_attr ) }/>| TO rt_xml.
      ELSE.
        APPEND |{ lv_indent_string }<{ iv_name }{ get_attribute_string( lt_attr ) }>| TO rt_xml.
        LOOP AT lt_component INTO ls_component.
          ASSIGN COMPONENT ls_component-name OF STRUCTURE i_value TO <l_value>.
          APPEND LINES OF build_tag(
              iv_name   = get_xml_name( ls_component-name )
              i_value   = <l_value>
              iv_indent = iv_indent + 1
          ) TO rt_xml.
        ENDLOOP.
        APPEND |{ lv_indent_string }</{ iv_name }>| TO rt_xml.
      ENDIF.
      RETURN.
    ENDIF.

    IF i_value IS INITIAL.
      APPEND |{ lv_indent_string }<{ iv_name }{ get_attribute_string( lt_attr ) }/>| TO rt_xml.
    ELSE.
      APPEND |{ lv_indent_string }<{ iv_name }{ get_attribute_string( lt_attr ) }>{
        escape( val = to_string( i_value ) format = cl_abap_format=>e_xml_text )
      }</{ iv_name }>| TO rt_xml.
    ENDIF.
  ENDMETHOD.


  METHOD camel_to_snake.
    DATA lv_text TYPE string.

    lv_text = iv_value.
    lv_text = replace( val = lv_text occ = 0 regex = '\s' with = space ) ##REGEX_POSIX.
    lv_text = replace( val = lv_text occ = 0 regex = '\u' with = '_$0' ) ##REGEX_POSIX.
    lv_text = replace( val = lv_text occ = 0 regex = '_+' with = '_' ) ##REGEX_POSIX.
    lv_text = replace( val = lv_text occ = 0 regex = '^_+' with = space ) ##REGEX_POSIX.
    lv_text = replace( val = lv_text occ = 0 regex = '_+$' with = space ) ##REGEX_POSIX.
    lv_text = to_upper( lv_text ).
    rv_value = lv_text.
  ENDMETHOD.


  METHOD convert_node_to_data.
    DATA lt_nodes TYPE mtt_xml_nodes.
    DATA lo_row TYPE REF TO data.
    FIELD-SYMBOLS <lt_nodes> TYPE mtt_xml_nodes.
    FIELD-SYMBOLS <l_table> TYPE ANY TABLE.
    FIELD-SYMBOLS <l_row> TYPE any.
    FIELD-SYMBOLS <l_field> TYPE any.

    ASSIGN is_node-childs->* TO <lt_nodes>.
    IF sy-subrc NE 0.
      ASSIGN lt_nodes TO <lt_nodes>.
    ENDIF.

    DESCRIBE FIELD e_data TYPE DATA(lv_type).
    CASE lv_type.
      WHEN cl_abap_typedescr=>typekind_table.
        ASSIGN e_data TO <l_table>.
        CREATE DATA lo_row LIKE LINE OF <l_table>.
        ASSIGN lo_row->* TO <l_row>.

        CALL METHOD convert_node_to_data
          EXPORTING
            is_node = is_node
          IMPORTING
            e_data  = <l_row>.
        IF <l_row> IS INITIAL.
          LOOP AT <lt_nodes> ASSIGNING FIELD-SYMBOL(<ls_node>)
            WHERE node_type EQ if_ixml_node=>co_node_element.
            CLEAR <l_row>.
            CALL METHOD convert_node_to_data
              EXPORTING
                is_node = <ls_node>
              IMPORTING
                e_data  = <l_row>.
            INSERT <l_row> INTO TABLE <l_table>.
          ENDLOOP.
        ELSE.
          INSERT <l_row> INTO TABLE <l_table>.
        ENDIF.

      WHEN cl_abap_typedescr=>typekind_struct1
        OR cl_abap_typedescr=>typekind_struct2.
        ASSIGN COMPONENT 'VALUE' OF STRUCTURE e_data TO <l_field>.
        IF sy-subrc EQ 0.
          CALL METHOD convert_node_to_data
            EXPORTING
              is_node = VALUE #( base = is_node-base )
            IMPORTING
              e_data  = <l_field>.
        ENDIF.
        LOOP AT is_node-attrs ASSIGNING FIELD-SYMBOL(<ls_attrs>).
          DATA(lv_attr_field) = get_abap_name(
              iv_name = <ls_attrs>-attr_name
              is_data = e_data
              is_attr = abap_true
          ).
          CHECK lv_attr_field IS NOT INITIAL.
          ASSIGN COMPONENT lv_attr_field OF STRUCTURE e_data TO <l_field>.
          CALL METHOD convert_node_to_data
            EXPORTING
              is_node = VALUE #( base = <ls_attrs>-base )
            IMPORTING
              e_data  = <l_field>.
        ENDLOOP.
        LOOP AT <lt_nodes> ASSIGNING FIELD-SYMBOL(<ls_child>).
          DATA(lv_tag_field) = get_abap_name(
              iv_name = <ls_child>-tag_name
              is_data = e_data
          ).
          CHECK lv_tag_field IS NOT INITIAL.
          ASSIGN COMPONENT lv_tag_field OF STRUCTURE e_data TO <l_field>.
          CALL METHOD convert_node_to_data
            EXPORTING
              is_node = <ls_child>
            IMPORTING
              e_data  = <l_field>.
        ENDLOOP.

      WHEN OTHERS.
        CHECK lines( <lt_nodes> ) LE 1.
        IF <lt_nodes> IS INITIAL.
          ASSIGN is_node-value TO <l_field>.
        ELSE.
          ASSIGN <lt_nodes>[ 1 ]-value TO <l_field>.
        ENDIF.
        IF lv_type CA 'DT'. " Date / Time
          e_data = replace( val = <l_field> regex = '\D' with = '' occ = 0 ) ##REGEX_POSIX.
        ELSE.
          e_data = <l_field>.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD deserialize.
    DATA lo_xml TYPE REF TO zcl_xml_serializer.

    lo_xml = NEW #( ).

    CALL METHOD lo_xml->deserialize_internal
      EXPORTING
        iv_xml           = iv_xml
        iv_pretty_name   = iv_pretty_name
        it_name_mappings = it_name_mappings
      IMPORTING
        e_data           = e_data.
  ENDMETHOD.


  METHOD deserialize_internal.
    DATA lo_ixml TYPE REF TO if_ixml.
    DATA lo_istream TYPE REF TO if_ixml_istream.
    DATA lo_stream_factory TYPE REF TO if_ixml_stream_factory.
    DATA lo_parser TYPE REF TO if_ixml_parser.
    DATA lo_document TYPE REF TO if_ixml_document.
    DATA lo_root_node TYPE REF TO if_ixml_node.

    CLEAR e_data.

    mv_pretty_name = COND #(
      WHEN iv_pretty_name EQ mc_pretty_mode_camel_case
        THEN mc_pretty_mode_snake_case
        ELSE mc_pretty_mode_upper_case
    ).
    mht_name_mappings = it_name_mappings.

    lo_ixml = cl_ixml=>create( ).
    lo_document = lo_ixml->create_document( ).
    lo_stream_factory = lo_ixml->create_stream_factory( ).
    lo_istream = lo_stream_factory->create_istream_string( iv_xml ).
    lo_parser = lo_ixml->create_parser(
        stream_factory = lo_stream_factory
        istream        = lo_istream
        document       = lo_document
    ).

    IF lo_parser->parse( ) EQ 0.
      lo_root_node ?= lo_document->get_root_element( ).
      CALL METHOD convert_node_to_data
        EXPORTING
          is_node = parse_node( lo_root_node )
        IMPORTING
          e_data  = e_data.
    ELSE.
      DATA(lo_parse_error) = lo_parser->get_error( index = 0 ).
      DATA(lv_reason) = lo_parse_error->get_reason( ).
      DATA(lv_line) = lo_parse_error->get_line( ).
      DATA(lv_column) = lo_parse_error->get_column( ).

      RAISE EXCEPTION TYPE cx_ixml_parse_error
        EXPORTING
          textid = cx_ixml_parse_error=>cx_ixml_parse_error
          reason = lv_reason
          line   = lv_line
          column = lv_column.
    ENDIF.
  ENDMETHOD.


  METHOD get_abap_name.
    DATA(lt_components) = get_structure_components( is_data ).
    IF mht_name_mappings IS NOT INITIAL.
      LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_component>).
        READ TABLE mht_name_mappings ASSIGNING FIELD-SYMBOL(<ls_map>)
          WITH TABLE KEY abap = <ls_component>-name.
        CHECK sy-subrc EQ 0.
        CHECK iv_name = <ls_map>-xml.
        rv_value = <ls_map>-abap.
        RETURN.
      ENDLOOP.
    ENDIF.
    IF is_attr EQ abap_true.
      rv_value = |_{ get_pretty_name( iv_name ) }|.
    ELSE.
      rv_value = get_pretty_name( iv_name ).
    ENDIF.
    IF NOT line_exists( lt_components[ name = rv_value ] ).
      CLEAR rv_value.
    ENDIF.
  ENDMETHOD.


  METHOD get_attribute_string.
    DATA lv_key TYPE string.
    DATA lv_val TYPE string.

    LOOP AT it_attr ASSIGNING FIELD-SYMBOL(<ls_attr>).
      lv_key = replace( val = <ls_attr>-key occ = 0 sub = '"' with = '' ).
      lv_val = escape( val = <ls_attr>-val format = cl_abap_format=>e_xml_attr ).
      rv_attr = |{ rv_attr } { lv_key }="{ to_string( lv_val ) }"|.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_pretty_name.
    CASE mv_pretty_name.
      WHEN mc_pretty_mode_snake_case.
        rv_value = camel_to_snake( iv_value ).
      WHEN mc_pretty_mode_camel_case.
        rv_value = snake_to_camel( iv_value ).
      WHEN mc_pretty_mode_upper_case.
        rv_value = to_upper( iv_value ).
      WHEN OTHERS.
        rv_value = iv_value.
    ENDCASE.
  ENDMETHOD.


  METHOD get_structure_components.
    DATA lo_structdescr TYPE REF TO cl_abap_structdescr.
    DATA lt_component TYPE cl_abap_structdescr=>component_table.
    DATA ls_component TYPE abap_componentdescr.

    DESCRIBE FIELD is_struct TYPE DATA(lv_type).
    ASSERT lv_type CA 'uv'. " Struct: Float/Deep

    lo_structdescr ?= cl_abap_structdescr=>describe_by_data( is_struct ).
    lt_component = lo_structdescr->get_components( ).

    LOOP AT lt_component INTO ls_component WHERE as_include EQ abap_true.
      lo_structdescr ?= ls_component-type.
      DELETE lt_component INDEX sy-tabix.
      INSERT LINES OF lo_structdescr->get_components( ) INTO lt_component INDEX sy-tabix.
    ENDLOOP.

    rt_component = lt_component.
  ENDMETHOD.


  METHOD get_xml_name.
    READ TABLE mht_name_mappings ASSIGNING FIELD-SYMBOL(<ls>)
      WITH TABLE KEY abap = iv_name.
    IF sy-subrc EQ 0.
      rv_value = <ls>-xml.
    ELSEIF is_attr EQ abap_true.
      rv_value = get_pretty_name( substring( val = iv_name off = 1 ) ).
    ELSE.
      rv_value = get_pretty_name( iv_name ).
    ENDIF.
  ENDMETHOD.


  METHOD parse_node.
    DATA(lv_name) = io_node->get_name( ).
    DATA(lo_attrs) = io_node->get_attributes( ).
    DATA(lo_childs) = io_node->get_children( ).
    FIELD-SYMBOLS <lt_childs> TYPE mtt_xml_nodes.

    rs_node = VALUE #( ).
    rs_node-tag_name = lv_name.
    rs_node-xml_node = io_node.
    rs_node-node_type = io_node->get_type( ).
    rs_node-value = io_node->get_value( ).

    IF lo_attrs IS BOUND.
      DATA(lo_attr_iter) = lo_attrs->create_iterator( ).
      DATA(lo_attr_node) = lo_attr_iter->get_next( ).
      WHILE lo_attr_node IS BOUND.
        DATA(lv_attr_name) = lo_attr_node->get_name( ).
        rs_node-attrs = VALUE #( BASE rs_node-attrs (
            attr_name   = lv_attr_name
            xml_node    = lo_attr_node
            node_type   = lo_attr_node->get_type( )
            value       = lo_attr_node->get_value( )
        ) ).
        lo_attr_node = lo_attr_iter->get_next( ).
      ENDWHILE.
    ENDIF.

    CREATE DATA rs_node-childs TYPE mtt_xml_nodes.
    ASSIGN rs_node-childs->* TO <lt_childs>.
    IF lo_childs IS BOUND.
      DATA(lo_childs_iter) = lo_childs->create_iterator( ).
      DATA(lo_childs_node) = lo_childs_iter->get_next( ).
      WHILE lo_childs_node IS BOUND.
        APPEND parse_node( lo_childs_node ) TO <lt_childs>.
        lo_childs_node = lo_childs_iter->get_next( ).
      ENDWHILE.
    ENDIF.
  ENDMETHOD.


  METHOD serialize.
    DATA lo_xml TYPE REF TO zcl_xml_serializer.

    lo_xml = NEW #( ).

    CALL METHOD lo_xml->serialize_internal
      EXPORTING
        i_data            = i_data
        iv_xml_decl       = iv_xml_decl
        iv_root_tag       = iv_root_tag
        iv_array_item_tag = iv_array_item_tag
        iv_namespace      = iv_namespace
        iv_pretty_name    = iv_pretty_name
        iv_compress       = iv_compress
        it_name_mappings  = it_name_mappings
      RECEIVING
        rv_xml            = rv_xml.
  ENDMETHOD.


  METHOD serialize_internal.
    CONSTANTS lc_xml_decl TYPE string VALUE '<?xml version="1.0"?>'.
    CONSTANTS lc_root_tag TYPE string VALUE 'Root'.
    DATA lt_xml TYPE string_table.
    DATA lt_attr TYPE mtt_xml_attrs.
    DATA lv_decl TYPE string.
    DATA lv_root TYPE string.

    mv_pretty_name = COND #( WHEN iv_pretty_name EQ mc_pretty_mode_camel_case
      THEN mc_pretty_mode_camel_case
      ELSE mc_pretty_mode_upper_case ).
    mv_compress = iv_compress.
    mv_array_item_tag = iv_array_item_tag.
    mht_name_mappings = it_name_mappings.
    lv_decl = COND #( WHEN iv_xml_decl IS NOT INITIAL THEN iv_xml_decl ELSE lc_xml_decl ).
    lv_root = COND #( WHEN iv_root_tag IS NOT INITIAL THEN iv_root_tag ELSE get_xml_name( lc_root_tag ) ).
    IF iv_namespace IS NOT INITIAL.
      lt_attr = VALUE #( ( key = 'xmlns' val = iv_namespace ) ).
    ENDIF.

    lt_xml = VALUE #( ( lv_decl ) ).
    APPEND LINES OF build_tag(
        i_value = i_data
        iv_name = lv_root
        it_attr = lt_attr
    ) TO lt_xml.
    rv_xml = concat_lines_of( table = lt_xml sep = cl_abap_char_utilities=>cr_lf ).
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


  METHOD to_string.
    DESCRIBE FIELD i_value TYPE DATA(lv_type).
    CASE lv_type.
      WHEN 'D'.
        rv_value = |{ CONV d( i_value ) DATE = ISO }|.
      WHEN 'b' OR 's' OR 'I' OR '8' OR 'P' OR 'a' OR 'e' OR 'F'.
        rv_value = condense( |{ i_value }| ).
      WHEN OTHERS.
        rv_value = |{ i_value }|.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
