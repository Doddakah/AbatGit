*&---------------------------------------------------------------------*
*&  Include           Z_FIM_SAD_EINVOICING_PROCESS
*&---------------------------------------------------------------------*



*&---------------------------------------------------------------------*
*&      Form  DISPLAY_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_report .

  DATA: lo_event_handler TYPE REF TO lcl_event_handler.


  TRY.
      CALL METHOD cl_salv_table=>factory
*        EXPORTING
*             list_display = abap_true
        IMPORTING
          r_salv_table = lo_gr_alv
        CHANGING
          t_table      = zcl_sd_saud_einvoicing=>gt_process.
    CATCH cx_salv_msg.
  ENDTRY.
*
  lo_gr_alv->set_screen_status(
  report        =  sy-repid
  pfstatus      =  'STANDARD_FULLSCREEN'
  set_functions = lo_gr_alv->c_functions_all ).

* Let's show all default buttons of ALV
  lo_gr_functions = lo_gr_alv->get_functions( ).
  lo_gr_functions->set_all( abap_true ).



* Fit the columns
  lo_columns = lo_gr_alv->get_columns( ).
*  lo_columns->set_optimize( 'X' ).



  CREATE OBJECT lo_grid.
  CREATE OBJECT lo_layout_logo.
  lo_grid->create_label( row = 1 column = 1 text = TEXT-002 tooltip = TEXT-002 ).
  lo_layout_logo->set_left_content( lo_grid ).
  lo_content = lo_layout_logo.
  lo_gr_alv->set_top_of_list( lo_content ).

* Apply zebra style to lv_rows
  lo_display = lo_gr_alv->get_display_settings( ).
  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).

* Enable the save layout buttons
  lv_key-report = sy-repid.
  lo_layout = lo_gr_alv->get_layout( ).
  lo_layout->set_key( lv_key ).
  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  lo_layout->set_default( abap_true ).

* Register events

*    DATA(lo_event_handler) = NEW lcl_event_handler( alv ).
**    CREATE OBJECT lo_event_handler.

  DATA: lo_events TYPE REF TO cl_salv_events_table,
        lo_event  TYPE REF TO cl_salv_events.

* Enable cell selection mode
  lo_events = lo_gr_alv->get_event( ).
*  lo_event = lo_gr_alv->get_event( ).
*    lo_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

*    set handler me->ON_LINK_CLICK for ALL INSTANCES.


  TRY.
      lo_column ?= lo_columns->get_column( 'QR_TXT' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_output_length('18').
      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lo_column->set_long_text( 'QR TEXT' ).
      lo_column->set_medium_text( 'QR TEXT' ).
      lo_column->set_short_text( 'QR TEXT' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.




  TRY.
      lo_column ?= lo_columns->get_column( 'VBELN' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lo_column->set_long_text( 'Billing Document Number' ).
      lo_column->set_medium_text( 'Billing Doc Number' ).
      lo_column->set_short_text( 'Bill Dno' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'CREDIT_TXT' ). " Find the 'MAKTX' column ans change attributes
      IF p_credit = abap_true.
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
      ELSEIF p_credit = abap_false.
        lo_column->set_visible( if_salv_c_bool_sap=>false ).
      ENDIF.
      lo_column->set_output_length('10').
      lo_column->set_long_text( 'Credit Text' ).
      lo_column->set_medium_text( 'Credit Text' ).
      lo_column->set_short_text( 'Crdt Txt' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'CREDIT_REF' ). " Find the 'MAKTX' column ans change attributes
      IF p_credit = abap_true.
        lo_column->set_visible( if_salv_c_bool_sap=>true ).
      ELSEIF p_credit = abap_false.
        lo_column->set_visible( if_salv_c_bool_sap=>false ).
      ENDIF.
      lo_column->set_output_length('12').
      lo_column->set_long_text( 'Credit Reference Document' ).
      lo_column->set_medium_text( 'Credit Ref Doc' ).
      lo_column->set_short_text( 'Crdt Ref' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.


  TRY.
      lo_column ?= lo_columns->get_column( 'AUBEL' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lo_column->set_long_text( 'Order Number' ).
      lo_column->set_medium_text( 'Order Number' ).
      lo_column->set_short_text( 'Order No' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'BLOB' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_output_length('03').
      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lo_column->set_long_text( 'Editable Text' ).
      lo_column->set_medium_text( 'Editable Text' ).
      lo_column->set_short_text( 'Edit Text' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.


  TRY.
      lo_column ?= lo_columns->get_column( 'DSIG_TXT' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_output_length('18').
      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lo_column->set_long_text( 'Digital Signature Text ' ).
      lo_column->set_medium_text( 'Digital Sign Text ' ).
      lo_column->set_short_text( 'Dig Txt' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'REJ_TXT' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lo_column->set_output_length('18').
      lo_column->set_long_text( 'Rejection  Text' ).
      lo_column->set_medium_text( 'Rejection Text' ).
      lo_column->set_short_text( 'Rejc Text' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'WAR_TXT' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lo_column->set_output_length('18').
      lo_column->set_long_text( 'Warning Text' ).
      lo_column->set_medium_text( 'Warning Text' ).
      lo_column->set_short_text( 'Warn Text' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'RES_FLG' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_cell_type( if_salv_c_cell_type=>checkbox ).
      lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
      lo_column->set_long_text( 'Resubmit Flag Box' ).
      lo_column->set_medium_text( 'Resubmit Flag Box' ).
      lo_column->set_short_text( 'Res Flg' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'CRDT_FLG' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_cell_type( if_salv_c_cell_type=>checkbox ).
      lo_column->set_cell_type( if_salv_c_cell_type=>checkbox_hotspot ).
      lo_column->set_long_text( 'Credit Flag Box' ).
      lo_column->set_medium_text( 'Credit Flag Box' ).
      lo_column->set_short_text( 'Crdt Flg' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'LIGHTS' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lo_column->set_long_text( 'E-Invoice Status' ).
      lo_column->set_medium_text( 'E-Invoice Status' ).
      lo_column->set_short_text( 'Einv Stat' ).
      lo_column->set_output_length('02').
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'VSTAT' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
*      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lo_column->set_long_text( 'Status' ).
      lo_column->set_medium_text( 'Status' ).
      lo_column->set_short_text( 'Status' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'VSTAT_TXT' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_output_length('15').
*      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lo_column->set_long_text( 'Status TEXT' ).
      lo_column->set_medium_text( 'Status TEXT' ).
      lo_column->set_short_text( 'Stat TxT' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'VSZTP' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
*      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lo_column->set_long_text( 'Date/Time' ).
      lo_column->set_medium_text( 'Date/Time' ).
      lo_column->set_short_text( 'Date/Time' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'VSZTP_TXT' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_output_length('35').
*      lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lo_column->set_long_text( 'Date/Time TEXT' ).
      lo_column->set_medium_text( 'Date/Time TXT' ).
      lo_column->set_short_text( 'Dt/Tm TXT' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'VKORG' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'VTWEG' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'SPART' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'NAME_ORG1' ). " Find the 'MAKTX' column ans change attributes
      lo_column->set_output_length('40').
      lo_column->set_visible( if_salv_c_bool_sap=>true ).
      lo_column->set_long_text( 'Sold to Party Name'  ).
      lo_column->set_medium_text(  'Sold to Party Name' ).
      lo_column->set_short_text(  'Sld t Part' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'MATNR' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
      lo_column->set_long_text( 'Product Code'  ).
      lo_column->set_medium_text( 'Product Code' ).
      lo_column->set_short_text(  'Prod Code' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  TRY.
      lo_column ?= lo_columns->get_column( 'MAKTX' ).
      lo_column->set_visible( if_salv_c_bool_sap=>false ).
      lo_column->set_long_text( 'Product Description'  ).
      lo_column->set_medium_text( 'Product Description' ).
      lo_column->set_short_text(  'Prod Desc' ).
    CATCH cx_salv_not_found.
    CATCH cx_salv_existing.
    CATCH cx_salv_data_error.
  ENDTRY.

  CREATE OBJECT lo_event_handler.

*  *   event handler
  SET HANDLER lo_event_handler->on_link_click FOR lo_events.
*  SET HANDLER lo_event_handler->raise_added_function FOR lo_events.
  SET HANDLER lo_event_handler->on_added_function FOR lo_events.



  lo_gr_alv->display( ).

ENDFORM.
