*&---------------------------------------------------------------------*
*&  Include           Z_FIM_SA_EINVOICING_REP_TOP
*&---------------------------------------------------------------------*

DATA: lts_bukrs    TYPE vbrk-bukrs,
      lts_vkorg    TYPE vbrk-vkorg,
      lts_auart    TYPE vbak-auart,
      lts_dats     TYPE vbrk-fkdat,
      lts_aubel    TYPE vbrp-aubel,
      lts_vbeln    TYPE vbrk-vbeln,
      lts_kunag    TYPE vbrk-kunag,
      lts_vstat    TYPE nast-vstat,
      lts_aland    TYPE vbrp-aland,
      lt_blob_text TYPE catsxt_longtext_itab,
      ls_blob_text LIKE LINE OF lt_blob_text.

DATA: lc_obj_einvioce TYPE REF TO zcl_sd_saud_einvoicing,
      lv_vbeln        TYPE vbeln,
      lv_rejtxt_flg   TYPE char01,
      lt_params       TYPE STANDARD TABLE OF rsparams,
      ls_param        TYPE rsparams,
      ls_oderheader   TYPE zsdm_order_header,
      lt_orderitem    TYPE zsdm_t_order_item,
      lt_conds        TYPE zsdm_order_conditiontab,
      lt_messages     TYPE bapiret2tab,
      lv_crmemo       TYPE vbeln_va,
      lt_items        TYPE TABLE OF bapisditm,
      ls_err          TYPE REF TO cx_root,
      ls_text         TYPE string,
      lt_text         TYPE TABLE OF ls_text,
      lt_tdline1      TYPE STANDARD TABLE OF tdline,
      ls_tdline1      TYPE tdline.


CONSTANTS: lc_selall                TYPE char10 VALUE 'SELALL',
           lc_dselall               TYPE char10 VALUE 'SELDSL',
           lc_release               TYPE char10 VALUE 'RELEASE',
           lc_credit                TYPE char10 VALUE 'CREDIT',
           lc_refresh               TYPE char10 VALUE 'REFRESH',
           lc_blob_msdprof_desc(20) TYPE c VALUE 'Saudi E-Invoicing'.

DATA: l_answer   TYPE abap_bool,
      i_question TYPE string.

TYPES: t_xline(2048) TYPE x,
       BEGIN OF t_line,
         line TYPE char72,
       END OF t_line.

DATA: lt_data_tab TYPE STANDARD TABLE OF t_xline,
      lt_text_tab TYPE STANDARD TABLE OF t_line,
      ls_text_tab TYPE t_line,
      lv_size     TYPE i,
      lv_binary   TYPE xstring,
      lv_numbytes TYPE i,
      lv_modkey   TYPE zblob_modkey,
      ls_bloblink TYPE zbloblink.




SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: so_bukrs  FOR lts_bukrs,
                so_vkorg  FOR lts_vkorg OBLIGATORY,
                so_auart  FOR lts_auart,
                so_fkdat  FOR lts_dats,
                so_aubel  FOR lts_aubel,
                so_vbeln  FOR lts_vbeln,
                so_kunag  FOR lts_kunag,
                so_vstat  FOR lts_vstat NO INTERVALS ,
                so_aland  FOR lts_aland.

DATA: lt_bdctab TYPE TABLE OF bdcdata WITH HEADER LINE.


SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.

PARAMETERS: p_credit TYPE char01 AS CHECKBOX.
PARAMETERS: p_puf    TYPE char01 AS CHECKBOX.


SELECTION-SCREEN END OF BLOCK b2.
DATA: lo_gr_alv       TYPE REF TO cl_salv_table, " Variables for ALV properties
      lo_gr_functions TYPE REF TO cl_salv_functions_list,
      lo_grid         TYPE REF TO cl_salv_form_layout_grid, " Variables for header
      lo_layout_logo  TYPE REF TO cl_salv_form_layout_logo,
      lo_content      TYPE REF TO cl_salv_form_element,
      lv_title        TYPE string,
      lv_rows         TYPE string,
      lo_layout       TYPE REF TO cl_salv_layout, " Variables for enabling Save button
      lv_key          TYPE salv_s_layout_key,
      lo_display      TYPE REF TO cl_salv_display_settings, " Variable for layout settings
      lo_selections   TYPE REF TO cl_salv_selections, " Variables for selection mode and column properties
      lo_columns      TYPE REF TO cl_salv_columns,
      lo_column       TYPE REF TO cl_salv_column_table,
      lt_bill         TYPE STANDARD TABLE OF bapivbrk,
*      ls_bill         TYPE  bapivbrk,
*      lt_return       TYPE STANDARD TABLE OF bapiret1,
      lt_result       TYPE STANDARD TABLE OF bapivbrksuccess,
      ls_result       LIKE LINE OF lt_result,
      xyt_msg         TYPE zsde_messages_t,
      lv_error        TYPE xfeld,
      ls_zbloblink    TYPE zbloblink,
      lv_xstring      TYPE xstring,
      lv_status       TYPE char25,
      lv_filesize     TYPE i,
      lv_filename     TYPE string.

DATA: lv_cr_no       TYPE bapivbeln-vbeln,
      lt_bapiret2    TYPE TABLE OF bapiret2,
      ls_orderheader TYPE zsdm_order_header,
      lt_tdline      TYPE STANDARD TABLE OF tdline,
      ls_tdline      TYPE tdline.





AT SELECTION-SCREEN.

  IF ( so_auart[] IS INITIAL AND so_fkdat[] IS INITIAL AND
                so_aubel[] IS INITIAL AND so_vbeln[] IS INITIAL ) .
    MESSAGE e064(zsd).
  ENDIF.



INITIALIZATION.

*AT SELECTION-SCREEN on so_vbeln .
*  IF so_vbeln IS INITIAL.
*    MESSAGE e001(zsd) .
*  ENDIF.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_bukrs-low.

CLASS lcl_event_handler DEFINITION.
*
  PUBLIC SECTION.
    CLASS-METHODS:
      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.

    CLASS-METHODS: on_added_function FOR EVENT if_salv_events_functions~added_function OF cl_salv_events_table
      IMPORTING e_salv_function.

*
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
*
  METHOD on_link_click.


    READ TABLE zcl_sd_saud_einvoicing=>gt_process ASSIGNING FIELD-SYMBOL(<lfs_process>) INDEX row.
    lv_vbeln = <lfs_process>-vbeln.
    IF sy-subrc = 0.
      CHECK sy-subrc IS INITIAL.
      IF column = 'RES_FLG' AND <lfs_process>-kschl = zcl_sd_saud_einvoicing=>gc_zpuf.
        IF <lfs_process>-res_flg IS INITIAL.
          <lfs_process>-res_flg = abap_true.
        ELSE.
          <lfs_process>-res_flg = abap_false.
        ENDIF.
      ENDIF.
      IF column = 'RES_FLG' AND <lfs_process>-kschl <> zcl_sd_saud_einvoicing=>gc_zpuf.
        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            titel     = TEXT-009
            textline1 = TEXT-010.

      ENDIF.
      IF column = 'CRDT_FLG'.
        IF <lfs_process>-crdt_flg IS INITIAL.
          <lfs_process>-crdt_flg = abap_true.
        ELSE.
          <lfs_process>-crdt_flg = abap_false.
        ENDIF.
      ENDIF.
      IF column = 'VBELN'.
        SET PARAMETER ID :'VF' FIELD <lfs_process>-vbeln.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
      ENDIF.
      IF column = 'AUBEL'.
        SET PARAMETER ID: 'AUN' FIELD <lfs_process>-aubel.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.
      IF column = 'BLOB'.

        CLEAR lv_modkey.
        lv_modkey = <lfs_process>-vbeln.

        SELECT SINGLE * INTO @DATA(l_bloblink)
           FROM zbloblink
           WHERE modcode = 'EINV' AND
                 modkey  = @lv_modkey  AND
                 filekey = '0001'.

        IF sy-subrc = 0.
          CLEAR: lv_size,
                 lt_data_tab,
                 lt_text_tab,
                 lt_blob_text.

          DATA(lo_inf_zblob) = NEW zcl_inf_zblob(
          x_modcode = 'EINV' "'EINV'
          x_modkey  =  lv_modkey "  '0060004506'
          x_filekey = '0001' ).

          IF sy-subrc = 0.
            CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
              EXPORTING
                buffer        = lo_inf_zblob->binary_data
              IMPORTING
                output_length = lv_size
              TABLES
                binary_tab    = lt_data_tab.

            CALL FUNCTION 'SCMS_BINARY_TO_TEXT'
              EXPORTING
                input_length = lv_size
              TABLES
                binary_tab   = lt_data_tab
                text_tab     = lt_text_tab.
          ENDIF.

          LOOP AT lt_text_tab ASSIGNING FIELD-SYMBOL(<fs_text_tab>).
            ls_blob_text = <fs_text_tab>.
            APPEND ls_blob_text TO lt_blob_text.
          ENDLOOP.

          CLEAR lv_binary.
          lv_binary = lo_inf_zblob->binary_data.
        ENDIF.



        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title        = 'EDIT BLOB'
            im_display_mode = ' '
            im_start_column = 10
            im_start_row    = 10
          CHANGING
            ch_text         = lt_blob_text.


        CLEAR: lt_data_tab,
               lt_text_tab,
               lv_binary,
               lv_size.

        LOOP AT lt_blob_text ASSIGNING FIELD-SYMBOL(<fs_text_blob>).
          ls_text_tab = <fs_text_blob>.
          APPEND ls_text_tab TO lt_text_tab.
        ENDLOOP.

        IF lt_text_tab[] IS INITIAL.
          EXIT.
        ENDIF.

        CALL FUNCTION 'SCMS_TEXT_TO_BINARY'
          IMPORTING
            output_length = lv_size
          TABLES
            binary_tab    = lt_data_tab
            text_tab      = lt_text_tab.

*        CLEAR lo_inf_zblob->binary_data.

        CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
          EXPORTING
            input_length = lv_size
          IMPORTING
            buffer       = lv_binary
          TABLES
            binary_tab   = lt_data_tab.

        ls_bloblink-modcode     = 'EINV'.
        ls_bloblink-modkey      = <lfs_process>-vbeln.
        ls_bloblink-bill_doc    = <lfs_process>-vbeln.
        ls_bloblink-erfuser     = sy-uname.
        ls_bloblink-erfdate     = sy-datum.
        ls_bloblink-erftime     = sy-uzeit.
        ls_bloblink-upload_date = sy-datum.
        ls_bloblink-filetype    = 'TXT'.
        ls_bloblink-filedesc    = lc_blob_msdprof_desc.
        ls_bloblink-filekey     = '0001'.

        lv_numbytes  = lv_size.


        DATA(lo_inf_zblob1) = NEW zcl_inf_zblob(
          x_zbloblink = ls_bloblink
          x_blob_data = lv_binary
          x_blob_length = lv_numbytes
          x_rfc       = zcl_inf_gc=>true ).



        CALL METHOD lo_inf_zblob1->delete_blob
          EXCEPTIONS
            delete_error = 1
            write_error  = 2
            OTHERS       = 3.


        CALL METHOD lo_inf_zblob1->store_blob
          EXCEPTIONS
            put_error   = 1
            write_error = 2
            OTHERS      = 3.

        CLEAR: lt_blob_text,
               lt_data_tab,
               lv_binary,
               lt_text_tab.
      ENDIF.
    ENDIF.




    IF column = 'QR_TXT'.

      FREE lt_bdctab[].

      PERFORM bdc_read USING lv_vbeln.
      CLEAR lv_vbeln.
*      SET PARAMETER ID : 'VF' FIELD <lfs_process>-vbeln.
      TRY.
          CALL TRANSACTION 'VF02' USING lt_bdctab MODE 'E' UPDATE 'S' .
        CATCH cx_sy_authorization_error.
          RETURN.
      ENDTRY.
    ENDIF.

    IF column = 'DSIG_TXT'.

      FREE lt_bdctab[].


      PERFORM bdc_read USING lv_vbeln.
      CLEAR lv_vbeln.
*      SET PARAMETER ID : 'VF' FIELD <lfs_process>-vbeln.
      TRY.
          CALL TRANSACTION 'VF02' USING lt_bdctab MODE 'E' UPDATE 'S' .

        CATCH cx_sy_authorization_error.
          RETURN.
      ENDTRY.
    ENDIF.

    IF column =  'REJ_TXT'  .
      FREE lt_bdctab[].
      PERFORM bdc_read USING lv_vbeln.
      CLEAR lv_vbeln.
*      SET PARAMETER ID : 'VF' FIELD <lfs_process>-vbeln.
      TRY.
          CALL TRANSACTION 'VF02' USING lt_bdctab MODE 'E' UPDATE 'S' .
        CATCH cx_sy_authorization_error.
          RETURN.
      ENDTRY.
    ENDIF.

    IF column = 'WAR_TXT'.
      FREE lt_bdctab[].
      PERFORM bdc_read USING lv_vbeln.
      CLEAR lv_vbeln.
*      SET PARAMETER ID : 'VF' FIELD <lfs_process>-vbeln.
      TRY.
          CALL TRANSACTION 'VF02' USING lt_bdctab MODE 'E' UPDATE 'S' .
        CATCH cx_sy_authorization_error.
          RETURN.
      ENDTRY.
    ENDIF.




*    IF <lfs_process> IS ASSIGNED.
*      UNASSIGN <lfs_process>.
*    ENDIF.
*    IF <lfs_process_cr> IS ASSIGNED.
*      UNASSIGN <lfs_process_cr>.
*    ENDIF.

    lo_gr_alv->refresh( ).

  ENDMETHOD.

  METHOD on_added_function.

    DATA: lv_res_checked TYPE char01 VALUE abap_false.


    CASE: e_salv_function.
      WHEN lc_selall.

        LOOP AT zcl_sd_saud_einvoicing=>gt_process ASSIGNING FIELD-SYMBOL(<lfs_proces_sel>) WHERE kschl = zcl_sd_saud_einvoicing=>gc_zpuf.
          <lfs_proces_sel>-res_flg = abap_true.
        ENDLOOP.
        lo_gr_alv->refresh( ).
      WHEN lc_dselall.

        LOOP AT zcl_sd_saud_einvoicing=>gt_process ASSIGNING FIELD-SYMBOL(<lfs_proces_dsel>).
          <lfs_proces_dsel>-res_flg = abap_false.
        ENDLOOP.

        lo_gr_alv->refresh( ).
      WHEN lc_credit.
        PERFORM process_credit.

      WHEN lc_release.
        CLEAR lv_rejtxt_flg.

        LOOP  AT zcl_sd_saud_einvoicing=>gt_process ASSIGNING FIELD-SYMBOL(<lfs_process_re>).
          IF <lfs_process_re>-res_flg = abap_true.
            lv_res_checked = abap_true.
          ENDIF.
        ENDLOOP.

        IF lv_res_checked = abap_false.
          CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
            EXPORTING
              titel     = TEXT-011
              textline1 = TEXT-012.
          RETURN.
        ENDIF.

        i_question = TEXT-004.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = i_question
            text_question         = TEXT-005
            display_cancel_button = space
            default_button        = '2'
            popup_type            = 'ICON_MESSAGE_WARNING'
          IMPORTING
            answer                = l_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF l_answer = zcl_sd_saud_einvoicing=>gc_1.
          PERFORM process_release_data.
        ELSEIF l_answer = zcl_sd_saud_einvoicing=>gc_0.
          EXIT.
        ENDIF.
      WHEN lc_refresh.
        lc_obj_einvioce->extract_data( EXPORTING it_bukrs = so_bukrs[]
                                       it_vkorg = so_vkorg[]
                                       it_auart = so_auart[]
                                       it_fkdat = so_fkdat[]
                                       it_aubel = so_aubel[]
                                       it_vbeln = so_vbeln[]
                                       it_kunag = so_kunag[]
                                       it_vstat = so_vstat[]
                                       is_credit = p_credit
                                       is_puff   = p_puf  ).

        lo_gr_alv->refresh( ).

    ENDCASE.


  ENDMETHOD.                 "on_link_click
*
ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  BDC_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_read USING lv_vbeln TYPE vbeln. .

  PERFORM bdc_dynpro USING 'SAPMV60A' '0101'.
  PERFORM bdc_field USING 'BDC_CURSOR' 'VBRK-VBELN'.
  PERFORM bdc_field USING 'BDC_OKCODE' '/00'.
  PERFORM bdc_field USING 'VBRK-VBELN' lv_vbeln.

  PERFORM bdc_dynpro USING 'SAPMV60A' '0104'.
  PERFORM bdc_field USING 'BDC_CURSOR' 'VBRK-VBELN'.
  PERFORM bdc_field USING 'BDC_OKCODE' '=KFDE'.



  PERFORM bdc_dynpro USING 'SAPMV60A' '6001'.
  PERFORM bdc_field USING 'BDC_SUBSCR' '/00'.
  PERFORM bdc_field USING 'BDC_CURSOR' 'VBRK-VBELN'.
  PERFORM bdc_field USING 'BDC_SUBSCR' 'SAPMV60A'.


  PERFORM bdc_dynpro USING 'SAPMV60A' '6001'.
  PERFORM bdc_field USING 'BDC_OKCODE' '=KFTE'.
  PERFORM bdc_field USING 'BDC_SUBSCR' 'SAPMV60A'.
  PERFORM bdc_field USING 'BDC_CURSOR' 'VBRK-FKART'.
  PERFORM bdc_field USING 'BDC_SUBSCR' 'SAPMV60A'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0314   text
*      -->P_0315   text
*----------------------------------------------------------------------*
FORM bdc_dynpro  USING p_program p_dynpro.

  lt_bdctab-program = p_program.
  lt_bdctab-dynpro = p_dynpro.
  lt_bdctab-dynbegin = 'X'.
  APPEND lt_bdctab.
  CLEAR lt_bdctab.

ENDFORM. " bdc_dynpro
*&---------------------------------------------------------------------*
*&      Form  BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0319   text
*----------------------------------------------------------------------*
FORM bdc_field USING p_fnam p_fval.

  lt_bdctab-fnam = p_fnam.
  lt_bdctab-fval = p_fval.
  APPEND lt_bdctab.
  CLEAR lt_bdctab.

ENDFORM. " bdc_field
*&---------------------------------------------------------------------*
*&      Form  PROCESS_RELEASE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_release_data .

  TYPES: BEGIN OF lty_prcs_rl,
           lights     TYPE char04,
           vstat      TYPE char01,
           vstat_txt  TYPE char61,
           vsztp      TYPE char01,
           vsztp_txt  TYPE char61,
           vbeln      TYPE vbeln,
           aubel      TYPE vbeln_va,
           kunag      TYPE kunag,
           fkdat      TYPE fkdat,
           qr_txt     TYPE char256,
           dsig_txt   TYPE char256,
           rej_txt    TYPE char256,
           res_flg    TYPE  char01,
           crdt_flg   TYPE char01,
           blob       TYPE char256,
           credit_txt TYPE char40,
           credit_ref TYPE char40,
           kschl      TYPE kschl,
           vkorg      TYPE vkorg,
           vtweg      TYPE vtweg,
           spart      TYPE spart,
           objky      TYPE nast-objky,
         END OF lty_prcs_rl,
         BEGIN OF lty_errtxt,
           jobcount TYPE tbtcjob-jobcount,
           vbeln    TYPE vbeln,
         END OF lty_errtxt.

  DATA: ls_prcs_rl       TYPE lty_prcs_rl,
        lt_prcs_rl       TYPE TABLE OF lty_prcs_rl,
        lv_jobcount      LIKE tbtcjob-jobcount,
        lt_errtxt        TYPE TABLE OF lty_errtxt,
        ls_errtxt        TYPE lty_errtxt,
        gc_running       TYPE btcstatus              VALUE 'R',
        gc_ready         TYPE btcstatus              VALUE 'Y',
        gc_scheduled     TYPE btcstatus              VALUE 'P',
        gc_released      TYPE btcstatus              VALUE 'S',
        gc_aborted       TYPE btcstatus              VALUE 'A',
        gc_finished      TYPE btcstatus              VALUE 'F',
        gc_put_active    TYPE btcstatus              VALUE 'Z',
        gc_unknown_state TYPE btcstatus              VALUE 'X'.


  LOOP AT zcl_sd_saud_einvoicing=>gt_process ASSIGNING FIELD-SYMBOL(<fs_release_process>) WHERE res_flg = abap_true.
    ls_prcs_rl-objky = <fs_release_process>-vbeln.
    MOVE-CORRESPONDING <fs_release_process> TO ls_prcs_rl.
    APPEND ls_prcs_rl TO lt_prcs_rl.
    CLEAR ls_prcs_rl.

  ENDLOOP.

  IF lt_prcs_rl IS NOT INITIAL.
    SELECT * FROM nast INTO TABLE @DATA(lt_nast_updt) FOR ALL ENTRIES IN @lt_prcs_rl
                                                      WHERE objky = @lt_prcs_rl-objky
                                                      AND   kschl = @lt_prcs_rl-kschl.
  ENDIF.

  LOOP AT lt_nast_updt ASSIGNING FIELD-SYMBOL(<fs_nast_updt>).
    <fs_nast_updt>-vstat = 0.
    <fs_nast_updt>-vsztp = 1.


    CALL FUNCTION 'RV_MESSAGE_UPDATE_SINGLE'
      EXPORTING
        msg_nast = <fs_nast_updt>.

  ENDLOOP.

  lv_jobcount = 1.

  LOOP AT zcl_sd_saud_einvoicing=>gt_process ASSIGNING FIELD-SYMBOL(<fs_process_data>) WHERE res_flg = abap_true
                                                                                       AND kschl = zcl_sd_saud_einvoicing=>gc_zpuf.
    <fs_process_data>-vsztp = zcl_sd_saud_einvoicing=>gc_1.
    <fs_process_data>-vstat = zcl_sd_saud_einvoicing=>gc_0.

*  *** process

    CLEAR ls_param.
    ls_param-selname = 'S_KSCHL'.
    ls_param-kind    = 'S'.
    ls_param-sign    = 'I'.
    ls_param-option  = 'EQ'.
    ls_param-low     = zcl_sd_saud_einvoicing=>gc_zpuf. "'ZZSA'.
    APPEND ls_param TO lt_params.

    CLEAR ls_param.
    ls_param-selname = 'S_VKORG'.
    ls_param-kind    = 'S'.
    ls_param-sign    = 'I'.
    ls_param-option  = 'EQ'.
    ls_param-low     = <fs_process_data>-vkorg. "'S183'.
    APPEND ls_param TO lt_params.


    CLEAR ls_param.
    ls_param-selname = 'S_KAPPL'.
    ls_param-kind    = 'S'.
    ls_param-sign    = 'I'.
    ls_param-option  = 'EQ'.
    ls_param-low     = 'V3'.
    APPEND ls_param TO lt_params.

    CLEAR ls_param.
    ls_param-selname = 'S_OBJKY'.
    ls_param-kind    = 'S'.
    ls_param-sign    = 'I'.
    ls_param-option  = 'EQ'.
    ls_param-low     = <fs_process_data>-vbeln. "'9500100233'.
    APPEND ls_param TO lt_params.

    .

    CLEAR ls_param.
    ls_param-selname = 'P_AGAIN'.
    ls_param-kind    = 'P'.
    ls_param-low     = abap_false.
    APPEND ls_param TO lt_params.



    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = 'JOB_EINVOICE'
      IMPORTING
        jobcount         = lv_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    TRY.
        SUBMIT zrsnast00_cpy USING SELECTION-SCREEN 901 WITH SELECTION-TABLE lt_params
                                                     VIA JOB 'JOB_EINVOICE' NUMBER lv_jobcount AND RETURN. " EXPORTING LIST TO MEMORY.

      CATCH cx_root INTO ls_err.
        IF ls_err IS NOT INITIAL.
          ls_text = ls_err->get_text( ).
          APPEND ls_text TO lt_text.
          CLEAR ls_text.
        ENDIF.
    ENDTRY.
    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = lv_jobcount
        jobname              = 'JOB_EINVOICE'
        strtimmed            = 'X'
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        OTHERS               = 8.

    ls_errtxt-jobcount = lv_jobcount.
    ls_errtxt-vbeln    = <fs_process_data>-vbeln.
    APPEND ls_errtxt TO lt_errtxt.

    CLEAR: lt_params,
           ls_param.
  ENDLOOP.

  IF lt_errtxt[] IS NOT INITIAL.
    SELECT jobname, jobcount , status FROM tbtco INTO TABLE @DATA(lt_tbtco) FOR ALL ENTRIES IN @lt_errtxt
                                                   WHERE jobname = 'JOB_EINVOICE'
                                                   AND jobcount = @lt_errtxt-jobcount.
  ENDIF.
*
  LOOP AT lt_tbtco ASSIGNING FIELD-SYMBOL(<fs_tbtco>).
    READ TABLE lt_errtxt ASSIGNING FIELD-SYMBOL(<fs_errtxt>) WITH KEY jobcount = <fs_tbtco>-jobcount.
    IF sy-subrc = 0.
      IF <fs_tbtco>-status = gc_aborted .
        CONCATENATE 'for' <fs_errtxt>-vbeln 'Job Cancelled' INTO ls_tdline1 SEPARATED BY space.
      ELSEIF <fs_tbtco>-status = gc_finished .
        CONCATENATE 'for' <fs_errtxt>-vbeln 'Job Completed' INTO ls_tdline1 SEPARATED BY space.
      ELSEIF <fs_tbtco>-status =  gc_scheduled .
        CONCATENATE 'for' <fs_errtxt>-vbeln 'Job Scheduled' INTO ls_tdline1 SEPARATED BY space.
      ELSEIF <fs_tbtco>-status =   gc_running  .
        CONCATENATE 'for' <fs_errtxt>-vbeln 'Active Job Created' INTO ls_tdline1 SEPARATED BY space.
      ELSEIF <fs_tbtco>-status =  gc_released .
        CONCATENATE 'for' <fs_errtxt>-vbeln 'Job Released' INTO ls_tdline1 SEPARATED BY space.
      ELSEIF  <fs_tbtco>-status =  gc_ready.
        CONCATENATE 'for' <fs_errtxt>-vbeln 'Job Ready' INTO ls_tdline1 SEPARATED BY space.
      ELSEIF <fs_tbtco>-status = gc_put_active  .
        CONCATENATE 'for' <fs_errtxt>-vbeln 'Job Active' INTO ls_tdline1 SEPARATED BY space.
      ELSEIF <fs_tbtco>-status = gc_unknown_state .
        CONCATENATE 'for' <fs_errtxt>-vbeln 'Unknown Status' INTO ls_tdline1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    APPEND ls_tdline1 TO lt_tdline1.
  ENDLOOP.

  SORT lt_tdline1.
  DELETE ADJACENT DUPLICATES FROM lt_tdline1.

  LOOP AT zcl_sd_saud_einvoicing=>gt_process ASSIGNING FIELD-SYMBOL(<fs_process_data1>) WHERE res_flg = abap_true
                                                                                       AND kschl = zcl_sd_saud_einvoicing=>gc_zpuf.
    READ TABLE lt_errtxt ASSIGNING FIELD-SYMBOL(<fs_errtxt1>) WITH KEY vbeln = <fs_process_data1>-vbeln.
    IF sy-subrc = 0.
      READ TABLE lt_tbtco ASSIGNING FIELD-SYMBOL(<fs_tbtco1>) WITH KEY jobcount = <fs_errtxt1>-jobcount
                                                                       status   = gc_aborted .
      IF sy-subrc = 0.
        CONCATENATE 'for' <fs_errtxt>-vbeln 'Release process not started' INTO  ls_tdline1 SEPARATED BY space.
      ENDIF.
    ENDIF.
    APPEND ls_tdline1 TO lt_tdline1[].
  ENDLOOP.
*  IF lt_tdline1[] IS INITIAL.
*    ls_tdline1 = 'Release process not started' .
*
*  ENDIF.



*  LOOP AT lt_text ASSIGNING FIELD-SYMBOL(<fs_text>).
*    ls_tdline1     = <fs_text>.
*    APPEND ls_tdline1 TO lt_tdline1.
**  ENDLOOP.
*  SELECT * FROM tbtco INTO TABLE @DATA(lt_tbtco)
*     FOR ALL ENTRIES IN @zcl_sd_saud_einvoicing=>gt_process WHERE jobname = 'JOB_EINVOICE'.


  IF lt_tdline1[] IS NOT INITIAL.

    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY_OK'
      EXPORTING
        endpos_col   = 100
        endpos_row   = 20
        startpos_col = 1
        startpos_row = 10
        titletext    = TEXT-006
      TABLES
        valuetab     = lt_tdline1
      EXCEPTIONS
        break_off    = 0
        OTHERS       = 0.
  ENDIF.



  CLEAR lt_tdline1[].

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PROCESS_CREDIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_credit .

  DATA: lv_date TYPE char10,
        lv_line TYPE n LENGTH 02.

  IF zcl_sd_saud_einvoicing=>gt_process IS NOT INITIAL.

    SELECT vbelv, vbeln, vbtyp_n, vbtyp_v FROM vbfa INTO TABLE @DATA(lt_credit_check)
                                                 FOR ALL ENTRIES IN @zcl_sd_saud_einvoicing=>gt_process
                                                 WHERE vbelv = @zcl_sd_saud_einvoicing=>gt_process-vbeln
                                                 AND   vbtyp_n = @zcl_sd_saud_einvoicing=>gc_k
                                                 AND   vbtyp_v = @zcl_sd_saud_einvoicing=>gc_m.
  ENDIF.




  LOOP AT zcl_sd_saud_einvoicing=>gt_process ASSIGNING FIELD-SYMBOL(<fs_process_credit_data>) WHERE crdt_flg = abap_true.

    READ TABLE lt_credit_check ASSIGNING FIELD-SYMBOL(<fs_credit_check>) WITH KEY vbelv = <fs_process_credit_data>-vbeln.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    CLEAR: ls_orderheader.

    ls_orderheader-doc_type = 'ZSPZ'.
    ls_orderheader-sales_org = <fs_process_credit_data>-vkorg.
    ls_orderheader-distr_chan = <fs_process_credit_data>-vtweg.
    ls_orderheader-division = <fs_process_credit_data>-spart.
*    ls_orderheader-ref_doc_l = <fs_process_credit_data>-vbeln.
    ls_orderheader-ref_doc = <fs_process_credit_data>-vbeln.
*
*
*    ls_orderheader-bill_date = sy-datum.

* Call the FM to create sales order
*    CALL FUNCTION 'Z_SDM_BAPI_NEW_CR_MEMO_CREATE'
*      EXPORTING
*        x_oderheader = ls_oderheader
*        xt_orderitem = lt_orderitem
*        xt_conds     = lt_conds
*      IMPORTING
*        yt_messages  = lt_messages
*        y_crmemo     = lv_crmemo
*      TABLES
*        yt_items     = lt_items.

*    APPEND LINES OF lt_messages TO yt_messages.
*    REFRESH lt_messages.

*    CALL FUNCTION 'Z_SDM_BAPI_ORDER_INTERFACE'
*      EXPORTING
*        x_oderheader   = ls_orderheader
*        xt_orderitem   = lt_orderitems
*        xt_conds       = lt_orderconds
*        xt_texts       = lt_texts
*        xt_bu_partner  = lt_bu_partner
*        x_so_operation = abap_true
*      IMPORTING
*        yt_messages    = lt_messages
*        y_orderno      = lv_orderno
*        y_billno       = lv_billno.

*        create the credit note from the original billing document
    CALL FUNCTION 'BAPI_SALESDOCUMENT_COPY'
      EXPORTING
        salesdocument    = <fs_process_credit_data>-vbeln
        documenttype     = 'ZSPZ'
      IMPORTING
        salesdocument_ex = lv_cr_no
      TABLES
        return           = lt_bapiret2.

*     transfer the messages and check for errors
    CALL FUNCTION 'Z_SDE_POPULATE_MESSAGE'
      EXPORTING
        xt_bapiret2    = lt_bapiret2
      IMPORTING
        y_error_exists = lv_error
      CHANGING
        xyt_msg        = xyt_msg.
    IF ( xyt_msg IS INITIAL AND lv_cr_no IS NOT INITIAL ).

      CONCATENATE lv_cr_no TEXT-008 INTO ls_tdline SEPARATED BY space.
      APPEND ls_tdline TO lt_tdline.

      CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY_OK'
        EXPORTING
          endpos_col   = 100
          endpos_row   = 20
          startpos_col = 1
          startpos_row = 10
          titletext    = TEXT-007
        TABLES
          valuetab     = lt_tdline
        EXCEPTIONS
          break_off    = 0
          OTHERS       = 0.

      CLEAR: ls_tdline,
             lt_tdline.

    ELSEIF xyt_msg IS NOT INITIAL.
      READ TABLE xyt_msg ASSIGNING FIELD-SYMBOL(<fs_msg>) INDEX 1.
      ls_tdline = <fs_msg>-message.
      APPEND ls_tdline TO lt_tdline.
    ENDIF.
    IF lv_error NE abap_true.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.
    CLEAR: lt_bapiret2,
           lv_error,
           xyt_msg.
  ENDLOOP.


  IF lt_tdline IS NOT INITIAL.

    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY_OK'
      EXPORTING
        endpos_col   = 100
        endpos_row   = 20
        startpos_col = 1
        startpos_row = 10
        titletext    = TEXT-006
      TABLES
        valuetab     = lt_tdline
      EXCEPTIONS
        break_off    = 0
        OTHERS       = 0.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    CLEAR: ls_tdline,
           lt_tdline.

  ENDIF.

  lc_obj_einvioce->extract_data( EXPORTING it_bukrs = so_bukrs[]
                                it_vkorg = so_vkorg[]
                                it_auart = so_auart[]
                                it_fkdat = so_fkdat[]
                                it_aubel = so_aubel[]
                                it_vbeln = so_vbeln[]
                                it_kunag = so_kunag[]
                                it_vstat = so_vstat[]
                                is_credit = p_credit
                                is_puff   = p_puf  ).

  lo_gr_alv->refresh( ).


ENDFORM.
