*&---------------------------------------------------------------------*
*&  Include           ZABAPFILEOS_07_HTTPLOG_CLS1
*&---------------------------------------------------------------------*

CLASS lcl_http_forecast DEFINITION.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_log TYPE REF TO lcl_log.

    METHODS fn.

  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES ts_http_call TYPE lcl_log_view=>ts_http_call.

    DATA mo_log TYPE REF TO lcl_log.
    DATA ms_http_call TYPE ts_http_call.

    METHODS _create_http_client
      IMPORTING iv_url_trg TYPE string
      RETURNING VALUE(ro)  TYPE REF TO if_http_client.

    METHODS _http_call_get.
    METHODS _http_call_post.
    METHODS _http_call_empty_resp.

    METHODS _log_httpcall.
ENDCLASS.


CLASS lcl_http_forecast IMPLEMENTATION.
  METHOD constructor.
    "IMPORTING io_log TYPE REF TO lcl_log.
    mo_log = io_log.
  ENDMETHOD.

  METHOD fn.

    MESSAGE s001(zabfos_msg) INTO sy-msgli.
    mo_log->add_prev_msg( ).

    _http_call_get( ).
    _log_httpcall( ).
    _http_call_post( ).
    _http_call_empty_resp( ).

    mo_log->save2db_n_clear( ).

  ENDMETHOD.

  METHOD _http_call_get.

    TYPES: BEGIN OF ts_req_send
            , product TYPE string
            , trg_date TYPE string
         , END OF ts_req_send
         .

    DATA url_sep TYPE string VALUE '/'.
    DATA lv_trg_url TYPE string.
    DATA lv_url_suffix TYPE string VALUE 'forecast/sales/'.
    DATA lv_url_target_with_path TYPE string VALUE '{PRODID}/{YYYY-MM-DD}'.
    DATA ls_req_send TYPE ts_req_send.

    DATA lv_error_msg TYPE string.
    DATA lo_http_client TYPE REF TO if_http_client.



    lv_trg_url = p_extsrv && lv_url_suffix.
    ls_req_send-product = 'PROD01'.
    ls_req_send-trg_date = '2024-06-06'.

    lo_http_client = _create_http_client( iv_url_trg = lv_trg_url ).

    CALL METHOD lo_http_client->request->set_method
      EXPORTING
        method = if_http_entity=>co_request_method_get.

    lv_url_target_with_path
         = lv_trg_url &&
           ls_req_send-product && url_sep && ls_req_send-trg_date.

    cl_http_utility=>set_request_uri( request = lo_http_client->request
                                      uri     = lv_url_target_with_path ).

    CLEAR ms_http_call.
    ms_http_call-req_method = lo_http_client->request->get_method( ).
    ms_http_call-req_path = lv_url_target_with_path.

    lo_http_client->send( EXCEPTIONS  OTHERS = 4 ).
    IF sy-subrc <> 0.
      lo_http_client->get_last_error( IMPORTING message = lv_error_msg ).
      MESSAGE i000(cl) WITH lv_error_msg.
      mo_log->add_prev_msg( ).
      RETURN.
    ENDIF.

    lo_http_client->receive( EXCEPTIONS   OTHERS = 4 ).
    IF sy-subrc <> 0.
      lo_http_client->get_last_error( IMPORTING message = lv_error_msg ).

      MESSAGE i000(cl) WITH lv_error_msg.
      mo_log->add_prev_msg( ).
      RETURN.
    ENDIF.

    ms_http_call-resp_cdata = lo_http_client->response->get_cdata( ).

    lo_http_client->response->get_status(
      IMPORTING
        code   = ms_http_call-resp_code    " HTTP status code
        reason = ms_http_call-resp_reason    " HTTP status description
    ).

    lo_http_client->close( ).


  ENDMETHOD.

  METHOD _http_call_post.

  ENDMETHOD.

  METHOD _http_call_empty_resp.

  ENDMETHOD.

  METHOD _create_http_client.
    "IMPORTING iv_url_trg TYPE string
    "RETURNING VALUE(ro)  TYPE REF TO if_http_client.

    DATA lv_ssl_id  TYPE ssfapplssl VALUE 'ANONYM'.

    cl_http_client=>create_by_url(
      EXPORTING
        url                = iv_url_trg    " URL
*        proxy_host         =     " Logical destination (specified in function call)
*        proxy_service      =     " Port Number
        ssl_id             = lv_ssl_id
*        sap_username       =     " ABAP System, User Logon Name
*        sap_client         =     " R/3 System, Client Number from Logon
      IMPORTING
        client             = ro    " HTTP Client Abstraction
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      mo_log->add_prev_msg( ).
    ENDIF.

  ENDMETHOD.

  METHOD _log_httpcall.

    MESSAGE s002(zabfos_msg) WITH ms_http_call-req_method ms_http_call-req_path
      INTO sy-msgli.
    mo_log->add_prev_msg( ).

    MESSAGE s003(zabfos_msg) WITH ms_http_call-resp_code ms_http_call-resp_reason
       INTO sy-msgli.
    mo_log->add_prev_msg( ).

    mo_log->save_complex( ms_http_call ).

  ENDMETHOD.

ENDCLASS.
