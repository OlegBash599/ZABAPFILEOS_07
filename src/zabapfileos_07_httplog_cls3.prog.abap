*&---------------------------------------------------------------------*
*&  Include           ZABAPFILEOS_07_HTTPLOG_CLS3
*&---------------------------------------------------------------------*

CLASS lcl_log DEFINITION.

  PUBLIC SECTION.

    CONSTANTS mc_balobj TYPE balobj_d VALUE 'ZABFOS'.
    CONSTANTS mc_balsub TYPE balsubobj VALUE 'ZFORECAST'.

    METHODS constructor.
    METHODS add_prev_msg.
    METHODS save2db_n_clear.
    METHODS save_complex
      IMPORTING iv TYPE any.

    METHODS get_logextnum
      RETURNING VALUE(rv) TYPE balnrext.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mc_log_prefix TYPE string VALUE 'ZFRCST_'.


    DATA mo_msg_list TYPE REF TO if_reca_message_list.

    DATA mv_complex_counter TYPE syindex.

ENDCLASS.

CLASS lcl_log IMPLEMENTATION.
  METHOD constructor.
    DATA lv_balnrext TYPE balnrext.

    DATA ls_bal_s_log_header TYPE bal_s_log.

    GET TIME.

    lv_balnrext = mc_log_prefix &&
                  sy-datum+4 && '_' && sy-uzeit.
    mv_complex_counter = 1.

    mo_msg_list = cf_reca_message_list=>create(
             id_object       = mc_balobj
             id_subobject    = mc_balsub
             id_extnumber    = lv_balnrext
*             id_deldate      = RECA0_DATE-MIN
*             if_delnotbefore = ABAP_FALSE
         ).

    mo_msg_list->get_header( ).

    ls_bal_s_log_header-params-callback-userexitp = ''.
    ls_bal_s_log_header-params-callback-userexitf = 'ZFUNC'.
    ls_bal_s_log_header-params-callback-userexitf = const_callback_function.

    mo_msg_list->change_header(
      EXPORTING
        is_msg_header =  ls_bal_s_log_header
      EXCEPTIONS
        error         = 1
        OTHERS        = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
    ENDIF.

  ENDMETHOD.

  METHOD add_prev_msg.
    mo_msg_list->add_symsg(
*      EXPORTING
*        if_cumulate  =     " Cumulation of Messages
*        id_detlevel  =     " Level of Detail (0..9)
*        id_probclass =     " Problem class
*        id_tabname   =     " Table Name
*        id_fieldname =     " Field Name
*        id_value     =     " Value
*        id_index     =     " Line Index
*        id_intreno   =     " INTRENO of Calling Program
*        id_custact   =     " IMG Activity
*        id_context   =     " Enhanced Navigation Data (Maximum 75 Characters)
*      IMPORTING
*        es_message   =     " Message data
    ).
  ENDMETHOD.

  METHOD save2db_n_clear.

    DATA ls_return TYPE bapiret2.

    mo_msg_list->store(
*      EXPORTING
*        if_in_update_task = ABAP_TRUE    " Call update program using "IN UPDATE TASK"
      EXCEPTIONS
        error             = 1
        OTHERS            = 2
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = abap_true    " Use of Command `COMMIT AND WAIT`
      IMPORTING
        return = ls_return.    " Return Messages

    mo_msg_list->clear( ).
  ENDMETHOD.

  METHOD get_logextnum.
    "RETURNING VALUE(rv) TYPE balnrext.
    rv =
      mo_msg_list->md_extnumber.
  ENDMETHOD.

  METHOD save_complex.
    DATA lv_json_http_info TYPE string.
    DATA lv_lognumb_ext TYPE balognr.

    lv_lognumb_ext = mo_msg_list->md_extnumber && mv_complex_counter.
    mv_complex_counter = mv_complex_counter + 1.

    lv_json_http_info =
    /ui2/cl_json=>serialize(
      EXPORTING
        data             = iv
    ).

    EXPORT http_call_json = lv_json_http_info
       TO DATABASE bal_indx(al)
     "  ID g_lognumber.
       ID lv_lognumb_ext.

    MESSAGE s004(zabfos_msg) WITH lv_lognumb_ext INTO sy-msgli.
    me->add_prev_msg( ).
  ENDMETHOD.

ENDCLASS.
