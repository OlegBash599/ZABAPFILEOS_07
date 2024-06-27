*&---------------------------------------------------------------------*
*&  Include           ZABAPFILEOS_07_HTTPLOG_CLS2
*&---------------------------------------------------------------------*

CLASS lcl_log_view DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF ts_http_call
          , req_method TYPE string
          , req_path TYPE string
          , req_json TYPE string
          , resp_code TYPE syindex
          , resp_reason TYPE string
          , resp_cdata TYPE string
      , END OF ts_http_call
      .

    METHODS constructor.

    METHODS fn.
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_log TYPE REF TO lcl_log.
    DATA ms_http_call TYPE ts_http_call.

    METHODS _db_search_balheader
      EXPORTING et_balheader TYPE balhdr_t.

    METHODS _bal_db_load
      IMPORTING it_balheader  TYPE balhdr_t
      EXPORTING et_msg_list   TYPE bal_t_msgh
                et_bal_t_logh TYPE bal_t_logh.

    METHODS _profile_with_callback
      CHANGING cs_display_profile TYPE bal_s_prof.

    METHODS display_logs
      IMPORTING is_display_profile TYPE bal_s_prof
                it_bal_t_logh      TYPE bal_t_logh
                it_msg_list        TYPE bal_t_msgh.


ENDCLASS.

CLASS lcl_log_view IMPLEMENTATION.
  METHOD constructor.
    "
  ENDMETHOD.

  METHOD fn.

    " RECA_GUI_LOGLIST_APPL <- можно вызывать этот ФМ для выборки

    DATA lt_balheader TYPE balhdr_t.

    DATA ls_display_profile TYPE bal_s_prof.
    DATA lt_bal_t_logh TYPE bal_t_logh.
    DATA lt_msg_list TYPE bal_t_msgh.

    _db_search_balheader( IMPORTING et_balheader = lt_balheader ).

    _bal_db_load( EXPORTING it_balheader = lt_balheader
                  IMPORTING et_msg_list = lt_msg_list
                            et_bal_t_logh = lt_bal_t_logh ).

    _profile_with_callback( CHANGING cs_display_profile = ls_display_profile ).

    display_logs( EXPORTING is_display_profile = ls_display_profile
                            it_bal_t_logh      = lt_bal_t_logh
                            it_msg_list        = lt_msg_list  ).

  ENDMETHOD.

  METHOD display_logs.
*      IMPORTING is_display_profile TYPE bal_s_prof
*                it_bal_t_logh      TYPE bal_t_logh
*                it_msg_list        TYPE bal_t_msgh.

    DATA ls_bal_s_excm TYPE bal_s_excm.
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = is_display_profile
        i_t_log_handle      = it_bal_t_logh
        i_t_msg_handle      = it_msg_list
      IMPORTING
        e_s_exit_command    = ls_bal_s_excm
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
    ENDIF.


  ENDMETHOD.

  METHOD _db_search_balheader.
    "EXPORTING et_balheader TYPE balhdr_t.

    DATA ls_search_logs TYPE bal_s_lfil.
    DATA lv_del_upto TYPE syindex.

    DATA lt_balheader TYPE balhdr_t.

    ls_search_logs-object = VALUE #( ( sign = 'I' option = 'EQ' low =  lcl_log=>mc_balobj ) ).
    ls_search_logs-subobject = VALUE #( ( sign = 'I' option = 'EQ' low =  lcl_log=>mc_balsub ) ).

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter = ls_search_logs
      IMPORTING
        e_t_log_header = lt_balheader
      EXCEPTIONS
        OTHERS         = 0.

    " чтобы на экране были последние 10 логов максимум
    IF lines( lt_balheader ) > 10.
      lv_del_upto = lines( lt_balheader ) - 10.
      DELETE lt_balheader  TO lv_del_upto.
    ELSE.

    ENDIF.

    et_balheader = lt_balheader.
  ENDMETHOD.

  METHOD _bal_db_load.
    "IMPORTING it_balheader  TYPE balhdr_t
    "EXPORTING et_msg_list   TYPE bal_t_msgh
    "           et_bal_t_logh TYPE bal_t_logh.

*   load logs from DB
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header     = it_balheader    " Alternative 1: Table of log headers
*       i_t_log_handle     =     " Alternative 2: Table of log handles
*       i_t_lognumber      =     " Alternative 3: Table of log numbers
*       i_client           = SY-MANDT    " Client for I_T_LOGNUMBER
*       i_do_not_load_messages        = SPACE    " Only load log header
*       i_exception_if_already_loaded =     " Raise exception if log already loaded
*       i_lock_handling    = 2    " 0: Ignore locks, 1: Do not read locked items; 3: Wait for un
      IMPORTING
        e_t_log_handle     = et_bal_t_logh    " Table of handles of logs read
        e_t_msg_handle     = et_msg_list    " Table of handles of messages read
*       e_t_locked         =     " Logs locked and not read
      EXCEPTIONS
        no_logs_specified  = 1
        log_not_found      = 2
        log_already_loaded = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
    ENDIF.
  ENDMETHOD.

  METHOD _profile_with_callback.
    "CHANGING cs_display_profile TYPE bal_s_prof.

    DATA ls_display_profile TYPE bal_s_prof.

    CALL FUNCTION 'BAL_DSP_PROFILE_STANDARD_GET'
      IMPORTING
        e_s_display_profile = ls_display_profile.   " Display Profile

    ls_display_profile-root_text = 'Root text is here'.
    ls_display_profile-use_grid = abap_true.

    ls_display_profile-clbk_read-userexitp = ''.
    ls_display_profile-clbk_read-userexitf = 'Z_AF07_SBAL_CALLBACK_READ'.
    ls_display_profile-clbk_read-userexitt = const_callback_function.

    ls_display_profile-clbk_ucom-userexitp = ''.
    ls_display_profile-clbk_ucom-userexitf = 'Z_AF07_SBAL_CALLBACK_USR_COMM'.
    ls_display_profile-clbk_ucom-userexitt = const_callback_function.

    ls_display_profile-clbk_ucbf-userexitp = ''.
    ls_display_profile-clbk_ucbf-userexitf = 'Z_AF07_SBAL_CALLBACK_BEFORE'.
    ls_display_profile-clbk_ucbf-userexitt = const_callback_function.

    ls_display_profile-clbk_ucaf-userexitp = ''.
    ls_display_profile-clbk_ucaf-userexitf = 'Z_AF07_SBAL_CALLBACK_AFTER'.
    ls_display_profile-clbk_ucaf-userexitt = const_callback_function.

    ls_display_profile-clbk_pbo-userexitp = ''.
    ls_display_profile-clbk_pbo-userexitf = 'Z_AF07_SBAL_CALLBACK_PBO'.
    ls_display_profile-clbk_pbo-userexitt = const_callback_function.


    """"" custom extra buttons
*          	In application toolbar
*3  In toolbar above message list on the left
*4  In toolbar above message list on the right

    " In application toolbar button
    ls_display_profile-ext_push1-active = 'X'.
    ls_display_profile-ext_push2-def-text = 'But01'.
    ls_display_profile-ext_push4-def-quickinfo = 'But1'.
    ls_display_profile-ext_push1-def-icon_id = icon_display_text.
    ls_display_profile-ext_push1-def-icon_text = 'Button1'.

    " In application toolbar button
    ls_display_profile-ext_push2-active = 'X'.
    ls_display_profile-ext_push2-def-text = 'But02'.
    ls_display_profile-ext_push4-def-quickinfo = 'But2'.
    ls_display_profile-ext_push2-def-icon_id = icon_display_text.
    ls_display_profile-ext_push2-def-icon_text = 'Button2'.
    ls_display_profile-ext_push2-def-path = '1'.


    ls_display_profile-ext_push3-active = 'X'.
    "   *3  In toolbar above message list on the left
    ls_display_profile-ext_push3-def-text = 'But03'.
    ls_display_profile-ext_push4-def-quickinfo = 'But3'.
    ls_display_profile-ext_push3-def-icon_id = icon_display_text.
    ls_display_profile-ext_push3-def-icon_text = 'Button3'.

    ls_display_profile-ext_push4-active = 'X'.
    " *4  In toolbar above message list on the right
    ls_display_profile-ext_push4-position = 4.
    ls_display_profile-ext_push4-def-text = 'But04'.
    ls_display_profile-ext_push4-def-quickinfo = 'But4'.
    ls_display_profile-ext_push4-def-icon_id = icon_display_text.
    ls_display_profile-ext_push4-def-icon_text = 'Button4'.


* define callback to react on this pushbutton
    ls_display_profile-clbk_ucom-userexitt     = const_callback_function.
    ls_display_profile-clbk_ucom-userexitp     = ''.
    ls_display_profile-clbk_ucom-userexitf     = 'Z_AF07_FCODE_PUSHBUTTON'.


    ls_display_profile-clbk_toolb-userexitt     = const_callback_function.
    ls_display_profile-clbk_toolb-userexitp     = ''.
    ls_display_profile-clbk_toolb-userexitf     = 'Z_AF07_TOOLBAR_MODIFY'.

    ls_display_profile-disvariant-report = sy-repid.
    ls_display_profile-disvariant-handle = 'LOG'.

    cs_display_profile = ls_display_profile.

  ENDMETHOD.
ENDCLASS.
