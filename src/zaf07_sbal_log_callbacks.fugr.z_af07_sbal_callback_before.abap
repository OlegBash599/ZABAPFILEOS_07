FUNCTION z_af07_sbal_callback_before.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(C_S_USER_COMMAND_DATA) TYPE  BAL_S_CBUC
*"----------------------------------------------------------------------
  DATA lc_complex_data_mark TYPE string VALUE 'Complex save id:'.
  DATA lv_trg_id TYPE string.
  DATA lv_log_id_indx TYPE balognr.
  DATA lv_http_info_json TYPE string.
  DATA html_xstring TYPE xstring.

  IF c_s_user_command_data-ucomm EQ '&IC1'.
    IF c_s_user_command_data-list_msgh-log_handle IS NOT INITIAL
        AND c_s_user_command_data-list_msgh-msgnumber IS NOT INITIAL.

      IF c_s_user_command_data-list_value CS lc_complex_data_mark.
        lv_trg_id = c_s_user_command_data-list_value.
        REPLACE ALL  OCCURRENCES OF lc_complex_data_mark IN lv_trg_id WITH ''.
        CONDENSE lv_trg_id NO-GAPS.
        lv_log_id_indx = lv_trg_id.

        IMPORT http_call_json = lv_http_info_json
          FROM DATABASE bal_indx(al)
            ID   lv_log_id_indx
            IGNORING STRUCTURE BOUNDARIES
            .

        TRY .
            CALL TRANSFORMATION sjson2html SOURCE XML lv_http_info_json
                                   RESULT XML html_xstring.

            cl_abap_browser=>show_html(
              html_string = cl_abap_codepage=>convert_from( html_xstring )
               ).

            c_s_user_command_data-ucomm_exec = abap_true.
          CATCH cx_root.

        ENDTRY.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
