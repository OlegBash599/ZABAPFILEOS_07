*&---------------------------------------------------------------------*
*&  Include           ZABAPFILEOS_07_PROTO_OBJ_CLS1
*&---------------------------------------------------------------------*

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.
    METHODS start_of_sel.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_write_doc_hdr
             , v_objectid TYPE cdhdr-objectid
             , v_tcode TYPE cdhdr-tcode
             , v_hdr_utime TYPE cdhdr-utime
             , v_hdr_udate TYPE cdhdr-udate
             , v_hdr_username TYPE cdhdr-username
             , t_cdtxt_tab TYPE STANDARD TABLE OF cdtxt WITH DEFAULT KEY
          , END OF ts_write_doc_hdr
          .

    TYPES: BEGIN OF ts_proto_h_chng
            , old_y TYPE STANDARD TABLE OF yztaf07_proto_h WITH DEFAULT KEY
            , new_x TYPE STANDARD TABLE OF yztaf07_proto_h WITH DEFAULT KEY
            , ch_ind TYPE cdchngind
          , END OF ts_proto_h_chng
          .

    TYPES: BEGIN OF ts_features
            , old_y TYPE STANDARD TABLE OF yztaf07_features WITH DEFAULT KEY
            , new_x TYPE STANDARD TABLE OF yztaf07_features WITH DEFAULT KEY
            , ch_ind TYPE cdchngind
          , END OF ts_features
          .

    TYPES: BEGIN OF ts_components
            , old_y TYPE STANDARD TABLE OF yztaf07_compnents WITH DEFAULT KEY
            , new_x TYPE STANDARD TABLE OF yztaf07_compnents WITH DEFAULT KEY
            , ch_ind TYPE cdchngind
          , END OF ts_components
          .

    TYPES: BEGIN OF ts_services
            , old_y TYPE STANDARD TABLE OF yztaf07_services WITH DEFAULT KEY
            , new_x TYPE STANDARD TABLE OF yztaf07_services WITH DEFAULT KEY
            , ch_ind TYPE cdchngind
          , END OF ts_services
          .

    TYPES: BEGIN OF ts_chngdocs_tabs
             , s_proto      TYPE ts_proto_h_chng
             , s_features   TYPE ts_features
             , s_components TYPE ts_components
             , s_services   TYPE ts_services
          , END OF ts_chngdocs_tabs
          .

    TYPES: tt_proto_with_kz TYPE STANDARD TABLE OF yztaf07_proto_h WITH DEFAULT KEY.


    DATA mt_proto_h_new     TYPE zttaf07_proto_h.
    DATA mt_features_new    TYPE zttaf07_features.
    DATA mt_components_new  TYPE zttaf07_compnents.
    DATA mt_services_new    TYPE zttaf07_services.

    DATA mt_proto_h_old     TYPE zttaf07_proto_h.
    DATA mt_features_old    TYPE zttaf07_features.
    DATA mt_components_old  TYPE zttaf07_compnents.
    DATA mt_services_old    TYPE zttaf07_services.

    DATA ms_write_doc_hdr TYPE ts_write_doc_hdr.
    DATA ms_chngdocs_tabs TYPE ts_chngdocs_tabs.

    DATA mc_tables_not_equal TYPE char1 VALUE 'F'.

    METHODS _init_fill.

    METHODS _del_some_record.

    METHODS _init_proto_h
      IMPORTING iv_seq     TYPE syindex
      EXPORTING es_proto_h TYPE ztaf07_proto_h.

    METHODS _init_features
      IMPORTING iv_seq     TYPE syindex
                is_proto_h TYPE ztaf07_proto_h.

    METHODS _init_components
      IMPORTING iv_seq     TYPE syindex
                is_proto_h TYPE ztaf07_proto_h.

    METHODS _init_services
      IMPORTING iv_seq     TYPE syindex
                is_proto_h TYPE ztaf07_proto_h.

    METHODS _save_bus_obj_tabs.
    METHODS _read_db.
    METHODS _do_commit.

    METHODS _update_and_log.
    METHODS _change_bus_obj_data.
    METHODS _write_doc_changes.

    METHODS _pre_n_write_proto_h_old_new.
    METHODS _pre_n_write_feature_old_new.
    METHODS _pre_n_write_comps_old_new.
    METHODS _pre_n_write_service_old_new.
    METHODS _call_write_doc_at_once.


ENDCLASS.

CLASS lcl_app IMPLEMENTATION.
  METHOD start_of_sel.

    IF p_break EQ abap_true.
      BREAK-POINT .
    ENDIF.

    CASE abap_true.
      WHEN p_init.
        _init_fill( ).
      WHEN p_updany.
        _update_and_log( ).
      WHEN p_del.
      WHEN OTHERS.
        MESSAGE x000(cl) WITH 'never should happen'.
    ENDCASE.


  ENDMETHOD.

  METHOD _init_fill.

    DATA ls_proto_h_new     TYPE ztaf07_proto_h.

    DO 5 TIMES.
      _init_proto_h( EXPORTING iv_seq = sy-index
                    IMPORTING es_proto_h = ls_proto_h_new ).
      _init_features( EXPORTING iv_seq = sy-index
                                is_proto_h = ls_proto_h_new ).
      _init_components( EXPORTING iv_seq = sy-index
                                is_proto_h = ls_proto_h_new ).
      _init_services( EXPORTING iv_seq = sy-index
                                is_proto_h = ls_proto_h_new ).
    ENDDO.
    _save_bus_obj_tabs( ).

    _do_commit( ).

  ENDMETHOD.

  METHOD _init_proto_h.
    "IMPORTING iv_seq     TYPE syindex
    "EXPORTING es_proto_h TYPE ztaf07_proto_h.

    es_proto_h-proto_product_id = |PROTO_N{ iv_seq }|.
    es_proto_h-proto_product_txt = |description for { es_proto_h-proto_product_id }|.
    es_proto_h-begin_work_date = sy-datum + 7.
    es_proto_h-analysis_date = es_proto_h-begin_work_date + 14.
    es_proto_h-estimate_date = es_proto_h-begin_work_date + 16.
    es_proto_h-market_date = es_proto_h-begin_work_date + 49.
    es_proto_h-responsible_team = |TEAMNUM { iv_seq * 4 }|.
    es_proto_h-product_owner = |PERNR_{ iv_seq * 100 }|.
    es_proto_h-qa_team = |TEAMNUM { iv_seq * 7 }|.
    es_proto_h-target_consumer_grp = |USRGRP_A{ iv_seq * 11 }|.

    APPEND es_proto_h TO mt_proto_h_new.
  ENDMETHOD.

  METHOD _init_features.
    "IMPORTING iv_seq     TYPE syindex
    "          is_proto_h TYPE ztaf07_proto_h.
    DATA ls_features_new    TYPE ztaf07_features.

    DO 3 TIMES.
      CLEAR ls_features_new.
      ls_features_new-mandt = cl_abap_syst=>get_client( ).
      ls_features_new-proto_product_id = is_proto_h-proto_product_id.
      ls_features_new-act_feature_val = ''.

      CASE sy-index.
        WHEN 1.
          ls_features_new-proto_feature = |Weight:{ iv_seq }|.
          ls_features_new-plan_feature_val = |{ iv_seq } KG|.
          ls_features_new-complexity_lvl = 50.
          ls_features_new-consumer_yes_lvl = 60.
          ls_features_new-consumer_no_lvl = 90.
        WHEN 2.
          ls_features_new-proto_feature = |Size:{ iv_seq }|.
          ls_features_new-plan_feature_val = |{ iv_seq * 2 } CM|.
          ls_features_new-complexity_lvl = 50.
          ls_features_new-consumer_yes_lvl = 60.
          ls_features_new-consumer_no_lvl = 90.
        WHEN 3.
          ls_features_new-proto_feature = |FunctionN{ iv_seq }|..
          ls_features_new-plan_feature_val = '1 start/stop button'.
          ls_features_new-complexity_lvl = 50.
          ls_features_new-consumer_yes_lvl = 60.
          ls_features_new-consumer_no_lvl = 90.
      ENDCASE.

      APPEND ls_features_new TO mt_features_new.
    ENDDO.

  ENDMETHOD.

  METHOD _init_components.
    "IMPORTING iv_seq     TYPE syindex
    "          is_proto_h TYPE ztaf07_proto_h.

    DATA ls_components_new  TYPE ztaf07_compnents.

    DO 2 TIMES.
      ls_components_new-mandt = cl_abap_syst=>get_client( ).
      ls_components_new-proto_product_id = is_proto_h-proto_product_id.
      ls_components_new-comp_posnr = lines( mt_components_new ) + 1 .
      ls_components_new-proto_quan = 1.
      ls_components_new-proto_uom = 'ST'.
      ls_components_new-component = |COMP_{ iv_seq }:{ sy-tabix }|.
      ls_components_new-component_quan_uom = 'ST'.
      ls_components_new-component_quan_plan = '3'.
      ls_components_new-component_quan_act = 0.
      ls_components_new-comp_unit_price_min = 100.
      ls_components_new-comp_unit_price_max = 350.
      ls_components_new-comp_unit_price_avg = 200.
      ls_components_new-comp_waers = 'RUB'.

      APPEND ls_components_new TO mt_components_new.
    ENDDO.

  ENDMETHOD.

  METHOD _init_services.
    "IMPORTING iv_seq     TYPE syindex
    "          is_proto_h TYPE ztaf07_proto_h.
    DATA ls_services_new    TYPE ztaf07_services.

    DO 1 TIMES.
      ls_services_new-mandt = cl_abap_syst=>get_client( ).
      ls_services_new-proto_product_id = is_proto_h-proto_product_id.
      ls_services_new-serv_posnr = lines( mt_services_new ) + 1 .
      ls_services_new-proto_quan = 2.
      ls_services_new-proto_uom = 'ST'.
      ls_services_new-service = |SRV_{ iv_seq }_{ sy-tabix }|.
      ls_services_new-service_hours_plan = 8.
      ls_services_new-service_hours_act = 0.
      ls_services_new-service_amount_plan = 50000.
      ls_services_new-service_amount_act = 0.
      ls_services_new-service_waers = 'RUB'.
      APPEND ls_services_new TO mt_services_new.
    ENDDO.

  ENDMETHOD.

  METHOD _save_bus_obj_tabs.

    CALL FUNCTION 'Z_AF07_PROTO_UPD_BUSTABS'
      IN UPDATE TASK
      EXPORTING
        it_proto_h    = mt_proto_h_new
        it_features   = mt_features_new
        it_components = mt_components_new
        it_services   = mt_services_new.
  ENDMETHOD.

  METHOD _do_commit.

    DATA ls_return2 TYPE bapiret2.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait   = abap_true
      IMPORTING
        return = ls_return2.

  ENDMETHOD.

  METHOD _update_and_log.
    GET TIME.

    _change_bus_obj_data( ). " prepare_tabs

    _save_bus_obj_tabs( ). " in update_task

    _write_doc_changes( ).

    _do_commit( ).

    MESSAGE s000(cl) WITH 'See ZPROTOTYPE in CDHDR and CDPOS'.
  ENDMETHOD.

  METHOD _change_bus_obj_data.

    " get any record

    DATA ls_proto_h TYPE ztaf07_proto_h.
    FIELD-SYMBOLS <fs_proto_h> TYPE ztaf07_proto_h.
    FIELD-SYMBOLS <fs_feature> TYPE ztaf07_features.
    FIELD-SYMBOLS <fs_component> TYPE ztaf07_compnents.

    _read_db( ).

    LOOP AT mt_proto_h_new ASSIGNING <fs_proto_h>.
      <fs_proto_h>-begin_work_date = <fs_proto_h>-begin_work_date + 1.
      <fs_proto_h>-analysis_date = <fs_proto_h>-analysis_date + 1.
      <fs_proto_h>-estimate_date = <fs_proto_h>-estimate_date + 1.
      <fs_proto_h>-market_date = <fs_proto_h>-market_date + 1.
      ls_proto_h = <fs_proto_h>.
    ENDLOOP.

    " либо добавляется, либо удаляется
    IF lines( mt_components_new ) > 3.
      DELETE mt_components_new FROM 4.
    ELSE.
      _init_components( EXPORTING iv_seq     = lines( mt_components_new )
                                  is_proto_h = ls_proto_h   ).
    ENDIF.

    " либо добавляется, либо удаляется
    IF lines( mt_features_new ) > 2.
      DELETE mt_features_new FROM 3.
    ELSE.
      _init_features( EXPORTING iv_seq     = lines( mt_features_new )
                                is_proto_h = ls_proto_h ).
    ENDIF.

    " mt_services_new
    " без изменений

  ENDMETHOD.

  METHOD _read_db.
    SELECT * FROM ztaf07_proto_h
    INTO TABLE mt_proto_h_old
  UP TO 1 ROWS
  .

    IF mt_proto_h_old IS NOT INITIAL.
      SELECT * FROM ztaf07_features
        INTO TABLE mt_features_old
        FOR ALL ENTRIES IN mt_proto_h_old
        WHERE proto_product_id EQ mt_proto_h_old-proto_product_id
        .

      SELECT * FROM ztaf07_compnents
        INTO TABLE mt_components_old
        FOR ALL ENTRIES IN mt_proto_h_old
        WHERE proto_product_id EQ mt_proto_h_old-proto_product_id
        .

      SELECT * FROM ztaf07_services
        INTO TABLE mt_services_old
        FOR ALL ENTRIES IN mt_proto_h_old
        WHERE proto_product_id EQ mt_proto_h_old-proto_product_id
        .

    ENDIF.

    mt_proto_h_new    = mt_proto_h_old.
    mt_features_new   = mt_features_old.
    mt_components_new = mt_components_old.
    mt_services_new   = mt_services_old.
  ENDMETHOD.

  METHOD _write_doc_changes.

    ms_write_doc_hdr-v_objectid = 'ZPROTOTYPE'.
    ms_write_doc_hdr-v_tcode    = 'ZFILEOS_TCODE7'.

    ms_write_doc_hdr-v_hdr_utime = sy-uzeit.
    ms_write_doc_hdr-v_hdr_udate = sy-datum.
    ms_write_doc_hdr-v_hdr_username  = cl_abap_syst=>get_user_name( ).

    _pre_n_write_proto_h_old_new( ).
    _pre_n_write_feature_old_new( ).
    _pre_n_write_comps_old_new( ).
    _pre_n_write_service_old_new( ).

    _call_write_doc_at_once( ).

  ENDMETHOD.

  METHOD _pre_n_write_proto_h_old_new.
    "EXPORTING ev_change_ind TYPE char1
    "          et_x_new_with_kz TYPE tt_proto_with_kz
    "           et_y_old_with_kz TYPE tt_proto_with_kz.

    DATA lv_tabname_proto_h    TYPE tabname VALUE 'ZTAF07_PROTO_H'.
    DATA lv_if_equal TYPE char1.

    ms_chngdocs_tabs-s_proto-ch_ind = 'U'.
    MOVE-CORRESPONDING mt_proto_h_old TO ms_chngdocs_tabs-s_proto-old_y.
    MOVE-CORRESPONDING mt_proto_h_new TO ms_chngdocs_tabs-s_proto-new_x.

    SORT ms_chngdocs_tabs-s_proto-old_y.
    SORT ms_chngdocs_tabs-s_proto-new_x.

    " cf_reca_storable=>get_change_indicator <- можно и этот способ использовать
    " , но prepare - уже классика и лежит в пакете SCDO
    CALL FUNCTION 'CHANGEDOCUMENT_PREPARE_TABLES'
      EXPORTING
        tablename              = lv_tabname_proto_h
      IMPORTING
        result                 = lv_if_equal
      TABLES
        table_new              = ms_chngdocs_tabs-s_proto-new_x
        table_old              = ms_chngdocs_tabs-s_proto-old_y
      EXCEPTIONS
        nametab_error          = 1
        wrong_structure_length = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      EXIT.
    ENDIF.

    IF lv_if_equal NE mc_tables_not_equal.
      MESSAGE s000(cl) WITH 'no changes was done'.
      CLEAR ms_chngdocs_tabs-s_proto-ch_ind.
    ENDIF.

    IF 1 = 2.
      " если ФМ вызвать нескольо раз, то будет разный номер документа изменения
      CALL FUNCTION 'ZPROTOTYPE_WRITE_DOCUMENT'
        IN UPDATE TASK
        EXPORTING
          objectid           = ms_write_doc_hdr-v_objectid
          tcode              = ms_write_doc_hdr-v_tcode
          utime              = ms_write_doc_hdr-v_hdr_utime
          udate              = ms_write_doc_hdr-v_hdr_udate
          username           = ms_write_doc_hdr-v_hdr_username
          upd_ztaf07_proto_h = ms_chngdocs_tabs-s_proto-ch_ind
        TABLES
          icdtxt_zprototype  = ms_write_doc_hdr-t_cdtxt_tab
          xztaf07_proto_h    = ms_chngdocs_tabs-s_proto-new_x
          yztaf07_proto_h    = ms_chngdocs_tabs-s_proto-old_y.
    ENDIF.


  ENDMETHOD.

  METHOD _pre_n_write_feature_old_new.

    DATA lv_tabname_trg    TYPE tabname VALUE 'ZTAF07_FEATURES'.
    DATA lv_if_equal TYPE char1.

    ms_chngdocs_tabs-s_features-ch_ind = 'U'.

    MOVE-CORRESPONDING mt_features_old TO ms_chngdocs_tabs-s_features-old_y.
    MOVE-CORRESPONDING mt_features_new TO ms_chngdocs_tabs-s_features-new_x.

    SORT ms_chngdocs_tabs-s_features-old_y.
    SORT ms_chngdocs_tabs-s_features-new_x.

    CALL FUNCTION 'CHANGEDOCUMENT_PREPARE_TABLES'
      EXPORTING
*       check_indicator        = SPACE    " Flag whether tables are to be changed
        tablename              = lv_tabname_trg    " Name of the table structure of the internal tables
*       check_only_logflags    = SPACE    " Control Flag: Check Change Document-Relevant Fields Only
      IMPORTING
        result                 = lv_if_equal    " Flag whether tables are identical
      TABLES
        table_new              = ms_chngdocs_tabs-s_features-new_x    " Table contains the changed data
        table_old              = ms_chngdocs_tabs-s_features-old_y    " Table contains the unchanged data
      EXCEPTIONS
        nametab_error          = 1
        wrong_structure_length = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      EXIT.
    ENDIF.

    IF lv_if_equal NE mc_tables_not_equal.
      MESSAGE s000(cl) WITH 'no changes was done:' lv_tabname_trg.
      CLEAR ms_chngdocs_tabs-s_features-ch_ind.
      EXIT.
    ENDIF.

    IF 1 = 2.
      " если ФМ вызвать нескольо раз, то будет разный номер документа изменения
      CALL FUNCTION 'ZPROTOTYPE_WRITE_DOCUMENT'
        IN UPDATE TASK
        EXPORTING
          objectid            = ms_write_doc_hdr-v_objectid
          tcode               = ms_write_doc_hdr-v_tcode
          utime               = ms_write_doc_hdr-v_hdr_utime
          udate               = ms_write_doc_hdr-v_hdr_udate
          username            = ms_write_doc_hdr-v_hdr_username
*         planned_change_number   = SPACE
*         object_change_indicator = 'U'
*         planned_or_real_changes = SPACE
*         no_change_pointers  = SPACE
          upd_ztaf07_features = ms_chngdocs_tabs-s_features-ch_ind
        TABLES
          icdtxt_zprototype   = ms_write_doc_hdr-t_cdtxt_tab
          xztaf07_features    = ms_chngdocs_tabs-s_features-new_x
          yztaf07_features    = ms_chngdocs_tabs-s_features-old_y.
    ENDIF.


  ENDMETHOD.

  METHOD _pre_n_write_comps_old_new.

    DATA lv_tabname_trg    TYPE tabname VALUE 'ZTAF07_COMPNENTS'.
    DATA lv_if_equal TYPE char1.

    ms_chngdocs_tabs-s_components-ch_ind = 'U'.
    MOVE-CORRESPONDING mt_components_old TO ms_chngdocs_tabs-s_components-old_y.
    MOVE-CORRESPONDING mt_components_new TO ms_chngdocs_tabs-s_components-new_x.

    SORT ms_chngdocs_tabs-s_components-old_y.
    SORT ms_chngdocs_tabs-s_components-new_x.

    CALL FUNCTION 'CHANGEDOCUMENT_PREPARE_TABLES'
      EXPORTING
*       check_indicator        = SPACE    " Flag whether tables are to be changed
        tablename              = lv_tabname_trg    " Name of the table structure of the internal tables
*       check_only_logflags    = SPACE    " Control Flag: Check Change Document-Relevant Fields Only
      IMPORTING
        result                 = lv_if_equal    " Flag whether tables are identical
      TABLES
        table_new              = ms_chngdocs_tabs-s_components-new_x    " Table contains the changed data
        table_old              = ms_chngdocs_tabs-s_components-old_y    " Table contains the unchanged data
      EXCEPTIONS
        nametab_error          = 1
        wrong_structure_length = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      EXIT.
    ENDIF.

    IF lv_if_equal NE mc_tables_not_equal.
      MESSAGE s000(cl) WITH 'no changes was done:' lv_tabname_trg.
      CLEAR ms_chngdocs_tabs-s_components-ch_ind.
      EXIT.
    ENDIF.

    IF 1 = 2.
      " если ФМ вызвать нескольо раз, то будет разный номер документа изменения
      CALL FUNCTION 'ZPROTOTYPE_WRITE_DOCUMENT'
        IN UPDATE TASK
        EXPORTING
          objectid             = ms_write_doc_hdr-v_objectid
          tcode                = ms_write_doc_hdr-v_tcode
          utime                = ms_write_doc_hdr-v_hdr_utime
          udate                = ms_write_doc_hdr-v_hdr_udate
          username             = ms_write_doc_hdr-v_hdr_username
*         planned_change_number   = SPACE
*         object_change_indicator = 'U'
*         planned_or_real_changes = SPACE
*         no_change_pointers   = SPACE
          upd_ztaf07_compnents = ms_chngdocs_tabs-s_components-ch_ind
        TABLES
          icdtxt_zprototype    = ms_write_doc_hdr-t_cdtxt_tab
          xztaf07_compnents    = ms_chngdocs_tabs-s_components-new_x
          yztaf07_compnents    = ms_chngdocs_tabs-s_components-old_y.
    ENDIF.
  ENDMETHOD.

  METHOD _pre_n_write_service_old_new.

    DATA lv_tabname_trg    TYPE tabname VALUE 'ZTAF07_SERVICES'.
    DATA lv_if_equal TYPE char1.

    ms_chngdocs_tabs-s_services-ch_ind = 'U'.
    MOVE-CORRESPONDING mt_services_old TO ms_chngdocs_tabs-s_services-old_y.
    MOVE-CORRESPONDING mt_services_new TO ms_chngdocs_tabs-s_services-new_x.

    SORT ms_chngdocs_tabs-s_services-old_y.
    SORT ms_chngdocs_tabs-s_services-new_x.

    CALL FUNCTION 'CHANGEDOCUMENT_PREPARE_TABLES'
      EXPORTING
*       check_indicator        = SPACE    " Flag whether tables are to be changed
        tablename              = lv_tabname_trg    " Name of the table structure of the internal tables
*       check_only_logflags    = SPACE    " Control Flag: Check Change Document-Relevant Fields Only
      IMPORTING
        result                 = lv_if_equal    " Flag whether tables are identical
      TABLES
        table_new              = ms_chngdocs_tabs-s_services-new_x    " Table contains the changed data
        table_old              = ms_chngdocs_tabs-s_services-old_y    " Table contains the unchanged data
      EXCEPTIONS
        nametab_error          = 1
        wrong_structure_length = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO sy-msgli.
      EXIT.
    ENDIF.

    IF lv_if_equal NE mc_tables_not_equal.
      MESSAGE s000(cl) WITH 'no changes was done:' lv_tabname_trg.
      CLEAR ms_chngdocs_tabs-s_services-ch_ind.
      EXIT.
    ENDIF.

    IF 1 = 2.
      " если ФМ вызвать нескольо раз, то будет разный номер документа изменения
      CALL FUNCTION 'ZPROTOTYPE_WRITE_DOCUMENT'
        IN UPDATE TASK
        EXPORTING
          objectid            = ms_write_doc_hdr-v_objectid
          tcode               = ms_write_doc_hdr-v_tcode
          utime               = ms_write_doc_hdr-v_hdr_utime
          udate               = ms_write_doc_hdr-v_hdr_udate
          username            = ms_write_doc_hdr-v_hdr_username
*         planned_change_number   = SPACE
*         object_change_indicator = 'U'
*         planned_or_real_changes = SPACE
*         no_change_pointers  = SPACE
          upd_ztaf07_services = ms_chngdocs_tabs-s_services-ch_ind
        TABLES
          icdtxt_zprototype   = ms_write_doc_hdr-t_cdtxt_tab
          xztaf07_services    = ms_chngdocs_tabs-s_services-new_x
          yztaf07_services    = ms_chngdocs_tabs-s_services-old_y.
    ENDIF.
  ENDMETHOD.

  METHOD _call_write_doc_at_once.


    CALL FUNCTION 'ZPROTOTYPE_WRITE_DOCUMENT'
      IN UPDATE TASK
      EXPORTING
        objectid            = ms_write_doc_hdr-v_objectid
        tcode               = ms_write_doc_hdr-v_tcode
        utime               = ms_write_doc_hdr-v_hdr_utime
        udate               = ms_write_doc_hdr-v_hdr_udate
        username            = ms_write_doc_hdr-v_hdr_username
*       planned_change_number   = SPACE
*       object_change_indicator = 'U'
*       planned_or_real_changes = SPACE
*       no_change_pointers  = SPACE
        upd_ztaf07_services = ms_chngdocs_tabs-s_services-ch_ind
      TABLES
        icdtxt_zprototype   = ms_write_doc_hdr-t_cdtxt_tab
        xztaf07_services    = ms_chngdocs_tabs-s_services-new_x
        yztaf07_services    = ms_chngdocs_tabs-s_services-old_y.


    CALL FUNCTION 'ZPROTOTYPE_WRITE_DOCUMENT'
      IN UPDATE TASK
      EXPORTING
        objectid             = ms_write_doc_hdr-v_objectid
        tcode                = ms_write_doc_hdr-v_tcode
        utime                = ms_write_doc_hdr-v_hdr_utime
        udate                = ms_write_doc_hdr-v_hdr_udate
        username             = ms_write_doc_hdr-v_hdr_username
*       planned_change_number   = SPACE
*       object_change_indicator = 'U'
*       planned_or_real_changes = SPACE
*       no_change_pointers   = SPACE
*       upd_icdtxt_zprototype   = SPACE
        upd_ztaf07_compnents = ms_chngdocs_tabs-s_components-ch_ind
        upd_ztaf07_features  = ms_chngdocs_tabs-s_features-ch_ind
        upd_ztaf07_proto_h   = ms_chngdocs_tabs-s_proto-ch_ind
        upd_ztaf07_services  = ms_chngdocs_tabs-s_services-ch_ind
      TABLES
        icdtxt_zprototype    = ms_write_doc_hdr-t_cdtxt_tab
        xztaf07_compnents    = ms_chngdocs_tabs-s_components-new_x
        yztaf07_compnents    = ms_chngdocs_tabs-s_components-old_y
        xztaf07_features     = ms_chngdocs_tabs-s_features-new_x
        yztaf07_features     = ms_chngdocs_tabs-s_features-old_y
        xztaf07_proto_h      = ms_chngdocs_tabs-s_proto-new_x
        yztaf07_proto_h      = ms_chngdocs_tabs-s_proto-old_y
        xztaf07_services     = ms_chngdocs_tabs-s_services-new_x
        yztaf07_services     = ms_chngdocs_tabs-s_services-old_y.

  ENDMETHOD.

  METHOD _del_some_record.

    " no change docs

    _read_db( ).

    CALL FUNCTION 'Z_AF07_PROTO_UPD_BUSTABS'
      IN UPDATE TASK
      EXPORTING
        it_proto_h    = mt_proto_h_new
        it_features   = mt_features_new
        it_components = mt_components_new
        it_services   = mt_services_new
        iv_mode       = 'D'.

    _do_commit( ).

  ENDMETHOD.

ENDCLASS.
