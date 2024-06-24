FUNCTION z_af07_proto_upd_bustabs.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_PROTO_H) TYPE  ZTTAF07_PROTO_H OPTIONAL
*"     VALUE(IT_FEATURES) TYPE  ZTTAF07_FEATURES OPTIONAL
*"     VALUE(IT_COMPONENTS) TYPE  ZTTAF07_COMPNENTS OPTIONAL
*"     VALUE(IT_SERVICES) TYPE  ZTTAF07_SERVICES OPTIONAL
*"     VALUE(IV_MODE) TYPE  UPDKZ_D DEFAULT 'A'
*"----------------------------------------------------------------------
  CONSTANTS lc_all_reload TYPE updkz_d VALUE 'A'.
  CONSTANTS lc_modify TYPE updkz_d VALUE 'M'.
  CONSTANTS lc_delete TYPE updkz_d VALUE 'D'.

  FIELD-SYMBOLS <fs_prot_h> TYPE ztaf07_proto_h.

  CASE iv_mode.
    WHEN lc_all_reload.
      LOOP AT it_proto_h ASSIGNING <fs_prot_h>.
        DELETE FROM ztaf07_proto_h   WHERE proto_product_id  = <fs_prot_h>-proto_product_id.
        DELETE FROM ztaf07_features  WHERE proto_product_id  = <fs_prot_h>-proto_product_id.
        DELETE FROM ztaf07_compnents WHERE proto_product_id  = <fs_prot_h>-proto_product_id.
        DELETE FROM ztaf07_services  WHERE proto_product_id  = <fs_prot_h>-proto_product_id.
      ENDLOOP.
      MODIFY ztaf07_proto_h   FROM TABLE it_proto_h.
      MODIFY ztaf07_features  FROM TABLE it_features.
      MODIFY ztaf07_compnents FROM TABLE it_components.
      MODIFY ztaf07_services  FROM TABLE it_services.

    WHEN lc_modify.
      MODIFY ztaf07_proto_h   FROM TABLE it_proto_h.
      MODIFY ztaf07_features  FROM TABLE it_features.
      MODIFY ztaf07_compnents FROM TABLE it_components.
      MODIFY ztaf07_services  FROM TABLE it_services.
    WHEN lc_delete.
      DELETE ztaf07_proto_h   FROM TABLE it_proto_h.
      DELETE ztaf07_features  FROM TABLE it_features.
      DELETE ztaf07_compnents FROM TABLE it_components.
      DELETE ztaf07_services  FROM TABLE it_services.

    WHEN OTHERS.
  ENDCASE.

ENDFUNCTION.
