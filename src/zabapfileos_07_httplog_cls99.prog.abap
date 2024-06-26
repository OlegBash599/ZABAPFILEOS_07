*&---------------------------------------------------------------------*
*&  Include           ZABAPFILEOS_07_HTTPLOG_CLS99
*&---------------------------------------------------------------------*

CLASS lcl_app DEFINITION.

  PUBLIC SECTION.


    METHODS constructor.
    METHODS init.
    METHODS start_of_sel.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mo_log TYPE REF TO lcl_log.
ENDCLASS.


CLASS lcl_app IMPLEMENTATION.
  METHOD constructor.
    me->init( ).
  ENDMETHOD.

  METHOD init.

  ENDMETHOD.

  METHOD start_of_sel.

    IF p_break EQ abap_true.
      BREAK-POINT .
    ENDIF.

    CASE abap_true.
      WHEN httpcall.
        CREATE OBJECT mo_log.
        NEW lcl_http_forecast( io_log = mo_log )->fn( ).
      WHEN showlog.
        NEW  lcl_log_view( )->fn( ).
      WHEN clearlog.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.


ENDCLASS.

DATA go_app TYPE REF TO lcl_app.
