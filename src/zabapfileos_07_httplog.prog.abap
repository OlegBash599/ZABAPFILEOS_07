*&---------------------------------------------------------------------*
*& Report ZABAPFILEOS_07_HTTPLOG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapfileos_07_httplog.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
PARAMETERS: p_extsrv TYPE string OBLIGATORY DEFAULT 'http://www.olegbash.ru/abfos/' LOWER CASE.

PARAMETERS: httpcall RADIOBUTTON GROUP r1 DEFAULT 'X',
            showlog  RADIOBUTTON GROUP r1,
            clearlog RADIOBUTTON GROUP r1
            .
SELECTION-SCREEN END OF BLOCK b01.

PARAMETERS p_break AS CHECKBOX DEFAULT ''.

SET EXTENDED CHECK OFF.
INCLUDE sbal_constants.
SET EXTENDED CHECK ON.

INCLUDE zabapfileos_07_httplog_cls3 IF FOUND. " local log wrapper
INCLUDE zabapfileos_07_httplog_cls2 IF FOUND. " log show
INCLUDE zabapfileos_07_httplog_cls1 IF FOUND. " http calls


INCLUDE zabapfileos_07_httplog_cls99 IF FOUND. " app

INITIALIZATION.
  go_app = NEW #( ).

START-OF-SELECTION.
  go_app->start_of_sel( ).
