*&---------------------------------------------------------------------*
*& Report ZABAPFILEOS_07_PROTOTYPE_OBJ
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabapfileos_07_prototype_obj.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
PARAMETERS: p_init   RADIOBUTTON GROUP dm1,
            p_updany RADIOBUTTON GROUP dm1 DEFAULT 'X',
            p_del    RADIOBUTTON GROUP dm1
            .
SELECTION-SCREEN END OF BLOCK b01.

PARAMETERS p_break AS CHECKBOX DEFAULT ''.

INCLUDE zabapfileos_07_proto_obj_cls1 IF FOUND.

START-OF-SELECTION.
  NEW lcl_app( )->start_of_sel( ).
