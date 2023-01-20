CLASS zcl_edoc_message_us DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_edocument_message_ext .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_edoc_message_us IMPLEMENTATION.


  METHOD if_edocument_message_ext~determine_company_code.
  ENDMETHOD.


  METHOD if_edocument_message_ext~determine_edoc_type.
  rv_edoc_type = 'ZUSINVIN'.
  ENDMETHOD.


  METHOD if_edocument_message_ext~determine_file_type.
  ENDMETHOD.


  METHOD if_edocument_message_ext~determine_operation.
      CONSTANTS : lc_update TYPE char1 VALUE 'U',
                lc_create TYPE char1 VALUE 'C'.

   rv_operation = lc_create.
  ENDMETHOD.


  METHOD if_edocument_message_ext~determine_partner.
  ENDMETHOD.


  METHOD if_edocument_message_ext~determine_process_attributes.
  ENDMETHOD.
ENDCLASS.
