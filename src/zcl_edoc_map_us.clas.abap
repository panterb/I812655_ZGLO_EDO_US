CLASS zcl_edoc_map_us DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA ms_source TYPE if_edoc_ext_datatypes=>mty_mapping_source_s .

    TYPES:
      BEGIN OF ms_pull_req,
        edoc_incoming_message_tab TYPE if_edoc_ext_datatypes=>mty_edoc_incom_msg_t,
        bukrs                     TYPE bukrs,
        vat_id(9)                 TYPE c,
      END OF ms_pull_req.

*************************For Submit
    METHODS map_invoice_request_fi
      IMPORTING
        !is_source TYPE any
      EXPORTING
        !ed_target TYPE REF TO data .

    METHODS map_invoice_response_fi
      IMPORTING
        !is_source TYPE any
      EXPORTING
        !ed_target TYPE REF TO data .


    CLASS-METHODS call_proxy
      IMPORTING
        !is_source       TYPE any
        !iv_logical_port TYPE prx_logical_port_name
      EXPORTING
        !ed_response     TYPE REF TO data
        !ev_xstring      TYPE xstring .

************************For Get Status
    METHODS map_status_request
      IMPORTING
        !is_source TYPE any
      EXPORTING
        !ed_target TYPE REF TO data .

    METHODS map_status_response
      IMPORTING
        !is_source TYPE any
      EXPORTING
        !ed_target TYPE REF TO data .

    CLASS-METHODS call_status_proxy
      IMPORTING
        !is_source       TYPE any
        !iv_logical_port TYPE prx_logical_port_name
      EXPORTING
        !ed_response     TYPE REF TO data
        !ev_xstring      TYPE xstring .

************************For Get/Pull Invoice
    METHODS map_pull_request
      IMPORTING
        !is_source TYPE any
      EXPORTING
        !ed_target TYPE REF TO data .

    METHODS map_pull_response
      IMPORTING
        !is_source TYPE any
      EXPORTING
        !ed_target TYPE REF TO data .

    CLASS-METHODS call_get_invocie_proxy
      IMPORTING
        !is_source   TYPE any
      EXPORTING
        !ed_response TYPE REF TO data
        !ev_xstring  TYPE xstring .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_edoc_map_us IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDOC_MAP_US->MAP_INVOICE_REQUEST_FI
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SOURCE                      TYPE        ANY
* | [<---] ED_TARGET                      TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD map_invoice_request_fi.
    DATA ld_target TYPE REF TO zedo_us_send_invoice_request.
    DATA ls_source TYPE if_edoc_ext_datatypes=>mty_mapping_source_s.
    DATA ls_bseg LIKE LINE OF ls_source-bseg.
    DATA lv_amount LIKE ls_bseg-wrbtr.

    FIELD-SYMBOLS <ls_target> TYPE zedo_us_send_invoice_request.
    CREATE DATA ld_target.
    ASSIGN ld_target->* TO <ls_target>.

    ls_source = is_source.

    <ls_target>-parameters-uuid = ls_source-reference-edoc_guid.
    <ls_target>-parameters-seller_id = ls_source-bkpf-bukrs.
    <ls_target>-parameters-invoice_id = ls_source-bkpf-belnr.
    <ls_target>-parameters-invoice_issue_date = ls_source-bkpf-budat.

    READ TABLE ls_source-bseg INTO ls_bseg WITH KEY koart = 'D'.
    <ls_target>-parameters-invoice_amount = ls_bseg-wrbtr.
    <ls_target>-parameters-buyer_id = ls_bseg-kunnr.
    ed_target = ld_target.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_EDOC_MAP_US=>CALL_PROXY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SOURCE                      TYPE        ANY
* | [--->] IV_LOGICAL_PORT                TYPE        PRX_LOGICAL_PORT_NAME
* | [<---] ED_RESPONSE                    TYPE REF TO DATA
* | [<---] EV_XSTRING                     TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD call_proxy.


    DATA lo_proxy TYPE REF TO zedo_us_co_e_doc_usexensibilit.
    DATA ls_input TYPE zedo_us_send_invoice_request.
    DATA ld_response TYPE REF TO zedo_us_send_invoice_response1.
    FIELD-SYMBOLS <ls_output> TYPE zedo_us_send_invoice_response1.
    DATA ls_response TYPE zedo_us_send_invoice_response1.

    CREATE DATA ld_response.
    ASSIGN ld_response->* TO <ls_output>.

    ls_input = is_source.

    TRY.
        DATA(destination) = cl_soap_destination_provider=>create_by_comm_arrangement(
                              comm_scenario  = 'ZEDO_US_INVOICE'
                              service_id     = 'ZEDO_US_SEND_INVOICE_SPRX'
*                          comm_system_id = '<Communication System Identifier>'
                            ).
        DATA(proxy) = NEW zedo_us_co_e_doc_usexensibilit(
                        destination = destination
                      ).
        DATA(request) = VALUE zedo_us_send_invoice_request( ).
        MOVE-CORRESPONDING ls_input TO request.
        proxy->send_invoice(
          EXPORTING
            input = request
          IMPORTING
            output = <ls_output>
        ).
        "handle response
      CATCH cx_soap_destination_error.
        "handle error
      CATCH cx_ai_system_fault.
        "handle error
    ENDTRY.
    CALL TRANSFORMATION zedo_us_send_invoice_request SOURCE root = ls_input
                            RESULT XML ev_xstring.
    ed_response = ld_response.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDOC_MAP_US->MAP_INVOICE_RESPONSE_FI
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SOURCE                      TYPE        ANY
* | [<---] ED_TARGET                      TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD map_invoice_response_fi.
    DATA ld_target TYPE REF TO zif_edo_datatypes_us=>mty_mapping_target_us_s.
    DATA ls_source TYPE zedo_us_send_invoice_response1.

    FIELD-SYMBOLS <ls_target> TYPE zif_edo_datatypes_us=>mty_mapping_target_us_s.

    CREATE DATA ld_target.
    ASSIGN ld_target->* TO <ls_target>.
    ls_source = is_source.

*    Prepare response XML
    CALL TRANSFORMATION zedo_us_send_invoice_response1 SOURCE root = ls_source
                          RESULT XML DATA(lv_xml).

    <ls_target>-response_xml = lv_xml.


    <ls_target>-response_id = ls_source-parameters-tax_authority_id.
    IF ls_source-parameters-tax_authority_id IS NOT INITIAL.
      <ls_target>-status = 'Success'.
      <ls_target>-track_id = ls_source-parameters-tax_authority_id.
    ENDIF.


    ed_target = ld_target.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZCL_EDOC_MAP_US=>CALL_STATUS_PROXY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SOURCE                      TYPE        ANY
* | [--->] IV_LOGICAL_PORT                TYPE        PRX_LOGICAL_PORT_NAME
* | [<---] ED_RESPONSE                    TYPE REF TO DATA
* | [<---] EV_XSTRING                     TYPE        XSTRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD call_status_proxy.


    DATA lo_proxy TYPE REF TO zedo_us_co_e_doc_usget_status.
    DATA ls_input TYPE zedo_us_get_status_request.
    DATA ld_response TYPE REF TO zedo_us_get_status_response.
    FIELD-SYMBOLS <ls_output> TYPE zedo_us_get_status_response.
    DATA ls_response TYPE zedo_us_get_status_response.

    CREATE DATA ld_response.
    ASSIGN ld_response->* TO <ls_output>.

    ls_input = is_source.

    TRY.
        DATA(destination) = cl_soap_destination_provider=>create_by_comm_arrangement(
                              comm_scenario  = 'ZEDO_US_INVOICE'
                              service_id     = 'ZEDO_US_GET_STATUS_SPRX'
*                          comm_system_id = '<Communication System Identifier>'
                            ).
        DATA(proxy) = NEW zedo_us_co_e_doc_usget_status(
                        destination = destination
                      ).

        DATA(request) = VALUE zedo_us_get_status_request( ).
        MOVE-CORRESPONDING ls_input TO request.
        proxy->get_status(
          EXPORTING
            input = request
          IMPORTING
            output = <ls_output>
        ).
        "handle response
      CATCH cx_soap_destination_error.
        "handle error
      CATCH cx_ai_system_fault.
        "handle error
    ENDTRY.
*    CALL TRANSFORMATION zedo_us_send_invoice_request SOURCE root = ls_input
*                            RESULT XML ev_xstring.
    ed_response = ld_response.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDOC_MAP_US->MAP_STATUS_REQUEST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SOURCE                      TYPE        ANY
* | [<---] ED_TARGET                      TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD map_status_request.
    DATA ld_target TYPE REF TO zedo_us_get_status_request.
    DATA ls_source TYPE if_edoc_ext_datatypes=>mty_edocument_s.
    DATA ls_zedous TYPE zedous.
    FIELD-SYMBOLS <ls_target> TYPE zedo_us_get_status_request.
    CREATE DATA ld_target.
    ASSIGN ld_target->* TO <ls_target>.

    ls_source = is_source.

    SELECT SINGLE * FROM zedous WHERE edoc_guid = @ls_source-edoc_guid INTO CORRESPONDING FIELDS OF @ls_zedous.

    <ls_target>-parameters-track_id = ls_zedous-trackid.
    ed_target = ld_target.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDOC_MAP_US->MAP_STATUS_RESPONSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_SOURCE                      TYPE        ANY
* | [<---] ED_TARGET                      TYPE REF TO DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD map_status_response.
    DATA ld_target TYPE REF TO zif_edo_datatypes_us=>mty_mapping_target_us_s.
    DATA ls_source TYPE zedo_us_get_status_response.

    FIELD-SYMBOLS <ls_target> TYPE zif_edo_datatypes_us=>mty_mapping_target_us_s.

    CREATE DATA ld_target.
    ASSIGN ld_target->* TO <ls_target>.
    ls_source = is_source.

*    Prepare response XML
*     CALL TRANSFORMATION ZEDO_US_SEND_INVOICE_RESPONSE1 SOURCE ROOT = ls_source
*                           RESULT XML DATA(lv_xml).

*      <ls_target>-response_xml = lv_xml.



     TRY.
         <ls_target>-response_xml = cl_edoc_util_ext=>convert_ddic_to_xml_xstring(
       EXPORTING iv_abap_data =  ls_source
        iv_ddic_type = 'ZEDO_US_GET_STATUS_RESPONSE' ).
       CATCH cx_edocument_ext.
         "handle exception
     ENDTRY.
    <ls_target>-status = ls_source-parameters-status.
    ed_target = ld_target.
  ENDMETHOD.

  METHOD call_get_invocie_proxy.

    DATA ls_request TYPE zedo_us_get_invoice_request.
    DATA ld_response TYPE REF TO zedo_us_get_invoice_response.
    FIELD-SYMBOLS <ls_output> TYPE zedo_us_get_invoice_response.
    DATA ls_response TYPE zedo_us_get_status_response.

    CREATE DATA ld_response.
    ASSIGN ld_response->* TO <ls_output>.

    ls_request = is_source.

    TRY.
        DATA(destination) = cl_soap_destination_provider=>create_by_comm_arrangement(
                              comm_scenario  = 'ZEDO_US_INVOICE'
                              service_id     = 'ZEDO_US_GET_INVOICE_SPRX'
                            ).
        DATA(proxy) = NEW zedo_us_co_e_doc_usget_invoice(
                        destination = destination
                      ).
        DATA(request) = VALUE zedo_us_get_invoice_request( ).
        proxy->get_invoice(
          EXPORTING
            input = ls_request
          IMPORTING
            output = <ls_output>
        ).
        "handle response
      CATCH cx_soap_destination_error.
        "handle error
      CATCH cx_ai_system_fault.
        "handle error
    ENDTRY.

    ed_response = ld_response.

  ENDMETHOD.

  METHOD map_pull_request.

    DATA:
      ls_source TYPE if_edoc_ext_datatypes=>mty_edoc_s_bukrs_s,
      ld_target TYPE REF TO zedo_us_get_invoice_request.

    FIELD-SYMBOLS: <fs_target> TYPE zedo_us_get_invoice_request.

    CREATE DATA ld_target.
    ASSIGN ld_target->* TO <fs_target>.

    ls_source = is_source.

**** Mapping External target pull request
    <fs_target>-parameters-seller_id = ls_source-low.

    ed_target = ld_target.

  ENDMETHOD.

  METHOD map_pull_response.

    DATA:
      ls_source  TYPE zedo_us_get_invoice_response,
      ld_target  TYPE REF TO ms_pull_req,
      lv_xml     TYPE xstring,
      lv_inv_xml TYPE xstring,
      ls_msg     TYPE if_edoc_ext_datatypes=>mty_edoc_incom_msg_s.

    FIELD-SYMBOLS:
                   <fs_target> TYPE  ms_pull_req.

*   Set source object
    ls_source = is_source.
    CREATE DATA ld_target.
    ASSIGN ld_target->* TO <fs_target>.

**** Mapping External target pull response

    "Convert XML into ABAP str.
    LOOP AT ls_source-parameters-invoices INTO DATA(ls_inv).

      CONCATENATE 'US_INCOMING' sy-datum sy-timlo
             INTO ls_msg-message_name SEPARATED BY '_'.
      CONCATENATE ls_msg-message_name '.xml' INTO ls_msg-message_name.
      ls_msg-xml_string = ls_inv-invoice.
      APPEND ls_msg TO <fs_target>-edoc_incoming_message_tab.

    ENDLOOP.

    ed_target = ld_target.

  ENDMETHOD.

ENDCLASS.
