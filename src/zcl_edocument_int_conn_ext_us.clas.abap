"! <p class="shorttext synchronized" lang="en">Implementation class for interface connector extensiblity</p>
CLASS zcl_edocument_int_conn_ext_us DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_edocument_int_conn_ext .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_edocument_int_conn_ext_us IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDOCUMENT_INT_CONN_EXT_US->IF_EDOCUMENT_INT_CONN_EXT~CLEAN_UP_MESSAGES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_MESSAGE                     TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_INCOM_MSG_T
* | [<-->] CT_CLEANED_UP                  TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_INCOM_MSG_T
* | [!CX!] CX_EDOCUMENT_EXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_edocument_int_conn_ext~clean_up_messages.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDOCUMENT_INT_CONN_EXT_US->IF_EDOCUMENT_INT_CONN_EXT~DISPLAY_EDOCUMENT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_EDOC_GUID                   TYPE        IF_EDOC_CLOUD_DATATYPES=>MTY_EDOC_GUID
* | [<-->] CV_DISPLAY_DONE                TYPE        IF_EDOC_CLOUD_DATATYPES=>MTY_ABAP_BOOL
* | [<-->] CT_LOG_MESSAGE                 TYPE        IF_EDOC_EXT_DATATYPES=>MTY_BAPIRET_T
* | [!CX!] CX_EDOCUMENT_EXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_edocument_int_conn_ext~display_edocument.
    DATA:
      lo_edocument_ext_es TYPE REF TO cl_edocument_ext_es,
*      lo_edocument_ext    TYPE REF TO cl_edocument_ext,
      ls_preview          TYPE abap_bool,
      lt_edocumentfile    TYPE if_edoc_ext_datatypes=>mty_edocumentfile_t,
      ls_log_message      TYPE bapiret2,
      ls_edocumentfile    TYPE if_edoc_ext_datatypes=>mty_edocumentfile_s,
      lr_edocument        TYPE REF TO if_edoc_ext_datatypes=>mty_edocument_s,
      lr_error_text       TYPE REF TO string.

    DATA ls_req TYPE zedo_us_send_invoice_request.
    FIELD-SYMBOLS <ls_send_req> TYPE zedo_us_send_invoice_request.
    DATA lr_send_req TYPE REF TO data.
    DATA ld_source_Data TYPE REF TO data.
    DATA lr_edoc_map_us TYPE REF TO zcl_edoc_map_us.
    FIELD-SYMBOLS <fs_source_Data> TYPE any.

**   Get eDocument

    cl_edocument_ext_es=>retrieve_by_edoc_guid(
      EXPORTING
        iv_edoc_guid          =  iv_edoc_guid                 " eDocument GUID
      RECEIVING
        ro_edocument_ext_es   =  lo_edocument_ext_es
    ).

    lr_edocument = lo_edocument_ext_es->get_edocument_structure( ).

    CASE lr_edocument->proc_status.
      WHEN 'CREATED' OR 'SEND_REQ'.
        ls_preview = abap_true.
      WHEN 'ACCEPT' OR 'REJECT'.
        ls_preview = abap_false.
      WHEN OTHERS.
        ls_preview = abap_true.
    ENDCASE.


    IF ls_preview IS NOT INITIAL AND lr_edocument->process = 'ZUSINV'.
      " for Preview, Get the request payload


      lr_edoc_map_us = NEW zcl_edoc_map_us( ).

      IF lr_edocument->source_type = 'FI_INVOICE'.

        cl_edocument_ext_es=>retrieve_by_source_key(

        EXPORTING iv_source_key = lr_edocument->source_key
                  iv_source_type = lr_edocument->source_type

        RECEIVING ro_edocument_ext_es = lo_edocument_ext_es

        ).
        me->if_edocument_int_conn_ext~get_source_data(
      EXPORTING io_edocument_ext_es = lo_edocument_ext_es
      CHANGING ed_country_source_data = ld_source_Data
       ).
        ASSIGN ld_source_Data->* TO <fs_source_Data>.
        lr_edoc_map_us->map_invoice_request_fi(
        EXPORTING is_source = <fs_source_data>
        IMPORTING ed_target = lr_send_req
        ).

        ASSIGN lr_send_req->* TO <ls_send_req>.
        MOVE-CORRESPONDING <ls_send_req> TO ls_req.
      ELSEIF lr_edocument->source_type = 'SD_INVOICE'.

      ENDIF.
      CALL TRANSFORMATION zedo_us_send_invoice_request SOURCE root = ls_req
                           RESULT XML DATA(lv_xml).

      ls_edocumentfile-create_date = sy-datum.
      ls_edocumentfile-create_time = sy-uzeit.
      ls_edocumentfile-file_raw    = lv_xml.
      ls_edocumentfile-edoc_guid   = iv_edoc_guid.
      CONCATENATE lr_edocument->source_type
                  lr_edocument->source_key '.XML'
      INTO ls_edocumentfile-file_name
      SEPARATED BY '_'.

      APPEND ls_edocumentfile TO lt_edocumentfile.


    ELSEIF lr_edocument->process = 'ZUSINVIN'.
    lt_edocumentfile = lo_edocument_ext_es->get_edoc_files( ).
      READ TABLE lt_edocumentfile INTO ls_edocumentfile
                              WITH KEY file_type = 'INBOUND'.
      IF sy-subrc <> 0 OR ls_edocumentfile-file_raw IS INITIAL.

        lr_error_text = lo_edocument_ext_es->get_error_text( ).
*        MESSAGE e000(edocument) WITH 'No file for display' INTO lr_error_text->*.
        ls_log_message-message = lr_error_text->*.
        ls_log_message-type = 'W'.
        ls_log_message-id         = sy-msgid.
        ls_log_message-number     = sy-msgno.
        ls_log_message-message_v1 = 'No file for display'.
        APPEND ls_log_message TO ct_log_message.
        RETURN.
      ENDIF.

    ELSE.

      lt_edocumentfile = lo_edocument_ext_es->get_edoc_files( ).
      READ TABLE lt_edocumentfile INTO ls_edocumentfile
                              WITH KEY file_type = 'REQUEST'.
      IF sy-subrc <> 0 OR ls_edocumentfile-file_raw IS INITIAL.

        lr_error_text = lo_edocument_ext_es->get_error_text( ).
*        MESSAGE e000(edocument) WITH 'No file for display' INTO lr_error_text->*.
        ls_log_message-message = lr_error_text->*.
        ls_log_message-type = 'W'.
        ls_log_message-id         = sy-msgid.
        ls_log_message-number     = sy-msgno.
        ls_log_message-message_v1 = 'No file for display'.
        APPEND ls_log_message TO ct_log_message.
        RETURN.
      ENDIF.
    ENDIF.

    DATA: lo_util_ext TYPE REF TO cl_edoc_util_ext.
    DATA lv_folder  TYPE  if_edoc_ext_datatypes=>mty_text_element.

    cl_edoc_util_ext=>display_xml(
      EXPORTING
        iv_xml_string           = ls_edocumentfile-file_raw
        iv_file_name            = ls_edocumentfile-file_name                  " File Name
        iv_is_preview           = ls_preview
    ).

    cv_display_done = abap_true.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDOCUMENT_INT_CONN_EXT_US->IF_EDOCUMENT_INT_CONN_EXT~GET_SOURCE_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IO_EDOCUMENT_EXT_ES            TYPE REF TO CL_EDOCUMENT_EXT_ES
* | [<-->] ED_COUNTRY_SOURCE_DATA         TYPE REF TO DATA
* | [!CX!] CX_EDOCUMENT_EXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_edocument_int_conn_ext~get_source_data.

    DATA ld_source_data TYPE REF TO if_edoc_ext_datatypes=>mty_mapping_source_s.
    DATA ls_fi_invoice    TYPE if_edoc_ext_datatypes=>mty_edoc_src_data_fi_inv_s.
    DATA lr_edocument     TYPE REF TO if_edoc_ext_datatypes=>mty_edocument_s.
    FIELD-SYMBOLS: <ls_source_data> TYPE if_edoc_ext_datatypes=>mty_mapping_source_s.

    CREATE DATA ld_source_data.
    ASSIGN ld_source_data->* TO <ls_source_data>.

    lr_edocument = io_edocument_ext_es->get_edocument_structure( ).

    CASE lr_edocument->source_type.
      WHEN 'FI_INVOICE'.
        io_edocument_ext_es->get_source_data( IMPORTING es_source_data = ls_fi_invoice ).

        <ls_source_data>-source_header = ls_fi_invoice-source_header.
        <ls_source_data>-reference-edoc_guid = lr_edocument->edoc_guid.
        <ls_source_data>-bkpf = ls_fi_invoice-document_header.
        <ls_source_data>-bseg = ls_fi_invoice-document_item.
        <ls_source_data>-bset = ls_fi_invoice-tax_data.
        <ls_source_data>-bsec = ls_fi_invoice-onetime_customer.

      WHEN OTHERS.
    ENDCASE.

    ed_country_source_data = ld_source_data.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDOCUMENT_INT_CONN_EXT_US->IF_EDOCUMENT_INT_CONN_EXT~PREPARE_MESSAGES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_MAX_NUMBER                  TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_MAX_NUMBER
* | [--->] IT_MESSAGE_TYPE                TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_MSG_TYPE_T
* | [--->] IT_BUKRS_RNG                   TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_S_BUKRS_T
* | [--->] IT_DATE_RECEIVED_RNG           TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_S_MSG_DATE_REC_T
* | [--->] IT_TIME_RECEIVED_RNG           TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_S_MSG_TIME_REC_T(optional)
* | [<-->] CT_PREPARATION_DATA            TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_MSG_PREP_DATA_T
* | [!CX!] CX_EDOCUMENT_EXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_edocument_int_conn_ext~prepare_messages.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDOCUMENT_INT_CONN_EXT_US->IF_EDOCUMENT_INT_CONN_EXT~PULL_MESSAGES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_PREPARATION_DATA            TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_MSG_PREP_DATA_T(optional)
* | [--->] IV_MAX_NUMBER                  TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_MAX_NUMBER
* | [--->] IT_MESSAGE_TYPE                TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_MSG_TYPE_T(optional)
* | [--->] IT_BUKRS_RNG                   TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_S_BUKRS_T(optional)
* | [--->] IT_DATE_RECEIVED_RNG           TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_S_MSG_DATE_REC_T(optional)
* | [--->] IT_TIME_RECEIVED_RNG           TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_S_MSG_TIME_REC_T(optional)
* | [<-->] CT_MESSAGE                     TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_INCOM_MSG_T
* | [!CX!] CX_EDOCUMENT_EXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_edocument_int_conn_ext~pull_messages.

    DATA: lv_method_name    TYPE if_edoc_ext_datatypes=>mty_method_name,
          lv_map_class_name TYPE if_edoc_ext_datatypes=>mty_class_name.

    DATA:
      lv_interface_id_out TYPE if_edoc_ext_datatypes=>mty_interface_id,
      lv_int_version_out  TYPE if_edoc_ext_datatypes=>mty_int_version,
      lv_int_version_in   TYPE if_edoc_ext_datatypes=>mty_int_version,
      lv_xstring          TYPE xstring,
      lo_edoc_util_ext    TYPE REF TO cl_edoc_util_ext,
      lo_map_class        TYPE REF TO object.

    FIELD-SYMBOLS: <ls_req_source_structure> TYPE any,
                   <ls_req_target_structure> TYPE any.
    FIELD-SYMBOLS: <ls_res_source_structure> TYPE any,
                   <ls_res_target_structure> TYPE any.

    DATA: ld_req_target TYPE REF TO data,
          ld_res_source TYPE REF TO data,
          ld_res_target TYPE REF TO data,
          ld_data       TYPE REF TO data.

    FIELD-SYMBOLS: <fs_msg>     TYPE ANY TABLE,
                   <ls_message> TYPE if_edoc_ext_datatypes=>mty_edoc_incom_msg_s,
                   <ls_msgtype> TYPE if_edoc_ext_datatypes=>mty_edoc_msg_type_s.

    CLEAR ct_message.

    lo_edoc_util_ext = cl_edoc_util_ext=>get_object( ).

*    DATA lv_burks TYPE burks.
    READ TABLE it_bukrs_rng INTO DATA(ls_burks) INDEX 1.
*    IF sy-subrc = 0.
*      lv_burks = ls_burks-low.
*    ENDIF.

    LOOP AT it_message_type ASSIGNING <ls_msgtype>.

      " Get the message type details
      lo_edoc_util_ext->determine_msg_type_interface(
                                    EXPORTING iv_message_type = <ls_msgtype>-message_type
                                    IMPORTING es_edomsgtypeint = DATA(ls_edomsgtypeint) ).

* determine mapping class & Method
      lv_map_class_name = 'ZCL_EDOC_MAP_US'.
      lv_method_name = 'MAP_PULL_REQUEST'.

      CREATE DATA ld_data TYPE if_edoc_ext_datatypes=>MTY_EDOC_S_BUKRS_s.

      GET REFERENCE OF ls_burks INTO ld_data.
      ASSIGN  ld_data->* TO <ls_req_source_structure>.

      CREATE OBJECT lo_map_class TYPE (lv_map_class_name).
      CALL METHOD lo_map_class->(lv_method_name)
        EXPORTING
          is_source = <ls_req_source_structure>
        IMPORTING
          ed_target = ld_req_target.

      ASSIGN ld_req_target->* TO <ls_req_target_structure>.


* call get invoice proxy
      CALL METHOD zcl_edoc_map_us=>call_get_invocie_proxy
        EXPORTING
          is_source   = <ls_req_target_structure>
        IMPORTING
          ed_response = ld_res_source
          ev_xstring  = lv_xstring.

* Response mapping

      ASSIGN ld_res_source->* TO <ls_res_source_structure>.
      lv_method_name = 'MAP_PULL_RESPONSE'.

      CALL METHOD lo_map_class->(lv_method_name)
        EXPORTING
          is_source = <ls_res_source_structure>
        IMPORTING
          ed_target = ld_res_target.


* Mapping incoming message
      ASSIGN ld_res_target->* TO <ls_res_target_structure>.

      ASSIGN COMPONENT 'EDOC_INCOMING_MESSAGE_TAB' OF STRUCTURE <ls_res_target_structure> TO <fs_msg>.

      LOOP AT <fs_msg> ASSIGNING <ls_message>.
        MOVE-CORRESPONDING <ls_msgtype> TO <ls_message>.
        <ls_message>-interface_id_out  = ls_edomsgtypeint-interface_id_out.
*        <ls_message>-int_version_out = lv_int_version_out.
        <ls_message>-interface_id_in = ls_edomsgtypeint-interface_id_in.
*        <ls_message>-int_version_in  = lv_int_version_in.
        <ls_message>-bukrs = ls_burks-low.
        <ls_message>-land = <ls_msgtype>-land.
        <ls_message>-change_date = sy-datum.
        <ls_message>-change_time = sy-timlo.
        <ls_message>-message_type = <ls_msgtype>-message_type.
        GET TIME STAMP FIELD DATA(ts).
        <ls_message>-received_at = ts.
      ENDLOOP.

      APPEND LINES OF <fs_msg> TO ct_message.

    ENDLOOP.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDOCUMENT_INT_CONN_EXT_US->IF_EDOCUMENT_INT_CONN_EXT~TRIGGER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_EDOC_GUID                   TYPE        IF_EDOC_CLOUD_DATATYPES=>MTY_EDOC_GUID
* | [--->] IO_EDOCUMENT_EXT_ES            TYPE REF TO CL_EDOCUMENT_EXT_ES
* | [--->] IV_INTERFACE_ID                TYPE        IF_EDOC_EXT_DATATYPES=>MTY_INTERFACE_ID(optional)
* | [--->] IV_INT_VERSION                 TYPE        IF_EDOC_EXT_DATATYPES=>MTY_INT_VERSION(optional)
* | [--->] ID_SOURCE_DATA                 TYPE REF TO DATA
* | [!CX!] CX_EDOCUMENT_EXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD if_edocument_int_conn_ext~trigger.
    DATA: lv_interface_id  TYPE if_edoc_ext_datatypes=>mty_interface_id,
          lv_log_port_name TYPE prx_logical_port_name,
          lv_error_txt     TYPE string.

    DATA: ld_source_data TYPE REF TO data,
          lo_class_desc  TYPE REF TO cl_abap_typedescr,
          lv_class_name  TYPE string.

    DATA: lv_process_step TYPE if_edoc_ext_datatypes=>mty_edoc_process_step,
          lv_subrc        TYPE sysubrc,
          lx_root         TYPE REF TO cx_root.

    DATA: lv_comm_action  TYPE abap_bool.

    DATA: lv_method_name    TYPE if_edoc_ext_datatypes=>mty_method_name,
          lv_map_class_name TYPE if_edoc_ext_datatypes=>mty_class_name. "Check
    DATA lo_map_class TYPE REF TO object.
    DATA: ld_data      TYPE REF TO data,
          lr_edocument TYPE REF TO if_edoc_ext_datatypes=>mty_edocument_s,
          lv_variant   TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_var,
          lv_xstring   TYPE xstring.

    FIELD-SYMBOLS: <ls_req_source_structure> TYPE any,
                   <ls_req_target_structure> TYPE any.
    FIELD-SYMBOLS: <ls_res_source_structure> TYPE any,
                   <ls_res_target_structure> TYPE any.
    FIELD-SYMBOLS: <fs_status_req_source> TYPE any.

    DATA: ld_req_target TYPE REF TO data,
          ld_res_source TYPE REF TO data,
          ld_res_target TYPE REF TO data.

    ASSIGN id_source_data->* TO <ls_req_source_structure>.
    lr_edocument = io_edocument_ext_es->get_edocument_structure( ).

*    *-- Logical port and interface determination
    lv_interface_id = iv_interface_id.
    IF iv_interface_id = 'ZUS_INVOICE_SEND_REQ'.
      lv_process_step = 'SENDEDOC'.
*     Select the interface ID
      lv_interface_id = io_edocument_ext_es->determine_interface_id(
                          iv_process_step = lv_process_step ).


*determine mapping class
      lv_map_class_name = 'ZCL_EDOC_MAP_US'.

***determine mapping method using new badi
      lv_method_name = 'MAP_INVOICE_REQUEST_FI'.

*call mapping class -> in this partner will do mapping and will call proxy
*  ls_country_req_source = <ls_source_structure>.
      CREATE OBJECT lo_map_class TYPE (lv_map_class_name).
      CALL METHOD lo_map_class->(lv_method_name)
        EXPORTING
          is_source = <ls_req_source_structure>
        IMPORTING
          ed_target = ld_req_target.

      ASSIGN ld_req_target->* TO <ls_req_target_structure>.
**call Proxy -> new badi will be provided to call proxy
      CALL METHOD zcl_edoc_map_us=>call_proxy
        EXPORTING
          is_source       = <ls_req_target_structure>
          iv_logical_port = lv_log_port_name
        IMPORTING
          ed_response     = ld_res_source
          ev_xstring      = lv_xstring.

      GET REFERENCE OF lv_xstring INTO ld_data.
*communicate sendedoc process step
      io_edocument_ext_es->communicate_process_step(
                    EXPORTING  iv_process_step   = 'SENDEDOC'
                               iv_interface_guid = 'STMP' "(or use any other non-empty ID that you prefer)
                               id_data           = ld_data ).

***determine mapping method using new badi
      lv_method_name = 'MAP_INVOICE_RESPONSE_FI'.
*call mapping response
      ASSIGN ld_res_source->* TO <ls_res_source_structure>.
      CALL METHOD lo_map_class->(lv_method_name)
        EXPORTING
          is_source = <ls_res_source_structure>
        IMPORTING
          ed_target = ld_res_target.

*determine process step using new badi provided
      lv_process_step = 'PROC_RESP'.

*call communicate process step
      io_edocument_ext_es->communicate_process_step(
                 EXPORTING  iv_process_step   = lv_process_step  "Process response
                            iv_interface_guid = 'STMP'
                            id_data           = ld_res_target
                            iv_variant        = lv_variant   "Result should be passed as variant
                            ).

    ELSEIF iv_interface_id = 'ZUS_INVOICE_GET_REQ'.

*determine mapping class
      lv_map_class_name = 'ZCL_EDOC_MAP_US'.
***determine mapping method using new badi
      lv_method_name = 'MAP_STATUS_REQUEST'.
      ASSIGN lr_edocument->* TO <fs_status_req_source>.
      CREATE OBJECT lo_map_class TYPE (lv_map_class_name).
      CALL METHOD lo_map_class->(lv_method_name)
        EXPORTING
          is_source = <fs_status_req_source>
        IMPORTING
          ed_target = ld_req_target.

      ASSIGN ld_req_target->* TO <ls_req_target_structure>.
**call Proxy -> new badi will be provided to call proxy
      CALL METHOD zcl_edoc_map_us=>call_status_proxy
        EXPORTING
          is_source       = <ls_req_target_structure>
          iv_logical_port = lv_log_port_name
        IMPORTING
          ed_response     = ld_res_source
          ev_xstring      = lv_xstring.

      GET REFERENCE OF lv_xstring INTO ld_data.
***determine mapping method using new badi
      lv_method_name = 'MAP_STATUS_RESPONSE'.
*call mapping response
      ASSIGN ld_res_source->* TO <ls_res_source_structure>.
      CALL METHOD lo_map_class->(lv_method_name)
        EXPORTING
          is_source = <ls_res_source_structure>
        IMPORTING
          ed_target = ld_res_target.

*determine process step using new badi provided
      lv_process_step = 'STATUS_RES'.

*call communicate process step
      io_edocument_ext_es->communicate_process_step(
                 EXPORTING  iv_process_step   = lv_process_step  "Process response
                            iv_interface_guid = 'STMP'
                            id_data           = ld_res_target
                            iv_variant        = lv_variant   "Result should be passed as variant
                            ).

    ELSEIF iv_interface_id = 'ZUS_INVOICE_REJECT_REQ'.
      DATA ld_target TYPE REF TO zif_edo_datatypes_us=>mty_mapping_target_us_s.
      FIELD-SYMBOLS <ls_target> TYPE zif_edo_datatypes_us=>mty_mapping_target_us_s.

      CREATE DATA ld_target.
      ASSIGN ld_target->* TO <ls_target>.

      <ls_target>-status = 'REJECT'.
*determine process step using new badi provided
      lv_process_step = 'REJECT_RES'.
*call communicate process step
      io_edocument_ext_es->communicate_process_step(
                 EXPORTING  iv_process_step   = lv_process_step  "Process response
                            iv_interface_guid = 'STMP'
                            id_data           = ld_target
                            iv_variant        = lv_variant   "Result should be passed as variant
                            ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
