CLASS zcl_edodument_us DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA: mv_interface_id TYPE if_edoc_ext_datatypes=>mty_interface_id.
    METHODS constructor .
    METHODS process_create
      IMPORTING
        !iv_interface_guid   TYPE if_edoc_ext_datatypes=>mty_interface_edoc_guid OPTIONAL
        !iv_process_step     TYPE if_edoc_ext_datatypes=>mty_edoc_process_step OPTIONAL
        !iv_variant          TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_var OPTIONAL
        !io_edocument_ext_es TYPE REF TO cl_edocument_ext_es OPTIONAL
      RETURNING
        VALUE(rv_result)     TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_res .
    METHODS process_request_send
      IMPORTING
        !iv_interface_guid   TYPE if_edoc_ext_datatypes=>mty_interface_edoc_guid OPTIONAL
        !iv_process_step     TYPE if_edoc_ext_datatypes=>mty_edoc_process_step OPTIONAL
        !iv_variant          TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_var OPTIONAL
        !io_edocument_ext_es TYPE REF TO cl_edocument_ext_es OPTIONAL
      RETURNING
        VALUE(rv_result)     TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_res .
    METHODS process_sendedoc
      IMPORTING
        !iv_interface_guid   TYPE if_edoc_ext_datatypes=>mty_interface_edoc_guid OPTIONAL
        !iv_process_step     TYPE if_edoc_ext_datatypes=>mty_edoc_process_step OPTIONAL
        !iv_variant          TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_var OPTIONAL
        !io_edocument_ext_es TYPE REF TO cl_edocument_ext_es OPTIONAL
        !is_data             TYPE xstring OPTIONAL
      RETURNING
        VALUE(rv_result)     TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_res .
    METHODS process_response
      IMPORTING
        !iv_interface_guid   TYPE if_edoc_ext_datatypes=>mty_interface_edoc_guid OPTIONAL
        !iv_process_step     TYPE if_edoc_ext_datatypes=>mty_edoc_process_step OPTIONAL
        !iv_variant          TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_var OPTIONAL
        !io_edocument_ext_es TYPE REF TO cl_edocument_ext_es OPTIONAL
        !is_data             TYPE zif_edo_datatypes_us=>mty_mapping_target_us_s
      RETURNING
        VALUE(rv_result)     TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_res
      RAISING
        cx_edocument_ext .
    METHODS process_trigger_send
      IMPORTING
        !iv_interface_guid   TYPE if_edoc_ext_datatypes=>mty_interface_edoc_guid OPTIONAL
        !iv_process_step     TYPE if_edoc_ext_datatypes=>mty_edoc_process_step OPTIONAL
        !iv_variant          TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_var OPTIONAL
        !io_edocument_ext_es TYPE REF TO cl_edocument_ext_es OPTIONAL
      RETURNING
        VALUE(rv_result)     TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_res .
    METHODS update_file_table
      IMPORTING
        !iv_file                TYPE xstring
        !iv_file_guid           TYPE if_edoc_cloud_datatypes=>mty_edoc_guid
        !iv_file_type           TYPE if_edoc_ext_datatypes=>mty_edoc_file_type
        !iv_file_name_extension TYPE if_edoc_ext_datatypes=>mty_edoc_file_name_ext
        !io_edocument_ext_es    TYPE REF TO cl_edocument_ext_es OPTIONAL .

    METHODS process_status_req
      IMPORTING
        !iv_interface_guid   TYPE if_edoc_ext_datatypes=>mty_interface_edoc_guid OPTIONAL
        !iv_process_step     TYPE if_edoc_ext_datatypes=>mty_edoc_process_step OPTIONAL
        !iv_variant          TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_var OPTIONAL
        !io_edocument_ext_es TYPE REF TO cl_edocument_ext_es OPTIONAL
      RETURNING
        VALUE(rv_result)     TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_res .
    METHODS process_status_res
      IMPORTING
        !iv_interface_guid   TYPE if_edoc_ext_datatypes=>mty_interface_edoc_guid OPTIONAL
        !iv_process_step     TYPE if_edoc_ext_datatypes=>mty_edoc_process_step OPTIONAL
        !iv_variant          TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_var OPTIONAL
        !io_edocument_ext_es TYPE REF TO cl_edocument_ext_es OPTIONAL
        !is_data             TYPE zif_edo_datatypes_us=>mty_mapping_target_us_s
      RETURNING
        VALUE(rv_result)     TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_res
      RAISING
        cx_edocument_ext .
    METHODS process_cancel
      IMPORTING
        !iv_interface_guid   TYPE if_edoc_ext_datatypes=>mty_interface_edoc_guid OPTIONAL
        !iv_process_step     TYPE if_edoc_ext_datatypes=>mty_edoc_process_step OPTIONAL
        !iv_variant          TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_var OPTIONAL
        !io_edocument_ext_es TYPE REF TO cl_edocument_ext_es OPTIONAL
      RETURNING
        VALUE(rv_result)     TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_res .
    METHODS process_sendtocust
      IMPORTING
        !iv_interface_guid   TYPE if_edoc_ext_datatypes=>mty_interface_edoc_guid OPTIONAL
        !iv_process_step     TYPE if_edoc_ext_datatypes=>mty_edoc_process_step OPTIONAL
        !iv_variant          TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_var OPTIONAL
        !io_edocument_ext_es TYPE REF TO cl_edocument_ext_es OPTIONAL
      RETURNING
        VALUE(rv_result)     TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_res .
    METHODS process_restart
      IMPORTING
        !iv_variant      TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_var OPTIONAL
      RETURNING
        VALUE(rv_result) TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_res .

    METHODS process_reject_req
      IMPORTING
        !iv_interface_guid   TYPE if_edoc_ext_datatypes=>mty_interface_edoc_guid OPTIONAL
        !iv_process_step     TYPE if_edoc_ext_datatypes=>mty_edoc_process_step OPTIONAL
        !iv_variant          TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_var OPTIONAL
        !io_edocument_ext_es TYPE REF TO cl_edocument_ext_es OPTIONAL
      RETURNING
        VALUE(rv_result)     TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_res .
    METHODS process_reject_res
      IMPORTING
        !iv_interface_guid   TYPE if_edoc_ext_datatypes=>mty_interface_edoc_guid OPTIONAL
        !iv_process_step     TYPE if_edoc_ext_datatypes=>mty_edoc_process_step OPTIONAL
        !iv_variant          TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_var OPTIONAL
        !io_edocument_ext_es TYPE REF TO cl_edocument_ext_es OPTIONAL
        !is_data             TYPE zif_edo_datatypes_us=>mty_mapping_target_us_s
      RETURNING
        VALUE(rv_result)     TYPE if_edoc_ext_datatypes=>mty_edoc_proc_step_res
      RAISING
        cx_edocument_ext .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_edodument_us IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDODUMENT_US->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.

*    ms_edogrinv-edoc_guid = ms_edocument-edoc_guid.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDODUMENT_US->PROCESS_CREATE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INTERFACE_GUID              TYPE        IF_EDOC_EXT_DATATYPES=>MTY_INTERFACE_EDOC_GUID(optional)
* | [--->] IV_PROCESS_STEP                TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROCESS_STEP(optional)
* | [--->] IV_VARIANT                     TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_VAR(optional)
* | [--->] IO_EDOCUMENT_EXT_ES            TYPE REF TO CL_EDOCUMENT_EXT_ES(optional)
* | [<-()] RV_RESULT                      TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_RES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD process_create.

    DATA  ls_edous TYPE zedous.
    DATA  ls_edocument TYPE REF TO if_edoc_ext_datatypes=>mty_edocument_s.
    FIELD-SYMBOLS <ls_edocument_fs> TYPE if_edoc_ext_datatypes=>mty_edocument_s.

    IF io_edocument_ext_es IS BOUND.
      TRY.
          io_edocument_ext_es->process_create_global(
            EXPORTING
              iv_interface_guid = iv_interface_guid                 " Interface Message ID
              iv_process_step   = iv_process_step                 " eDocument Process Step
              iv_variant        = iv_variant                 " eDocument Process Step Variant
            RECEIVING
              rv_result         = rv_result                 " eDocument Process Step Result
          ).
          io_edocument_ext_es->get_edocument_structure( RECEIVING rr_edocument = ls_edocument ).
        CATCH cx_edocument_ext.
          "handle exception
      ENDTRY.

    ENDIF.

    ASSIGN ls_edocument->* TO <ls_edocument_fs>.
    ls_edous-edoc_guid = <ls_edocument_fs>-edoc_guid.
    INSERT zedous FROM @ls_edous.

*
*ls_edous-edoc_guid = '42010AEE298E1EDCBE87D8237701CF81'.
*INSERT ZEDOUS FROM @ls_edous.
*ls_edous-edoc_guid = '42010AEE298E1EDCBE883DD962A272EA'.
*INSERT ZEDOUS FROM @ls_edous.
*ls_edous-edoc_guid = '42010AEE298E1EDCBE885045EB2C73AD'.
*INSERT ZEDOUS FROM @ls_edous.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDODUMENT_US->PROCESS_REQUEST_SEND
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INTERFACE_GUID              TYPE        IF_EDOC_EXT_DATATYPES=>MTY_INTERFACE_EDOC_GUID(optional)
* | [--->] IV_PROCESS_STEP                TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROCESS_STEP(optional)
* | [--->] IV_VARIANT                     TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_VAR(optional)
* | [--->] IO_EDOCUMENT_EXT_ES            TYPE REF TO CL_EDOCUMENT_EXT_ES(optional)
* | [<-()] RV_RESULT                      TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_RES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD process_request_send.
    IF io_edocument_ext_es IS BOUND.
      TRY.
          io_edocument_ext_es->process_req_send_global(
            EXPORTING
              iv_interface_guid =     iv_interface_guid              " Interface Message ID
              iv_process_step   =     iv_process_step             " eDocument Process Step
              iv_variant        =     iv_variant             " eDocument Process Step Variant
            RECEIVING
              rv_result         =     rv_result            " eDocument Process Step Result
          ).
        CATCH cx_edocument_ext.
          "handle exception
      ENDTRY.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDODUMENT_US->PROCESS_RESPONSE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INTERFACE_GUID              TYPE        IF_EDOC_EXT_DATATYPES=>MTY_INTERFACE_EDOC_GUID(optional)
* | [--->] IV_PROCESS_STEP                TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROCESS_STEP(optional)
* | [--->] IV_VARIANT                     TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_VAR(optional)
* | [--->] IO_EDOCUMENT_EXT_ES            TYPE REF TO CL_EDOCUMENT_EXT_ES(optional)
* | [--->] IS_DATA                        TYPE        ZIF_EDO_DATATYPES_US=>MTY_MAPPING_TARGET_US_S
* | [<-()] RV_RESULT                      TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_RES
* | [!CX!] CX_EDOCUMENT_EXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD process_response.
* TRACKID
    DATA lv_file_guid TYPE if_edoc_cloud_datatypes=>mty_edoc_guid.
    DATA ls_edous TYPE zedous.
    IF is_data-status = 'Success'.
      rv_result = 'TRACKID'.
    ELSE.
      rv_result = 'ERROR'.

      " Exception testing
      DATA lv_error_txt TYPE string.
      lv_error_txt = 'Error while submitting the document'.
    ENDIF.

    IF is_data-response_xml IS NOT INITIAL.
*       Get the system UUID
      TRY.
          lv_file_guid  = cl_system_uuid=>create_uuid_c32_static( ).
        CATCH cx_uuid_error.
          "handle exception
      ENDTRY.

      update_file_table( io_edocument_ext_es = io_edocument_ext_es
                         iv_file = is_data-response_xml
                         iv_file_guid = lv_file_guid
                         iv_file_type = 'RESPONSE'
                         iv_file_name_extension = 'XML' ).

    ENDIF.


    ls_edous-edoc_guid = io_edocument_ext_es->get_edocument_structure( )->edoc_guid.
    ls_edous-trackid = is_data-track_id.

    MODIFY zedous FROM @ls_edous.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDODUMENT_US->PROCESS_SENDEDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INTERFACE_GUID              TYPE        IF_EDOC_EXT_DATATYPES=>MTY_INTERFACE_EDOC_GUID(optional)
* | [--->] IV_PROCESS_STEP                TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROCESS_STEP(optional)
* | [--->] IV_VARIANT                     TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_VAR(optional)
* | [--->] IO_EDOCUMENT_EXT_ES            TYPE REF TO CL_EDOCUMENT_EXT_ES(optional)
* | [--->] IS_DATA                        TYPE        XSTRING(optional)
* | [<-()] RV_RESULT                      TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_RES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD process_sendedoc.
    DATA: lv_file_guid  TYPE if_edoc_cloud_datatypes=>mty_edoc_guid.
    CLEAR rv_result.

    rv_result = iv_variant.
    IF is_data IS NOT INITIAL.
*       Get the system UUID
      lv_file_guid  = cl_system_uuid=>create_uuid_c32_static( ).

      update_file_table( io_edocument_ext_es = io_edocument_ext_es
                         iv_file = is_data
                         iv_file_guid = lv_file_guid
                         iv_file_type = 'REQUEST'
                         iv_file_name_extension = 'XML' ).


    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDODUMENT_US->PROCESS_TRIGGER_SEND
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INTERFACE_GUID              TYPE        IF_EDOC_EXT_DATATYPES=>MTY_INTERFACE_EDOC_GUID(optional)
* | [--->] IV_PROCESS_STEP                TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROCESS_STEP(optional)
* | [--->] IV_VARIANT                     TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_VAR(optional)
* | [--->] IO_EDOCUMENT_EXT_ES            TYPE REF TO CL_EDOCUMENT_EXT_ES(optional)
* | [<-()] RV_RESULT                      TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_RES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD process_trigger_send.

    IF io_edocument_ext_es IS BOUND.
      TRY.
          io_edocument_ext_es->process_trigger_send_global(
            EXPORTING
              iv_interface_guid = iv_interface_guid                 " Interface Message ID
              iv_process_step   = iv_process_step                 " eDocument Process Step
              iv_variant        = iv_variant                 " eDocument Process Step Variant
            RECEIVING
              rv_result         = rv_result                 " eDocument Process Step Result
          ).
        CATCH cx_edocument_ext.
          "handle exception
      ENDTRY.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDODUMENT_US->UPDATE_FILE_TABLE
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_FILE                        TYPE        XSTRING
* | [--->] IV_FILE_GUID                   TYPE        IF_EDOC_CLOUD_DATATYPES=>MTY_EDOC_GUID
* | [--->] IV_FILE_TYPE                   TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_FILE_TYPE
* | [--->] IV_FILE_NAME_EXTENSION         TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_FILE_NAME_EXT
* | [--->] IO_EDOCUMENT_EXT_ES            TYPE REF TO CL_EDOCUMENT_EXT_ES(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD update_file_table.
    DATA: ls_edocumentfile TYPE if_edoc_ext_datatypes=>mty_edocumentfile_s,
          lr_edocument     TYPE REF TO if_edoc_ext_datatypes=>mty_edocument_s.

*   Move file parameters
    CLEAR ls_edocumentfile.
    ls_edocumentfile-file_guid   = iv_file_guid.
    ls_edocumentfile-create_date = sy-datum.
    ls_edocumentfile-create_time = sy-uzeit.
    ls_edocumentfile-file_type   = iv_file_type.

    lr_edocument = io_edocument_ext_es->get_edocument_structure( ).

*   File name and file content
    CONCATENATE
                lr_edocument->land
                '_'
                lr_edocument->source_key
                '_'
                ls_edocumentfile-create_date
                '_'
                ls_edocumentfile-create_time
                '.'
                iv_file_name_extension
           INTO ls_edocumentfile-file_name.

    ls_edocumentfile-file_raw = iv_file.

    IF io_edocument_ext_es IS BOUND.
      io_edocument_ext_es->add_file_to_edocumentfile( CHANGING cv_edocumentfile = ls_edocumentfile ).
    ENDIF.
**    io_edocument_stmp->ms_edocument-file_guid = ls_edocumentfile-file_guid.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDODUMENT_US->PROCESS_STATUS_REQ
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INTERFACE_GUID              TYPE        IF_EDOC_EXT_DATATYPES=>MTY_INTERFACE_EDOC_GUID(optional)
* | [--->] IV_PROCESS_STEP                TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROCESS_STEP(optional)
* | [--->] IV_VARIANT                     TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_VAR(optional)
* | [--->] IO_EDOCUMENT_EXT_ES            TYPE REF TO CL_EDOCUMENT_EXT_ES(optional)
* | [<-()] RV_RESULT                      TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_RES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD process_status_req.
    IF io_edocument_ext_es IS BOUND.
      DATA: lv_interface_id TYPE if_edoc_ext_datatypes=>mty_interface_id.
      DATA lr_edocument     TYPE REF TO if_edoc_ext_datatypes=>mty_edocument_s.
      FIELD-SYMBOLS <fs_edocument> TYPE if_edoc_ext_datatypes=>mty_edocument_s.
      lr_edocument =  io_edocument_ext_es->get_edocument_structure(  ).
      ASSIGN lr_edocument->* TO <fs_edocument>.

      mv_interface_id = io_edocument_ext_es->determine_interface_id( iv_process_step ).
      <fs_edocument>-interface_id = mv_interface_id.
      TRY.
          io_edocument_ext_es->process_trigger_send_global(
            EXPORTING
              iv_interface_guid = iv_interface_guid                   " Interface Message ID
              iv_process_step   = iv_process_step                 " eDocument Process Step
              iv_variant        = iv_variant                 " eDocument Process Step Variant
            RECEIVING
              rv_result         = rv_result                 " eDocument Process Step Result
          ).
        CATCH cx_edocument_ext.
          "handle exception
      ENDTRY.
      CLEAR rv_result.
    ENDIF.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDODUMENT_US->PROCESS_STATUS_RES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INTERFACE_GUID              TYPE        IF_EDOC_EXT_DATATYPES=>MTY_INTERFACE_EDOC_GUID(optional)
* | [--->] IV_PROCESS_STEP                TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROCESS_STEP(optional)
* | [--->] IV_VARIANT                     TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_VAR(optional)
* | [--->] IO_EDOCUMENT_EXT_ES            TYPE REF TO CL_EDOCUMENT_EXT_ES(optional)
* | [--->] IS_DATA                        TYPE        ZIF_EDO_DATATYPES_US=>MTY_MAPPING_TARGET_US_S
* | [<-()] RV_RESULT                      TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_RES
* | [!CX!] CX_EDOCUMENT_EXT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD process_status_res.
    DATA lv_file_guid TYPE if_edoc_cloud_datatypes=>mty_edoc_guid.
    IF is_data-status = 'ACCEPT'.
      rv_result = 'ACCEPT'.
    ELSE.
      rv_result = 'REJECT'.

      " Exception testing
      DATA lv_error_txt TYPE string.
      lv_error_txt = 'Error while submitting the document'.
    ENDIF.

    IF is_data-response_xml IS NOT INITIAL.
*       Get the system UUID
      lv_file_guid  = cl_system_uuid=>create_uuid_c32_static( ).

      update_file_table( io_edocument_ext_es = io_edocument_ext_es
                         iv_file = is_data-response_xml
                         iv_file_guid = lv_file_guid
                         iv_file_type = 'RESPONSE'
                         iv_file_name_extension = 'XML' ).

    ENDIF.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDODUMENT_US->PROCESS_CANCEL
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INTERFACE_GUID              TYPE        IF_EDOC_EXT_DATATYPES=>MTY_INTERFACE_EDOC_GUID(optional)
* | [--->] IV_PROCESS_STEP                TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROCESS_STEP(optional)
* | [--->] IV_VARIANT                     TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_VAR(optional)
* | [--->] IO_EDOCUMENT_EXT_ES            TYPE REF TO CL_EDOCUMENT_EXT_ES(optional)
* | [<-()] RV_RESULT                      TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_RES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD process_cancel.
    rv_result = ''.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDODUMENT_US->PROCESS_SENDTOCUST
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_INTERFACE_GUID              TYPE        IF_EDOC_EXT_DATATYPES=>MTY_INTERFACE_EDOC_GUID(optional)
* | [--->] IV_PROCESS_STEP                TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROCESS_STEP(optional)
* | [--->] IV_VARIANT                     TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_VAR(optional)
* | [--->] IO_EDOCUMENT_EXT_ES            TYPE REF TO CL_EDOCUMENT_EXT_ES(optional)
* | [<-()] RV_RESULT                      TYPE        IF_EDOC_EXT_DATATYPES=>MTY_EDOC_PROC_STEP_RES
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD process_sendtocust.
    TRY.
        io_edocument_ext_es->send_email_to_cust( EXPORTING iv_generic_badi_filter = 'EXTUS' ).
      CATCH cx_edocument_ext.
        "handle exception
    ENDTRY.
    rv_result = ''.
  ENDMETHOD.

* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_EDOCUMENT_EXT_SK->PROCESS_RESTART
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_VARIANT                     TYPE        EDOC_PROC_STEP_VARIANT(optional)
* | [<-()] RV_RESULT                      TYPE        EDOC_PROC_STEP_RESULT
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD process_restart.

    CLEAR rv_result.
    rv_result = iv_variant.

  ENDMETHOD.

  METHOD process_reject_req.
    IF io_edocument_ext_es IS BOUND.
      DATA: lv_interface_id TYPE if_edoc_ext_datatypes=>mty_interface_id.
      DATA lr_edocument     TYPE REF TO if_edoc_ext_datatypes=>mty_edocument_s.
      FIELD-SYMBOLS <fs_edocument> TYPE if_edoc_ext_datatypes=>mty_edocument_s.
      lr_edocument =  io_edocument_ext_es->get_edocument_structure(  ).
      ASSIGN lr_edocument->* TO <fs_edocument>.

      mv_interface_id = io_edocument_ext_es->determine_interface_id( iv_process_step ).
      <fs_edocument>-interface_id = mv_interface_id.
      TRY.
          io_edocument_ext_es->process_trigger_send_global(
            EXPORTING
              iv_interface_guid = iv_interface_guid                   " Interface Message ID
              iv_process_step   = iv_process_step                 " eDocument Process Step
              iv_variant        = iv_variant                 " eDocument Process Step Variant
            RECEIVING
              rv_result         = rv_result                 " eDocument Process Step Result
          ).
        CATCH cx_edocument_ext.
          "handle exception
      ENDTRY.
      CLEAR rv_result.
    ENDIF.
  ENDMETHOD.

  METHOD process_reject_res.
  DATA lv_file_guid TYPE if_edoc_cloud_datatypes=>mty_edoc_guid.
    IF is_data-status = 'REJECT'.
      rv_result = 'REJECTED'.
    ELSE.
      rv_result = 'ERROR'.

      " Exception testing
      DATA lv_error_txt TYPE string.
      lv_error_txt = 'Error while rejecting the document'.
    ENDIF.

    IF is_data-response_xml IS NOT INITIAL.

*       Get the system UUID
      lv_file_guid  = cl_system_uuid=>create_uuid_c32_static( ).

      update_file_table( io_edocument_ext_es = io_edocument_ext_es
                         iv_file = is_data-response_xml
                         iv_file_guid = lv_file_guid
                         iv_file_type = 'RESPONSE'
                         iv_file_name_extension = 'XML' ).

    ENDIF.

  ENDMETHOD.

ENDCLASS.
