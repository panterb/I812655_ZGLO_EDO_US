CLASS zcl_edocument_base_ext_us DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_edocument_base_ext .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_edocument_base_ext_us IMPLEMENTATION.


  METHOD if_edocument_base_ext~determine_edocument_class.
    CV_EDOC_CLASS_NAME = 'ZCL_EDODUMENT_US'.
  ENDMETHOD.


  METHOD if_edocument_base_ext~determine_process.
    IF is_edocument-source_type = 'SRC_FILE'.
      cv_process_name = 'ZUSINVIN'.
      cv_process_version = '0001'.
    ELSE.
      cv_process_name = 'ZUSINV'.
      cv_process_version = '0001'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
