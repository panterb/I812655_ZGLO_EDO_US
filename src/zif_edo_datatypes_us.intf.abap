INTERFACE zif_edo_datatypes_us
  PUBLIC .
 TYPES:
    BEGIN OF mty_mapping_target_us_s.
    include  type if_edoc_ext_datatypes=>mty_mapping_target_s.
    types: track_id TYPE string.


    types: END OF mty_mapping_target_us_s.
ENDINTERFACE.
