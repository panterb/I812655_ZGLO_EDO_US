class ZEDO_US_CO_E_DOC_USGET_STATUS definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !DESTINATION type ref to IF_PROXY_DESTINATION optional
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    preferred parameter LOGICAL_PORT_NAME
    raising
      CX_AI_SYSTEM_FAULT .
  methods GET_STATUS
    importing
      !INPUT type ZEDO_US_GET_STATUS_REQUEST
    exporting
      !OUTPUT type ZEDO_US_GET_STATUS_RESPONSE
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZEDO_US_CO_E_DOC_USGET_STATUS IMPLEMENTATION.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZEDO_US_CO_E_DOC_USGET_STATUS'
    logical_port_name   = logical_port_name
    destination         = destination
  ).

  endmethod.


  method GET_STATUS.

  data(lt_parmbind) = value abap_parmbind_tab(
    ( name = 'INPUT' kind = '0' value = ref #( INPUT ) )
    ( name = 'OUTPUT' kind = '1' value = ref #( OUTPUT ) )
  ).
  if_proxy_client~execute(
    exporting
      method_name = 'GET_STATUS'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.
ENDCLASS.
