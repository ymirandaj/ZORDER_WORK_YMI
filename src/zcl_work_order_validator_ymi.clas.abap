CLASS zcl_work_order_validator_ymi DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Valida creación orden de trabajo</p>
      "! Para crear una orden de trabajo se valida que el cliente, técnico y prioridad sean los correctos.
      "! @parameter iv_customer_id | Párametro que contiene el id del cliente
      "! @parameter iv_technician_id | Parámetro que contiene el id del técnico
      "! @parameter iv_priority | Parámetro que contiene el la prioridad cuyo valor debe ser (A = ALTA, B = BAJA)
      validate_create_order IMPORTING iv_customer_id   TYPE string
                                      iv_technician_id TYPE string
                                      iv_priority      TYPE string
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Valida la actualización de orden de trabajo</p>
      "! Para actualizar la orden de trabajo se debe validar que la orden exista y que no esté en estado completado
      "! @parameter iv_work_order_id | Párametro que contiene el id de la orden de trabajo
      "! @parameter iv_status | Parámetro que contiene estado a consultar
      validate_update_order IMPORTING iv_work_order_id TYPE string
                                      iv_status        TYPE string
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Valida la eliminación de orden de trabajo</p>
      "! Para eliminar la orden de trabajo se debe validar que la orden exista y que no esté en estado completado
      "! @parameter iv_work_order_id | Párametro que contiene el id de la orden de trabajo
      "! @parameter iv_status | Parámetro que contiene estado a consultar
      validate_delete_order IMPORTING iv_work_order_id TYPE string
                                      iv_status        TYPE string
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Valida estado y prioridad</p>
      "! Validar si el estado y prioridad son válidos.
      "! @parameter iv_status | Párametro que contiene el id de la orden de trabajo
      "! @parameter iv_priority | Parámetro que contiene estado a consultar
      validate_status_and_priority IMPORTING iv_status       TYPE string
                                             iv_priority     TYPE string
                                   RETURNING VALUE(rv_valid) TYPE abap_bool..
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS: c_valid_status   TYPE string VALUE 'PE CO', " Example statuses: Pending, Completed
               c_valid_priority TYPE string VALUE 'A B'. " Example priorities: High, Low




    METHODS:
      "! <p class="shorttext synchronized" lang="en">Valida si existe el cliente</p>
      "! Este método valida si el id enviado existe en la tabla de clientes
      "! @parameter iv_customer_id | Parámetro que contiene el id del cliente.
      check_customer_exists IMPORTING iv_customer_id   TYPE string
                            RETURNING VALUE(rv_exists) TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Valida si existe el técnico</p>
      "! Este método valida si el id enviado existe en la tabla de los técnicos
      "! @parameter iv_technician_id | Parámetro que contiene el id del técnico.
      check_technician_exists IMPORTING iv_technician_id TYPE string
                              RETURNING VALUE(rv_exists) TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Valida si existe la orden</p>
      "! Este método valida si el id enviado existe en la tabla de ordenes de trabajo
      "! @parameter iv_work_order_id | Parámetro que contiene el id del cliente.
      check_order_exists IMPORTING iv_work_order_id TYPE string
                         RETURNING VALUE(rv_exists) TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Valida si existe la orden en la tabla histórica</p>
      "! Este método valida si el id enviado existe en la tabla histórica de las ordenes de trabajo
      "! @parameter iv_work_order_id | Parámetro que contiene el id del cliente.
      check_order_history IMPORTING iv_work_order_id TYPE string
                          RETURNING VALUE(rv_exists) TYPE abap_bool.



ENDCLASS.



CLASS zcl_work_order_validator_ymi IMPLEMENTATION.
  METHOD validate_create_order.


    rv_valid = check_customer_exists( iv_customer_id  ).
    IF rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if technician exists
    rv_valid = check_technician_exists( iv_technician_id ).
    IF rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if priority is valid

    FIND iv_priority IN c_valid_priority.

    IF sy-subrc <> 0.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.



  ENDMETHOD.

  METHOD validate_delete_order.
    " Check if the work order exists
    rv_valid = check_order_exists(  iv_work_order_id  ).
    IF rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order status is editable (e.g., Pending)
    IF iv_status <> 'PE'.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_status_and_priority.
    " Validate the status value
    FIND iv_status IN c_valid_status.

    IF sy-subrc <> 0.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Validate the priority value
    FIND iv_priority IN c_valid_priority.

    IF sy-subrc <> 0.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.

  METHOD validate_update_order.
    " Check if the work order exists
    rv_valid = check_order_exists(  iv_work_order_id  ).
    IF rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order status is editable (e.g., Pending)
    IF iv_status NE 'PE'.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.
  ENDMETHOD.





  METHOD check_customer_exists.

    SELECT SINGLE FROM ztcustomer_ymi
    FIELDS customer_id
    WHERE customer_id = @iv_customer_id
    INTO @DATA(ls_customer).

    IF sy-subrc = 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD check_order_exists.
    SELECT SINGLE FROM ztworkorder_ymi
       FIELDS work_order_id
       WHERE work_order_id = @iv_work_order_id
       INTO @DATA(ls_order).

    IF sy-subrc = 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD check_order_history.
    SELECT SINGLE FROM ztworkorder_hist
       FIELDS history_id
       WHERE work_order_id = @iv_work_order_id
       INTO @DATA(ls_customer).

    IF sy-subrc = 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD check_technician_exists.
    SELECT SINGLE FROM zttechnician_ymi
       FIELDS technician_id
       WHERE technician_id = @iv_technician_id
       INTO @DATA(ls_customer).

    IF sy-subrc = 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
