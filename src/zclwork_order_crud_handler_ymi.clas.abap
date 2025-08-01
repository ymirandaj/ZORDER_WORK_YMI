CLASS zclwork_order_crud_handler_ymi DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_work_order,
             work_order_id TYPE ztworkorder_ymi-work_order_id,
             customer_id   TYPE ztcustomer_ymi-customer_id,
             technician_id TYPE zttechnician_ymi-technician_id,
             priority      TYPE zde_priority_code_ymi,
             description   TYPE ztworkorder_ymi-description,
             status        TYPE ztworkorder_ymi-status,
           END OF ty_work_order,
           gt_work_orders TYPE SORTED TABLE OF ztworkorder_ymi WITH NON-UNIQUE KEY work_order_id.



    METHODS:
      constructor,
      create_work_order
        IMPORTING iv_work_order    TYPE  ty_work_order
        RETURNING VALUE(rv_result) TYPE abap_bool,
      update_work_order
        IMPORTING iv_work_order    TYPE  ty_work_order
        RETURNING VALUE(rv_result) TYPE abap_bool,
      delete_work_order
        IMPORTING iv_work_order_id TYPE  ztworkorder_ymi-work_order_id
        RETURNING VALUE(rv_result) TYPE abap_bool,
      read_work_orders
*        IMPORTING iv_work_order_id TYPE  ztworkorder_ymi-work_order_id
        RETURNING VALUE(rv_result) TYPE gt_work_orders.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA co_valid_order TYPE  REF TO  zcl_work_order_validator_ymi.
ENDCLASS.



CLASS zclwork_order_crud_handler_ymi IMPLEMENTATION.
  METHOD create_work_order.


    DATA(lv_valid) =  me->co_valid_order->validate_create_order( iv_customer_id = |{ iv_work_order-customer_id }|
                                                 iv_priority = 'B'
                                                 iv_technician_id = |{ iv_work_order-technician_id }| ).

    IF lv_valid IS INITIAL.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    DATA(ls_work_order) = CORRESPONDING ztworkorder_ymi( iv_work_order  ).

    ls_work_order-status = 'PE'.
    ls_work_order-creation_date = cl_abap_context_info=>get_system_date( ).

    INSERT ztworkorder_ymi FROM @ls_work_order.

    IF sy-subrc <> 0.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    rv_result = abap_true.

  ENDMETHOD.

  METHOD delete_work_order.

  ENDMETHOD.

  METHOD read_work_orders.
    SELECT FROM ztworkorder_ymi
    FIELDS client, work_order_id, customer_id, technician_id, creation_date,
           status, priority, description
    INTO TABLE @rv_result.

  ENDMETHOD.

  METHOD update_work_order.
    DATA(lv_valid) =  me->co_valid_order->validate_update_order( iv_work_order_id = |{ iv_work_order-work_order_id }|
                                                                  iv_status = |{ iv_work_order-status }| ).

  ENDMETHOD.

  METHOD constructor.
    CREATE OBJECT me->co_valid_order.
  ENDMETHOD.

ENDCLASS.
