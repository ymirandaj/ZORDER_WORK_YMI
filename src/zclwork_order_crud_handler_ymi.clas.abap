"! <p class="shorttext synchronized" lang="en">Clase zclwork_order_crud_handler_ymi </p>
"! Clase que implementa las validaciones de la clase zcl_work_order_validator_ymi para el
"! CRUD  de las órdenes de trabajo.
CLASS zclwork_order_crud_handler_ymi DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_work_order_create,
             customer_id   TYPE ztcustomer_ymi-customer_id,
             technician_id TYPE zttechnician_ymi-technician_id,
             priority      TYPE zde_priority_code_ymi,
             description   TYPE ztworkorder_ymi-description,
           END OF ty_work_order_create,
           BEGIN OF ty_work_order_update,
             work_order_id TYPE ztworkorder_ymi-work_order_id,
             customer_id   TYPE ztcustomer_ymi-customer_id,
             technician_id TYPE zttechnician_ymi-technician_id,
             priority      TYPE zde_priority_code_ymi,
             status        TYPE zde_status_code_ymi,
             description   TYPE ztworkorder_ymi-description,
           END OF ty_work_order_update,
           gt_work_orders TYPE SORTED TABLE OF zdcs_work_orders_ymi WITH NON-UNIQUE KEY id.



    METHODS:
      constructor,
      "! <p class="shorttext synchronized" lang="en">Crear orden de trabajo</p>
      "! Este método agrega la orden de trabajo siempre que sea válida
      "! @parameter iv_work_order |  Parámetro que contiene los detalles de la orden de trabajo(
      "! Cliente, técnico, prioridad, descripción
      "!  ).
      create_work_order
        IMPORTING iv_work_order    TYPE  ty_work_order_create
        RETURNING VALUE(rv_result) TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Actualizar orden de trabajo</p>
      "! Este método actualiza la orden de trabajo siempre que sea válida
      "! @parameter iv_work_order |  Parámetro que es una estructura de la tabla de la orden de trabajo
      update_work_order
        IMPORTING iv_work_order    TYPE  ty_work_order_update
        RETURNING VALUE(rv_result) TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Eliminar orden de trabajo</p>
      "! Este método agrega la orden de trabajo siempre que sea válida
      "! @parameter iv_work_order_id | Parámetro que indica el id de la orden de trabajo
      delete_work_order
        IMPORTING iv_work_order_id TYPE  ztworkorder_ymi-work_order_id
        RETURNING VALUE(rv_result) TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">Obtiene todas las ordenes de trabajo</p>
      "! Permite obtener todas las ordenes de trabajo
      "!  "! @parameter iv_page_number | Indíca el número de página a consultar y no puede ser negativo
      "! @parameter iv_records_per_page | Indíca la cantidad de registros a mostrar en un rango de 1 - 500
      read_work_orders
        IMPORTING iv_page_number      TYPE i
                  iv_records_per_page TYPE i
        RETURNING VALUE(rv_result)    TYPE gt_work_orders,
      "! <p class="shorttext synchronized" lang="en">Consultar orden de trabajo por ID</p>
      "! Este método obtiene la orden de trabajo filtrado por su id
      "! @parameter iv_work_order_id | Parámetro que indica el id de la orden de trabajo
      read_work_order_by_id
        IMPORTING iv_work_order_id TYPE  ztworkorder_ymi-work_order_id
        RETURNING VALUE(rv_result) TYPE zdcs_work_orders_ymi.


  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA: co_valid_order        TYPE  REF TO  zcl_work_order_validator_ymi,
          co_work_order_history TYPE REF TO zcl_history_work_ord_ymi.
ENDCLASS.



CLASS zclwork_order_crud_handler_ymi IMPLEMENTATION.
  METHOD create_work_order.
    DATA lv_work_order_id TYPE n LENGTH 10.

    SELECT FROM  ztworkorder_ymi
    FIELDS MAX( work_order_id )
    INTO @lv_work_order_id.
    lv_work_order_id = lv_work_order_id + 1.



    DATA(lv_valid) =  me->co_valid_order->validate_create_order( iv_customer_id = |{ iv_work_order-customer_id }|
                                                 iv_priority = |{ iv_work_order-priority }|
                                                 iv_technician_id = |{ iv_work_order-technician_id }| ).

    IF lv_valid IS INITIAL.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    DATA(ls_work_order) = CORRESPONDING ztworkorder_ymi( iv_work_order  ).

    ls_work_order-work_order_id = lv_work_order_id.
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
    DATA(lv_valid) =  me->co_valid_order->validate_delete_order( iv_work_order_id = |{ iv_work_order_id }| ).

    IF lv_valid = abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    DELETE FROM ztworkorder_ymi
    WHERE work_order_id = @iv_work_order_id.

    IF sy-subrc = 0.
      rv_result = abap_true.
    ELSE.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD read_work_orders.

    DATA lv_offset TYPE int8.


    IF iv_page_number <= 0.
      RAISE EXCEPTION NEW zcx_validator_error( text = |El número de la página no debe ser menor o igual a 0.| ).
    ENDIF.
    IF iv_records_per_page <= 0 OR iv_records_per_page > 500.
      RAISE EXCEPTION NEW zcx_validator_error( text = |Las cantidad de registros debe ser entre 1 a 500.| ).
    ENDIF.

    lv_offset = ( iv_page_number - 1 ) * iv_records_per_page.

    SELECT FROM zdcs_work_orders_ymi
    FIELDS Id, CustomerId, Customer, TechnicianId, Technician, CreationDate, StatusCode, Status,
           PriorityCode, Priority, OrderDescription
    ORDER BY id ASCENDING
    INTO TABLE @rv_result
    OFFSET @lv_offset
    UP TO @iv_records_per_page ROWS.

  ENDMETHOD.

  METHOD update_work_order.
    DATA(lv_valid) =  me->co_valid_order->validate_update_order( iv_work_order_id = |{ iv_work_order-work_order_id }| ).

    IF lv_valid = abap_false.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    UPDATE  ztworkorder_ymi
    SET customer_id = @iv_work_order-customer_id,
        description = @iv_work_order-description,
        status = @iv_work_order-status,
        priority = @iv_work_order-priority,
        technician_id = @iv_work_order-technician_id
    WHERE work_order_id = @iv_work_order-work_order_id.



    IF sy-subrc <> 0.
      rv_result = abap_false.
      RETURN.
    ENDIF.

    rv_result = me->co_work_order_history->save_work_order_hist(  iv_work_order-work_order_id ).
    IF lv_valid = abap_false.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    rv_result = abap_true.

    COMMIT WORK.


  ENDMETHOD.

  METHOD constructor.
    CREATE OBJECT me->co_valid_order.
    CREATE OBJECT me->co_work_order_history.

  ENDMETHOD.

  METHOD read_work_order_by_id.

    SELECT SINGLE FROM zdcs_work_orders_ymi
     FIELDS Id, CustomerId, Customer, TechnicianId, Technician, CreationDate, StatusCode, Status,
            PriorityCode, Priority, OrderDescription
     WHERE Id = @iv_work_order_id
     INTO @rv_result.


  ENDMETHOD.

ENDCLASS.
