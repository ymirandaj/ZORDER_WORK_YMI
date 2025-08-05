"! <p class="shorttext synchronized" lang="en">Clase zcl_wok_order_crud_test_ymi </p>
"! Clase de test para simular la implementación de la clase zclwork_order_crud_handler_ymi
"!  usada para el ingreso, actualización, eliminación y consulta de las órdenes de trabajo
CLASS zcl_wok_order_crud_test_ymi DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS     constructor.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      test_create_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
      test_read_work_orders IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
      test_read_work_order_by_id IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
      test_update_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
      test_delete_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
      test_authorization IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
      is_valid_auth_check IMPORTING iv_work_order_id TYPE zde_work_order_id_ymi
                                    iv_activity      TYPE n
                          RETURNING VALUE(rv_result) TYPE abap_bool,

      load_data_test.

    DATA go_work_order TYPE REF TO  zclwork_order_crud_handler_ymi.
ENDCLASS.



CLASS zcl_wok_order_crud_test_ymi IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    me->load_data_test( ).
    me->test_create_work_order( out ).
    me->test_update_work_order( out ).
    me->test_delete_work_order( out ).
    me->test_read_work_orders( out ).
    me->test_read_work_order_by_id( out ).
    me->test_authorization( out ).

  ENDMETHOD.
  METHOD constructor.
    CREATE OBJECT me->go_work_order.
  ENDMETHOD.

  METHOD test_create_work_order.

    DATA ls_work_order_create TYPE go_work_order->ty_work_order_create.
    ls_work_order_create-customer_id = '1'.
    ls_work_order_create-technician_id = '1'.
    ls_work_order_create-description = 'Creación de de la orden'.
    ls_work_order_create-priority = 'B'.

    TRY.

        DATA(lv_result) = go_work_order->create_work_order( iv_work_order = ls_work_order_create ).

        IF lv_result = abap_false.
          io_out->write( |No se pudo crear la orden de trabajo| ).
        ELSE.
          io_out->write( |Orden de trabajo guardada con éxito| ).
        ENDIF.

      CATCH zcx_validator_error  INTO DATA(lx_error).
        io_out->write( lx_error->get_text( ) ).
      CATCH cx_root INTO DATA(lx_root).
        io_out->write( lx_root->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD test_delete_work_order.
    DATA lv_work_order_id TYPE n LENGTH 10 VALUE '3'.
    TRY.

        DATA(lv_result) =  go_work_order->delete_work_order( iv_work_order_id = lv_work_order_id ).

        IF lv_result = abap_false.
          io_out->write( 'Error al actualizar' ).
        ELSE.
          io_out->write( 'Error al actualizar' ).
        ENDIF.
      CATCH zcx_validator_error  INTO DATA(lx_error).
        io_out->write( lx_error->get_text( ) ).
      CATCH cx_root INTO DATA(lx_root).
        io_out->write( lx_root->get_text( ) ).

    ENDTRY.


  ENDMETHOD.

  METHOD test_read_work_orders.
    DATA(lt_work_orders) = go_work_order->read_work_orders( iv_page_number = 1 iv_records_per_page = 20 ).

    IF lt_work_orders IS INITIAL.
      io_out->write( data = |no hay registros de orden de trabajo| name = 'Listado de órdenes de trabajo' ).
    ELSE.
      io_out->write( data = lt_work_orders name = 'Listado de órdenes de trabajo' ).
    ENDIF.
  ENDMETHOD.

  METHOD test_read_work_order_by_id.
    "SE EXTRAE LOS DATOS PARA LA MODIFICACIÓN DE LA ORDEN DE TRABAJO
    DATA lv_work_order_id TYPE n LENGTH  10 VALUE '5' .
    DATA(ls_work_order) = go_work_order->read_work_order_by_id( iv_work_order_id = lv_work_order_id ).

    IF ls_work_order IS INITIAL.
      io_out->write( data = |La orden de trabajo { lv_work_order_id } no existe.|  name = 'Orden de trabajo' ).
    ELSE.
      io_out->write( data = ls_work_order name = 'Orden de trabajo' ).
    ENDIF.
  ENDMETHOD.
  METHOD test_update_work_order.

    "SE EXTRAE LOS DATOS PARA LA MODIFICACIÓN DE LA ORDEN DE TRABAJO
    DATA lv_work_order_id TYPE n LENGTH  10 VALUE '6' .

    DATA(ls_work_order) = go_work_order->read_work_order_by_id( iv_work_order_id = lv_work_order_id ).

    IF ls_work_order IS INITIAL.
      io_out->write( |La orden de trabajo { lv_work_order_id } no existe.| ).
      RETURN.
    ENDIF.

    TRY.


        ls_work_order-OrderDescription = 'Se cambia la prioridad'.
        ls_work_order-priority = 'A'.


        DATA(ls_work_order_update) =
            CORRESPONDING go_work_order->ty_work_order_update( ls_work_order
                                                               MAPPING customer_id = CustomerId
                                                                       description = OrderDescription
                                                                       priority = PriorityCode
                                                                       status = StatusCode
                                                                       technician_id = TechnicianId
                                                                       work_order_id = Id ).


        DATA(lv_result) =  go_work_order->update_work_order( iv_work_order = ls_work_order_update ).

        IF lv_result = abap_false.
          io_out->write( 'Error al actualizar' ).
        ELSE.
          io_out->write( 'Orden actualizada correctamente' ).
        ENDIF.
      CATCH zcx_validator_error  INTO DATA(lx_error).
        io_out->write( lx_error->get_text( ) ).
      CATCH cx_root INTO DATA(lx_root).
        io_out->write( lx_root->get_text( ) ).

    ENDTRY.

  ENDMETHOD.

  METHOD load_data_test.

    DATA: lt_customers   TYPE SORTED TABLE OF ztcustomer_ymi WITH UNIQUE KEY customer_id,
          lt_technicians TYPE SORTED TABLE OF zttechnician_ymi WITH UNIQUE KEY technician_id,
          lt_status   TYPE TABLE OF ztstatus_ymi,
          lt_priority TYPE TABLE OF ztpriority_ymi.


    lt_customers = VALUE #( ( customer_id =  1 name = 'Andrea Leones' address = 'Rcto. El Guabo' phone = '05025313' )
                            ( customer_id =  2 name = 'Dayanna Andocilla' address = 'Rcto. El Guabo' phone = '05025313' )
                            ( customer_id =  3 name = 'Jomaira Miranda' address = 'Rcto. El Guabo' phone = '05025313' )
                            ( customer_id =  4 name = 'Ezequiel Miranda' address = 'Rcto. El Guabo' phone = '05025313' )
                            ( customer_id =  5 name = 'Rosa Jara' address = 'Rcto. El Guabo' phone = '05025313' )
                         ).

    lt_technicians = VALUE #( ( technician_id = 1 name = 'Yomar Miranda' specialty = 'Programador' ) ).


    lt_status = VALUE #( (  status_code = 'PE'  status_description = 'PENDING'  )
                         (  status_code = 'CO'  status_description = 'COMPLETED'  )
                       ).
    lt_priority = VALUE #( ( priority_code = 'A' priority_description = 'High' )
                            ( priority_code = 'B' priority_description = 'Low' ) ).


    MODIFY ztstatus_ymi FROM TABLE @lt_status.
    MODIFY ztpriority_ymi FROM TABLE @lt_priority.
    MODIFY ztcustomer_ymi FROM TABLE @lt_customers.
    MODIFY zttechnician_ymi FROM TABLE @lt_technicians.


  ENDMETHOD.



  METHOD is_valid_auth_check.
    AUTHORITY-CHECK OBJECT 'ZAOWORKORD'
    ID 'ZAFWORKORD' FIELD iv_work_order_id
    ID 'ACTVT' FIELD iv_activity.
    IF sy-subrc = 0.
      rv_result = abap_true.

    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD test_authorization.
    DATA lv_result TYPE abap_bool.

* CONSULTAR SI TIENE AUTORIZACIÓN PARA ACTUALIZAR
    lv_result = is_valid_auth_check( iv_activity = '02' iv_work_order_id = '2'  ).
    IF lv_result = abap_true.
      me->test_update_work_order( io_out ).
    ELSE.
      io_out->write( |No tiene permiso para actualizar| ).
    ENDIF.
* CONSULTAR SI TIENE AUTORIZACIÓN PARA CONSULTAR
    lv_result = is_valid_auth_check( iv_activity = '03' iv_work_order_id = '2'  ).
    IF lv_result = abap_true.
      me->test_read_work_orders( io_out ).
      me->test_read_work_order_by_id( io_out ).
    ELSE.
      io_out->write( |No tiene permiso para consultar| ).
    ENDIF.

* CONSULTAR SI TIENE AUTORIZACIÓN PARA ELIMINAR
    lv_result = is_valid_auth_check( iv_activity = '06' iv_work_order_id = '2'  ).
    IF lv_result = abap_true.
      me->test_delete_work_order( io_out ).
    ELSE.
      io_out->write( |No tiene permiso para eliminar| ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
