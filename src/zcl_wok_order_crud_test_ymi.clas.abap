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
      load_data_test.
    DATA go_work_order TYPE REF TO  zclwork_order_crud_handler_ymi.
ENDCLASS.



CLASS zcl_wok_order_crud_test_ymi IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    me->load_data_test( ).
    me->test_read_work_orders( out ).


  ENDMETHOD.
  METHOD constructor.
    CREATE OBJECT me->go_work_order.
  ENDMETHOD.

  METHOD test_create_work_order.

    DATA ls_work_order_create TYPE go_work_order->ty_work_order.
    ls_work_order_create-customer_id = '1'.
    ls_work_order_create-technician_id = '1'.
    ls_work_order_create-description = ''.
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
    DATA lv_work_order_id TYPE n LENGTH 10.
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
 DATA(lt_work_orders) = go_work_order->read_work_orders( ).

    IF lt_work_orders IS INITIAL.
      io_out->write( |no hay registros de orden de trabajo| ).
     else.
     io_out->write( data = lt_work_orders name = 'Orden de trabajo' ).
    ENDIF.
  ENDMETHOD.
  METHOD test_read_work_order_by_id.
  "SE EXTRAE LOS DATOS PARA LA MODIFICACIÓN DE LA ORDEN DE TRABAJO
    DATA lv_work_order_id TYPE n LENGTH  10 VALUE '2' .
    DATA(ls_work_order) = go_work_order->read_work_order_by_id( iv_work_order_id = lv_work_order_id ).

    IF ls_work_order IS INITIAL.
      io_out->write( |La orden de trabajo { lv_work_order_id } no existe.| ).
     else.
     io_out->write( data = ls_work_order name = 'Orden de trabajo' ).
    ENDIF.
  ENDMETHOD.
  METHOD test_update_work_order.

    "SE EXTRAE LOS DATOS PARA LA MODIFICACIÓN DE LA ORDEN DE TRABAJO
    DATA lv_work_order_id TYPE n LENGTH  10 VALUE '2' .
    DATA(ls_work_order) = go_work_order->read_work_order_by_id( iv_work_order_id = lv_work_order_id ).

    IF ls_work_order IS INITIAL.
      io_out->write( |La orden de trabajo { lv_work_order_id } no existe.| ).
      RETURN.
    ENDIF.

    TRY.

        "SE MODIFICAN LOS DATOS
        ls_work_order-creation_date = cl_abap_context_info=>get_system_date( ).
        ls_work_order-description = 'Se cambia la prioridad'.
        ls_work_order-priority = 'A'.



        DATA(lv_result) =  go_work_order->update_work_order( iv_work_order = ls_work_order ).

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
          lt_technicians TYPE SORTED TABLE OF zttechnician_ymi WITH UNIQUE KEY technician_id.


    lt_customers = VALUE #( ( customer_id =  1 name = 'Andrea Leones' address = 'Rcto. El Guabo' phone = '05025313' )
                            ( customer_id =  2 name = 'Dayanna Andocilla' address = 'Rcto. El Guabo' phone = '05025313' )
                            ( customer_id =  3 name = 'Jomaira Miranda' address = 'Rcto. El Guabo' phone = '05025313' )
                            ( customer_id =  4 name = 'Ezequiel Miranda' address = 'Rcto. El Guabo' phone = '05025313' )
                            ( customer_id =  5 name = 'Rosa Jara' address = 'Rcto. El Guabo' phone = '05025313' )
                         ).

    lt_technicians = VALUE #( ( technician_id = 1 specialty = 'Yomar Miranda' ) ).

    MODIFY ztcustomer_ymi FROM TABLE @lt_customers.
    MODIFY zttechnician_ymi FROM TABLE @lt_technicians.


  ENDMETHOD.

ENDCLASS.
