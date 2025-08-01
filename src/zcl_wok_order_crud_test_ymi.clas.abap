CLASS zcl_wok_order_crud_test_ymi DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor.
    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA go_work_order TYPE REF TO  zclwork_order_crud_handler_ymi.
ENDCLASS.



CLASS zcl_wok_order_crud_test_ymi IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.



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

    DATA lv_status TYPE zde_status_code_ymi.
    TRY.



        DATA ls_work_order_create TYPE go_work_order->ty_work_order.
        ls_work_order_create-work_order_id = '4'.
        ls_work_order_create-customer_id = '1'.
        ls_work_order_create-description = 'Prueba de la orden #3'.
        ls_work_order_create-priority = 'A'.
        ls_work_order_create-technician_id = '1'.

        DATA(lv_result) = go_work_order->create_work_order( iv_work_order =  ls_work_order_create ).

        IF lv_result IS INITIAL.
          out->write( 'No se puede ingresar el registro' ).
        ELSE.
          out->write( 'Validación correcta' ).

          data(lt_work_order) = go_work_order->read_work_orders( ).
          out->write( lt_work_order ).
        ENDIF.

      CATCH cx_root INTO DATA(lx_error).
        out->write( lx_error->get_text( ) ).
    ENDTRY.


  ENDMETHOD.
  METHOD constructor.
    CREATE OBJECT me->go_work_order.
  ENDMETHOD.

ENDCLASS.
