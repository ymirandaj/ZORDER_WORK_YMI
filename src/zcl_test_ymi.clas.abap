CLASS zcl_test_ymi DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_test_ymi IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

data: lt_status type table of ztstatus_ymi,
     lt_priority type table of ztpriority_ymi.

lt_status = value #( (  status_code = 'PE'  status_description = 'PENDING'  )
                     (  status_code = 'CO'  status_description = 'COMPLETED'  )
                   ).
lt_priority = value #( ( priority_code = 'A' priority_description = 'High' )
                        ( priority_code = 'B' priority_description = 'Low' ) ).


  modify ztstatus_ymi from table @lt_status.
modify ztpriority_ymi from table @lt_priority.

  SELECT FROM zdcs_work_orders_ymi
    FIELDS Id, CustomerId, Customer, TechnicianId, Technician, CreationDate, StatusCode, Status,
           PriorityCode, Priority, OrderDescription
    ORDER BY id ASCENDING
    INTO TABLE @DATA(rv_result).

    out->write( rv_result ).
*    OFFSET @lv_offset
*    UP TO @iv_records_per_page ROWS.
  ENDMETHOD.
ENDCLASS.
