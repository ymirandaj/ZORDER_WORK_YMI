CLASS zcl_history_work_ord_ymi DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
   "! <p class="shorttext synchronized" lang="en">Agrega historial de la orden de trabajo</p>
      "! Este método guarda el historial de la orden de trabajo
      "! @parameter iv_work_order_id | Parámetro que indica el id de la orden de trabajo
    METHODS save_work_order_hist
      IMPORTING iv_work_order_id TYPE zde_work_order_id_ymi
      RETURNING VALUE(rv_result) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_history_work_ord_ymi IMPLEMENTATION.
  METHOD save_work_order_hist.
    DATA lv_hist_id TYPE n LENGTH 10 .

    "OBTENER LOS DATOS ACTUALES DE LA ORDEN ANTES DE MODIFICAR

    SELECT SINGLE FROM  ztworkorder_ymi
    FIELDS work_order_id, description
    INTO @DATA(ls_work_order).

    IF sy-subrc <> 0.
      rv_result = abap_false.
      RETURN.
    ENDIF.


"ASIGNACIÓN DE LA ÚLTIMO ID DEL HISTORIAL
    SELECT FROM  ztworkordhistymi
    FIELDS MAX( history_id )
    INTO @lv_hist_id.
    lv_hist_id = lv_hist_id + 1.


    DATA ls_work_order_hist TYPE ztworkordhistymi.

    ls_work_order_hist-history_id =  lv_hist_id .
    ls_work_order_hist-modification_date = cl_abap_context_info=>get_system_date(  ).
    ls_work_order_hist-work_order_id = ls_work_order-work_order_id.
    ls_work_order_hist-change_description = ls_work_order-description.

    INSERT ztworkordhistymi FROM @ls_work_order_hist.

    IF sy-subrc <> 0.
      rv_result = abap_false.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
