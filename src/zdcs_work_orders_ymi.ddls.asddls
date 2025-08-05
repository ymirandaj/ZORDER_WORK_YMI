@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consulta las órdenes de trabajo'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZDCS_WORK_ORDERS_YMI
  as select from ztworkorder_ymi  as work_orders
    inner join   ztcustomer_ymi   as customers  on customers.customer_id = work_orders.customer_id
    inner join   zttechnician_ymi as technician on technician.technician_id = work_orders.technician_id
    inner join   ztstatus_ymi     as status     on status.status_code = work_orders.status
    inner join   ztpriority_ymi   as priority   on priority.priority_code = work_orders.priority
{
  key work_orders.work_order_id as Id,
      customers.customer_id   as CustomerId,
      customers.name            as Customer,
      technician.technician_id as TechnicianId, 
      technician.name as Technician,
      work_orders.creation_date as CreationDate, 
      status.status_code as StatusCode,
      status.status_description as Status,
      priority.priority_code as PriorityCode,
      priority.priority_description as Priority,
      work_orders.description as OrderDescription
}
