 "! <p class="shorttext synchronized" lang="en">Clase zcx_validator_error </p>
   "! Clase para manejo de las excepciones del módulo.
CLASS zcx_validator_error DEFINITION
  PUBLIC
  INHERITING FROM cx_no_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        textid    LIKE textid OPTIONAL
        !previous LIKE previous OPTIONAL
        text      TYPE string OPTIONAL.

    METHODS get_text REDEFINITION.
  PROTECTED SECTION.
    DATA: msg_error      TYPE string
    .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_validator_error IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor(
         textid = textid
         previous = previous
       ).

    IF text IS NOT INITIAL.
      me->msg_error = text.
    ENDIF.

  ENDMETHOD.

  METHOD get_text.
    result = |{ msg_error }|.
  ENDMETHOD.
ENDCLASS.
