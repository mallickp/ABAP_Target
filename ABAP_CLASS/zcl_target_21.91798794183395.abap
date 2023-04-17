In order to convert the given function module to an object-oriented ABAP class, we can follow these steps:

1. Create a new class `ZCL_DTECM` with the necessary interfaces and definitions.
2. Replace the function module with a method in the class.
3. Replace the SQL query with a call to the CDS view `ZCDS_DTECM`.

Here's the code for the new class:

```abap
CLASS zcl_dtecm DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    METHODS cnv_2010c_get_domain_fixed_val
      IMPORTING
        !iv_domname TYPE domname
      EXPORTING
        !ev_domname TYPE domname
      CHANGING
        !et_domain_fixed_val TYPE cnv_2010c_t_domain_fixed_value
        !return TYPE bapiret2 OPTIONAL .

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_dtecm IMPLEMENTATION.

  METHOD cnv_2010c_get_domain_fixed_val.

    DATA: ls_bapi_ret TYPE bapiret2.

    IF iv_domname IS INITIAL.

      CALL FUNCTION 'CNV_2010C_FILL_BAPIRET2'
        EXPORTING
          type   = 'E'
          cl     = 'CNV_2010C'
          number = '000'
        IMPORTING
          return = ls_bapi_ret.

      ls_bapi_ret-message = text-012.

      APPEND ls_bapi_ret TO return.
      RETURN.

    ENDIF.

    SELECT *
      FROM zcds_dtecm
      INTO CORRESPONDING FIELDS OF TABLE et_domain_fixed_val
      WHERE domname = iv_domname
      AND ddlanguage = sy-langu
      AND as4local = 'A'.

    IF sy-subrc EQ 0.

      SORT et_domain_fixed_val ASCENDING BY valpos.
      ev_domname = iv_domname.

    ELSE.

      SELECT *
        FROM zcds_dtecm
        INTO CORRESPONDING FIELDS OF TABLE et_domain_fixed_val
        WHERE domname = iv_domname
        AND ddlanguage = 'E'
        AND as4local = 'A'.

      IF sy-subrc EQ 0.

        SORT et_domain_fixed_val ASCENDING BY valpos.
        ev_domname = iv_domname.

      ELSE.

        CALL FUNCTION 'CNV_2010C_FILL_BAPIRET2'
          EXPORTING
            type   = 'E'
            cl     = 'CNV_2010C'
            number = '000'
          IMPORTING
            return = ls_bapi_ret.

        ls_bapi_ret-message = text-013.

        APPEND ls_bapi_ret TO return.

      ENDIF.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
```

This class has a method `cnv_2010c_get_domain_fixed_val` that replaces the original function module. It uses the CDS view `ZCDS_DTECM` instead of the SQL query to fetch data from the database.