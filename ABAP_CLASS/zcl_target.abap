To convert the ABAP function module to an ABAP object-oriented class and replace the SQL query with a CDS view, follow these steps:

1. Create the class ZCL_DTECM
2. Create the methods within the class
3. Replace the SQL query with a CDS view

Here's the converted class and method:

```abap
CLASS zcl_dtecm DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    CLASS-METHODS get_domain_fixed_val
      IMPORTING
        VALUE(iv_domname) TYPE domname
      EXPORTING
        VALUE(ev_domname) TYPE domname
      TABLES
        et_domain_fixed_val TYPE cnv_2010c_t_domain_fixed_value
      EXCEPTIONS
        NOT_FOUND.

  PRIVATE SECTION.
    CLASS-METHODS fill_bapiret2
      IMPORTING
        !type TYPE syuzeit DEFAULT 'E'
        !cl TYPE symsgv DEFAULT 'CNV_2010C'
        !number TYPE symsgno DEFAULT '000'
      EXPORTING
        !return TYPE bapiret2.

ENDCLASS.

CLASS zcl_dtecm IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    TRY.
        get_domain_fixed_val(
          EXPORTING
            iv_domname = iv_domname
          IMPORTING
            ev_domname = ev_domname
          TABLES
            et_domain_fixed_val = et_domain_fixed_val
        ).
      CATCH NOT_FOUND.
        " Exception handling, e.g., display error message
    ENDTRY.
  ENDMETHOD.

  METHOD get_domain_fixed_val.
    DATA: ls_bapi_ret TYPE bapiret2.

    IF iv_domname IS INITIAL.
      fill_bapiret2(
        EXPORTING
          return = ls_bapi_ret
      ).
      ls_bapi_ret-message = 'Domain Name is empty.'.

      APPEND ls_bapi_ret TO return.
      RETURN.
    ENDIF.

    " Replace the SQL query with a CDS view (ZCDS_DTECM)
    SELECT * FROM zcds_dtecm
      INTO CORRESPONDING FIELDS OF TABLE et_domain_fixed_val
      WHERE domname = iv_domname AND ddlanguage = sy-langu.

    IF sy-subrc = 0.
      SORT et_domain_fixed_val ASCENDING BY valpos.
      ev_domname = iv_domname.
    ELSE.
      RAISE NOT_FOUND.
    ENDIF.
  ENDMETHOD.

  METHOD fill_bapiret2.
    return-type = type.
    return-id = cl.
    return-number = number.
    return-message = 'Error retrieving domain fixed values.'.
  ENDMETHOD.

ENDCLASS.
```

Please note that the provided code assumes a CDS view 'ZCDS_DTECM' that has the same fields as table DD07T. You'll need to create this CDS view in your system with the proper fields and logic according to your requirements.