You can modernize the given function module by creating a new ABAP class `ZCL_DTECM` and creating a new method `GET_DOMAIN_FIXED_VAL` inside the class. You can then replace the SQL query with the CDS view `ZCDS_DTECM`.

Below is the converted code with ABAP Object-Oriented approach:

```abap
CLASS zcl_dtecm DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: ty_domain_fixed_value TYPE cnv_2010c_t_domain_fixed_value.

    METHODS get_domain_fixed_val
      IMPORTING
        !iv_domname TYPE domname
      EXPORTING
        !ev_domname TYPE domname
      CHANGING
        !ct_domain_fixed_val TYPE ty_domain_fixed_value
        !cs_bapi_ret TYPE bapiret2 OPTIONAL .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dtecm IMPLEMENTATION.

  METHOD get_domain_fixed_val.

    " Check if iv_domname is empty
    IF iv_domname IS INITIAL.
      " Fill bapiret2
      cs_bapi_ret-type = 'E'.
      cs_bapi_ret-id = 'CNV_2010C'.
      cs_bapi_ret-number = '000'.
      cs_bapi_ret-message = 'Domain name cannot be initial'. " Replace with your message text

      RETURN.
    ENDIF.

    " Select data from CDS view ZCDS_DTECM
    SELECT d~ddlanguage, d~valpos, d~domvalue_l, d~ddtext
      FROM zcds_dtecm AS d
      WHERE d~domname = @iv_domname
      INTO CORRESPONDING FIELDS OF TABLE @ct_domain_fixed_val.

    " Sort data and set ev_domname
    IF sy-subrc = 0.
      SORT ct_domain_fixed_val ASCENDING BY valpos.
      ev_domname = iv_domname.
    ELSE.
      " Fill bapiret2
      cs_bapi_ret-type = 'E'.
      cs_bapi_ret-id = 'CNV_2010C'.
      cs_bapi_ret-number = '000'.
      cs_bapi_ret-message = 'No data found for domain name'. " Replace with your message text

      RETURN.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
```

To use this method in your ABAP code, instantiate the class and call the method as shown below:

```abap
DATA(lo_dtecm) = NEW zcl_dtecm( ).
DATA lt_domain_fixed_val TYPE zcl_dtecm=>ty_domain_fixed_value.
DATA lv_domname TYPE domname.
DATA ls_bapi_ret TYPE bapiret2.

lv_domname = 'YourDomainNameHere'. " Pass the domain name here
lo_dtecm->get_domain_fixed_val(
  EXPORTING
    iv_domname           = lv_domname
  IMPORTING
    ev_domname           = lv_domname
  CHANGING
    ct_domain_fixed_val  = lt_domain_fixed_val
    cs_bapi_ret          = ls_bapi_ret ).
```