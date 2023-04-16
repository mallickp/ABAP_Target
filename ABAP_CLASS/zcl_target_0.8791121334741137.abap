**ABAP**

CLASS zcl_dtecm DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun. " For ABAP Unit Test

    CLASS-METHODS get_domain_fixed_val
      IMPORTING
        !iv_domname TYPE domname
      EXPORTING
        !ev_domname TYPE domname
      CHANGING
        !et_domain_fixed_val TYPE cnv_2010c_t_domain_fixed_value
        !return STRUCTURE bapiret2 OPTIONAL
      RAISING
        cx_static_check.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_dtecm IMPLEMENTATION.

  METHOD get_domain_fixed_val.
    DATA ls_bapi_ret TYPE bapiret2.

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
      RAISE EXCEPTION TYPE cx_static_check.
    ENDIF.

    SELECT d.ddlanguage,
           d.valpos,
           d.domvalue_l,
           d.ddtext
      FROM zcds_dtecm AS d
      INTO CORRESPONDING FIELDS OF TABLE et_domain_fixed_val
      WHERE d.domname = @iv_domname
        AND d.ddlanguage = @sy-langu
        AND d.as4local = 'A'.

    IF sy-subrc EQ 0.
      SORT et_domain_fixed_val ASCENDING BY valpos.
      ev_domname = iv_domname.
    ELSE.
      SELECT d.ddlanguage,
             d.valpos,
             d.domvalue_l,
             d.ddtext
        FROM zcds_dtecm AS d
        INTO CORRESPONDING FIELDS OF TABLE et_domain_fixed_val
        WHERE d.domname = @iv_domname
          AND d.ddlanguage = 'E'
          AND d.as4local = 'A'.

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
        RAISE EXCEPTION TYPE cx_static_check.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.