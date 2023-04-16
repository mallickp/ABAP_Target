CLASS zcl_dtecm DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get_domain_fixed_val
      IMPORTING
        !iv_domname TYPE domname
      EXPORTING
        !ev_domname TYPE domname
      TABLES
        !et_domain_fixed_val TYPE cnv_2010c_t_domain_fixed_value
        !return OPTIONAL STRUCTURE bapiret2.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_dtecm IMPLEMENTATION.

  METHOD get_domain_fixed_val.

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
      WHERE domname = @iv_domname
      AND ddlanguage = @sy-langu.

    IF sy-subrc EQ 0.
      SORT et_domain_fixed_val ASCENDING BY valpos.
      ev_domname = iv_domname.
    ELSE.
      SELECT *
        FROM zcds_dtecm
        INTO CORRESPONDING FIELDS OF TABLE et_domain_fixed_val
        WHERE domname = @iv_domname
        AND ddlanguage = 'E'.

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
