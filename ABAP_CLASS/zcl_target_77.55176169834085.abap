Below is the conversion of the given ABAP function module into an ABAP object-oriented class, ZCL_DTECM, and replacing the SQL query with CDS view ZCDS_DTECM.

CLASS zcl_dtecm DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_domain_fixed_val,
             ddlanguage TYPE ddlanguage,
             valpos     TYPE valpos,
             domvalue_l TYPE domvalue_l,
             ddtext     TYPE ddtext,
           END OF ty_domain_fixed_val,
           tt_domain_fixed_val TYPE STANDARD TABLE OF ty_domain_fixed_val.

    METHODS get_domain_fixed_val
      IMPORTING
        !iv_domname    TYPE domname
      EXPORTING
        !ev_domname    TYPE domname
      CHANGING
        !et_domain_fixed_val TYPE tt_domain_fixed_val
        !return_struct TYPE bapiret2 OPTIONAL.

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

      APPEND ls_bapi_ret TO return_struct.
      RETURN.
    ENDIF.

    SELECT ddlanguage valpos domvalue_l ddtext
      FROM zcds_dtecm
      INTO CORRESPONDING FIELDS OF TABLE et_domain_fixed_val
      WHERE domname = @iv_domname
        AND ddlanguage = @sy-langu
        AND as4local = 'A'.

    IF sy-subrc = 0.
      SORT et_domain_fixed_val ASCENDING BY valpos.
      ev_domname = iv_domname.
    ELSE.
      SELECT ddlanguage valpos domvalue_l ddtext
        FROM zcds_dtecm
        INTO CORRESPONDING FIELDS OF TABLE et_domain_fixed_val
        WHERE domname = @iv_domname
          AND ddlanguage = 'E'
          AND as4local = 'A'.

      IF sy-subrc = 0.
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

        APPEND ls_bapi_ret TO return_struct.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.