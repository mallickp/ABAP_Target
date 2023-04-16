Here is the modernized ABAP Object Oriented code for class ZCL_DTECM which replaces the SQL query with CDS view ZCDS_DTECM.

```abap
CLASS zcl_dtecm DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_domain_fixed_val,
             ddlanguage TYPE ddlanguage,
             valpos     TYPE valpos,
             domvalue_l TYPE domvalue_l,
             ddtext     TYPE ddtext,
           END OF ty_domain_fixed_val.

    TYPES: tt_domain_fixed_val TYPE STANDARD TABLE OF ty_domain_fixed_val WITH DEFAULT KEY.

    METHODS get_domain_fixed_val
      IMPORTING
        !iv_domname   TYPE domname
      EXPORTING
        !ev_domname   TYPE domname
      CHANGING
        !ct_domain_fixed_val TYPE tt_domain_fixed_val
      RAISING
        cx_sy_itab_line_not_found.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_dtecm IMPLEMENTATION.

  METHOD get_domain_fixed_val.

    DATA: lt_domain_fixed_val TYPE tt_domain_fixed_val,
          ls_bapi_ret         TYPE bapiret2.

    IF iv_domname IS INITIAL.
      RAISE EXCEPTION TYPE cx_sy_itab_line_not_found
        EXPORTING
          textid = 'CNV_2010C'
          msgno  = '000'
          msgv1  = text-012.
    ENDIF.

    TRY.
        SELECT ddlanguage, valpos, domvalue_l, ddtext
          INTO CORRESPONDING FIELDS OF TABLE lt_domain_fixed_val
          FROM zcds_dtecm
          WHERE domname = @iv_domname
          AND ddlanguage EQ @sy-langu
          AND as4local EQ 'A'.

        IF lt_domain_fixed_val IS INITIAL.
          RAISE EXCEPTION TYPE cx_sy_itab_line_not_found
            EXPORTING
              textid = 'CNV_2010C'
              msgno  = '000'
              msgv1  = text-013.
        ELSE.
          ct_domain_fixed_val = lt_domain_fixed_val.
          ev_domname = iv_domname.
        ENDIF.

      CATCH cx_sy_itab_line_not_found.
        APPEND INITIAL LINE TO ct_domain_fixed_val ASSIGNING FIELD-SYMBOL(<fs_domain_fixed_val>).
        <fs_domain_fixed_val>-ddlanguage = 'E'.
        <fs_domain_fixed_val>-valpos = 0.
        <fs_domain_fixed_val>-domvalue_l = 'E'.
        <fs_domain_fixed_val>-ddtext = text-013.
        ev_domname = iv_domname.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
```

This class ZCL_DTECM contains a method get_domain_fixed_val which replaces the function module cnv_2010c_get_domain_fixed_val in the original code. The method takes the input parameter iv_domname, an exporting parameter ev_domname and a changing table ct_domain_fixed_val. The CDS view ZCDS_DTECM is used in the SELECT query to get the domain fixed values. The exceptions are handled using cx_sy_itab_line_not_found.