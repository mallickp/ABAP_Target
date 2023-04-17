Here's the optimized source code using ABAP 7.40 syntax and features, like inline declarations and string templates. The function modules have been converted to a class:

```abap
CLASS lcl_cnv_2010c DEFINITION.
  PUBLIC SECTION.
    METHODS fill_bapiret2
      IMPORTING
        !type         TYPE bapireturn-type
        !cl           TYPE symsgid
        !number       TYPE symsgno
        !par1         TYPE symsgv1 DEFAULT space
        !par2         TYPE symsgv2 DEFAULT space
        !par3         TYPE symsgv3 DEFAULT space
        !par4         TYPE symsgv4 DEFAULT space
        !log_no       TYPE bapireturn-log_no DEFAULT space
        !log_msg_no   TYPE bapireturn-log_msg_no DEFAULT space
        !parameter    TYPE bapiret2-parameter DEFAULT space
        !row          TYPE bapiret2-row DEFAULT 0
        !field        TYPE bapiret2-field DEFAULT space
      RETURNING
        VALUE(ret)   TYPE bapiret2.

    METHODS get_domain_fixed_val
      IMPORTING
        !iv_domname       TYPE domname
      EXPORTING
        !ev_domname       TYPE domname
      TABLES
        et_domain_fixed_val TYPE STANDARD TABLE OF dd07t
      EXCEPTIONS
        not_found.
ENDCLASS.

CLASS lcl_cnv_2010c IMPLEMENTATION.
  METHOD fill_bapiret2.
    CLEAR ret.

    ret-type = type.
    ret-id = cl.
    ret-number = number.
    ret-message_v1 = par1.
    ret-message_v2 = par2.
    ret-message_v3 = par3.
    ret-message_v4 = par4.
    ret-parameter = parameter.
    ret-row = row.
    ret-field = field.
    ret-system = sy-sysid.

    MESSAGE ID cl TYPE type NUMBER number
            WITH par1 par2 par3 par4
            INTO ret-message.

    ret-log_no = log_no.
    ret-log_msg_no = log_msg_no.
  ENDMETHOD.

  METHOD get_domain_fixed_val.
    DATA: lt_domain_fixed_val TYPE STANDARD TABLE OF dd07t,
          lt_return           TYPE STANDARD TABLE OF bapiret2,
          ls_bapi_ret         TYPE bapiret2.

    IF iv_domname IS INITIAL.
      ls_bapi_ret = fill_bapiret2( type = 'E' cl = 'CNV_2010C' number = '000' ).
      ls_bapi_ret-message = text-012.
      APPEND ls_bapi_ret TO lt_return.
      RETURN.
    ENDIF.

    SELECT ddlanguage valpos domvalue_l ddtext
      FROM dd07t INTO TABLE lt_domain_fixed_val
      WHERE domname = iv_domname AND ddlanguage EQ sy-langu AND as4local EQ 'A'.

    IF sy-subrc EQ 0.
      SORT lt_domain_fixed_val ASCENDING BY valpos.
      et_domain_fixed_val = lt_domain_fixed_val.
      ev_domname = iv_domname.
    ELSE.
      SELECT ddlanguage valpos domvalue_l ddtext
        FROM dd07t INTO TABLE lt_domain_fixed_val
        WHERE domname = iv_domname AND ddlanguage EQ 'E' AND as4local EQ 'A'.

      IF sy-subrc EQ 0.
        SORT lt_domain_fixed_val ASCENDING BY valpos.
        et_domain_fixed_val = lt_domain_fixed_val.
        ev_domname = iv_domname.
      ELSE.
        ls_bapi_ret = fill_bapiret2( type = 'E' cl = 'CNV_2010C' number = '000' ).
        ls_bapi_ret-message = text-013.
        APPEND ls_bapi_ret TO lt_return.
        RAISE EXCEPTION TYPE cx_sy_itab_line_not_found.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

To use the new class, replace calls to the original function modules with calls to the corresponding methods of the lcl_cnv_2010c class:

```abap
DATA(lo_cnv_2010c) = NEW lcl_cnv_2010c( ).
DATA(lt_domain_fixed_val) = VALUE dd07t_tab( ).
DATA(ls_bapiret2) TYPE bapiret2.
DATA(lv_domname) TYPE domname.

TRY.
    lo_cnv_2010c->get_domain_fixed_val( IMPORTING iv_domname = lv_domname
                                         EXPORTING ev_domname = lv_domname
                                         TABLES et_domain_fixed_val = lt_domain_fixed_val ).
  CATCH cx_sy_itab_line_not_found.
    " Handle exception
ENDTRY.

ls_bapiret2 = lo_cnv_2010c->fill_bapiret2( type = 'E' cl = 'CNV_2010C' number = '000' ).
```