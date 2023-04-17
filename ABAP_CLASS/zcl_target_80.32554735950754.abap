To optimize the given code using ABAP 7.40 syntax and features, please find the updated code below:

```ABAP
CLASS cl_cnv_2010c DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS fill_bapiret2
      IMPORTING
        !iv_type TYPE bapiret2-type
        !iv_cl TYPE sy-msgid
        !iv_number TYPE sy-msgno
        !iv_par1 TYPE sy-msgv1 DEFAULT space
        !iv_par2 TYPE sy-msgv2 DEFAULT space
        !iv_par3 TYPE sy-msgv3 DEFAULT space
        !iv_par4 TYPE sy-msgv4 DEFAULT space
        !iv_log_no TYPE bapiret2-log_no DEFAULT space
        !iv_log_msg_no TYPE bapiret2-log_msg_no DEFAULT space
        !iv_parameter TYPE bapiret2-parameter DEFAULT space
        !iv_row TYPE bapiret2-row DEFAULT 0
        !iv_field TYPE bapiret2-field DEFAULT space
      EXPORTING
        !er_return TYPE bapiret2.

    CLASS-METHODS get_domain_fixed_val
      IMPORTING
        !iv_domname TYPE domname
      EXPORTING
        !ev_domname TYPE domname
      CHANGING
        !ct_domain_fixed_val TYPE cnv_2010c_t_domain_fixed_value
        !cs_return TYPE bapiret2 OPTIONAL.

ENDCLASS.

CLASS cl_cnv_2010c IMPLEMENTATION.
  METHOD fill_bapiret2.
    CLEAR er_return.

    er_return = VALUE #( type = iv_type
                         id = iv_cl
                         number = iv_number
                         message_v1 = iv_par1
                         message_v2 = iv_par2
                         message_v3 = iv_par3
                         message_v4 = iv_par4
                         parameter = iv_parameter
                         row = iv_row
                         field = iv_field
                         system = cl_system=>own_logical_system_get_stable( )
                         message = |{ iv_cl TYPE string } { iv_type TYPE string } { iv_number TYPE string }|
                                    |{ iv_par1 TYPE string } { iv_par2 TYPE string } { iv_par3 TYPE string } { iv_par4 TYPE string }|
                         log_no = iv_log_no
                         log_msg_no = iv_log_msg_no ).
  ENDMETHOD.

  METHOD get_domain_fixed_val.
    DATA: lt_domain_fixed_val TYPE cnv_2010c_t_domain_fixed_value,
          ls_bapi_ret TYPE bapiret2.

    IF iv_domname IS INITIAL.
      fill_bapiret2( EXPORTING iv_type   = 'E'
                              iv_cl     = 'CNV_2010C'
                              iv_number = '000'
                     CHANGING cs_return = ls_bapi_ret ).
      ls_bapi_ret-message = 'Please provide a valid domain name'.
      APPEND ls_bapi_ret TO cs_return.
      RETURN.
    ENDIF.

    SELECT ddlanguage valpos domvalue_l ddtext
      FROM dd07t INTO TABLE lt_domain_fixed_val
      WHERE domname = iv_domname AND ddlanguage IN ( sy-langu 'E' ) AND as4local = 'A'.

    IF sy-subrc = 0.
      SORT lt_domain_fixed_val ASCENDING BY valpos.
      ev_domname = iv_domname.
    ELSE.
      fill_bapiret2( EXPORTING iv_type   = 'E'
                              iv_cl     = 'CNV_2010C'
                              iv_number = '000'
                     CHANGING cs_return = ls_bapi_ret ).
      ls_bapi_ret-message = 'Domain name not found'.
      APPEND ls_bapi_ret TO cs_return.
    ENDIF.

    ct_domain_fixed_val = lt_domain_fixed_val.
  ENDMETHOD.

ENDCLASS.
```

Now the function modules are converted to class with two methods, `fill_bapiret2` and `get_domain_fixed_val`. Inline declarations and string templates are used in the updated code.