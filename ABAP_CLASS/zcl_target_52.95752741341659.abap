First, we'll create a class ZCL_DTECM and implement the required methods for the functionality conversion. Then, we'll replace the SQL query with a CDS view ZCDS_DTECM.

Create a new class ZCL_DTECM with the below code:

```ABAP
CLASS zcl_dtecm DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    TYPES: BEGIN OF ty_mara,
             matnr TYPE mara-matnr,
             mtart TYPE mara-mtart,
             mbrsh TYPE mara-mbrsh,
             matkl TYPE mara-matkl,
           END OF ty_mara.
    DATA: wa_mara TYPE ty_mara.
    METHODS: get_mara_data
             IMPORTING
               iv_matnr TYPE mara-matnr
             RETURNING
               VALUE(rv_mara_data) TYPE ty_mara,
             display_data.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_dtecm IMPLEMENTATION.

  METHOD get_mara_data.
    SELECT SINGLE * FROM zcds_dtecm
      INTO @DATA(ls_mara)
      WHERE matnr = @iv_matnr.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_mara TO rv_mara_data.
    ENDIF.
  ENDMETHOD.

  METHOD display_data.
    IF wa_mara IS NOT INITIAL.
      WRITE: /10 wa_mara-matnr, " MATERIAL NUMBER
              wa_mara-mtart, " MATERIAL TYPE
              wa_mara-mbrsh, " INDUSTRY SECTOR
              wa_mara-matkl. " MATERIAL ID
    ELSE.
      WRITE: /10 'wa_mara is empty...'.
    ENDIF.
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.
    DATA(lo_dtecm) = NEW zcl_dtecm( ).
    lo_dtecm->wa_mara = lo_dtecm->get_mara_data( iv_matnr = '00000000000000000023' ).
    lo_dtecm->display_data( ).
  ENDMETHOD.

ENDCLASS.
```

Next, create a CDS view ZCDS_DTECM as per your requirement to replace the SQL query.

Now the class ZCL_DTECM has replaced the function module with modern ABAP Object Oriented, and the SQL query is also replaced with a CDS view ZCDS_DTECM.