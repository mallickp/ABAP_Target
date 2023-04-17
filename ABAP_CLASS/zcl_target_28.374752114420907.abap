Here's the converted ABAP object-oriented code for class ZCL_DTECM:

```ABAP
CLASS ZCL_DTECM DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      SELECT_MARA
        IMPORTING
          IM_MATNR TYPE MARA-MATNR
        RETURNING
          VALUE(RS_MARA) TYPE MARA,
      SHOW_RESULT
        IMPORTING
          IS_MARA TYPE MARA.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ZCL_DTECM IMPLEMENTATION.

  METHOD SELECT_MARA.
    SELECT SINGLE *
      FROM zcds_dtecm  "Replace table MARA with CDS view ZCDS_DTECM
      INTO CORRESPONDING FIELDS OF RS_MARA
      WHERE matnr = IM_MATNR.

  ENDMETHOD.

  METHOD SHOW_RESULT.
    IF IS_MARA IS NOT INITIAL.
      WRITE: /10:
        IS_MARA-MATNR, 'MATERIAL NUMBER',
        IS_MARA-MTART, 'MATERIAL TYPE',
        IS_MARA-MBRSH, 'INDUSTRY SECTOR',
        IS_MARA-MATKL, 'MATERIAL ID'.
    ELSE.
      WRITE: /10, 'IS_MARA is empty...'.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA(lo_dtecm) = NEW ZCL_DTECM( ).
  DATA(ls_mara) TYPE MARA.
  CONSTANTS: lc_matnr TYPE MARA-MATNR VALUE '000000000000000023'.

  ls_mara = lo_dtecm->SELECT_MARA( IM_MATNR = lc_matnr ).

  IF SY-SUBRC = 0.
    WRITE: /10, 'SELECT SUCCESSFUL'.
  ELSE.
    WRITE: /10, 'SELECT NOT SUCCESSFUL'.
  ENDIF.

  lo_dtecm->SHOW_RESULT( IS_MARA = ls_mara ).
```

This code modernizes the given ABAP function module and replaces the SQL query with the CDS view ZCDS_DTECM. The class ZCL_DTECM contains two methods - SELECT_MARA which takes the material number as input and fetches the corresponding MARA record from the ZCDS_DTECM CDS view, and SHOW_RESULT which displays the result in the required format. The START-OF-SELECTION part has been updated to use the methods of ZCL_DTECM.