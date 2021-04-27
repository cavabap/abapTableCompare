interface ZIF_TABLE_COMPARISON
  public .
  "!     compare two identical itabs (old vs. new; PBO vs. PAI)
  "! CHANGEDOCUMENT_PREPARE_TABLES
  "!
  "!  The static method COMPARE contains two IMPORTING parameters
  "!  corresponding to the internal tables storing the PAI data (new) and
  "!  the PBO data (old), respectively. The results of the comparison are
  "!  exported as internal tables having the same line type
  "!  as the PBO/PAI data.
  METHODS
    compare
      IMPORTING
        VALUE(it_itab_new) TYPE table " itab with current data (PAI)
        VALUE(it_itab_old) TYPE table " itab with old data     (PBO)
      EXPORTING
        et_insert          TYPE table " itab with new data
        et_update          TYPE table " itab with changed data
        et_delete          TYPE table " itab with deleted data
      RAISING zcx_table_comparison.

endinterface.
