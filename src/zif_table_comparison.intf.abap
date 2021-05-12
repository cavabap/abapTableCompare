interface ZIF_TABLE_COMPARISON
  public .

  "! <p class="shorttext synchronized" lang="en">compare two identical itabs </p>
  "! e.g. old vs. new,  PBO vs. PAI
  "! uses CHANGEDOCUMENT_PREPARE_TABLES
  "!
  "!
  "!  COMPARE contains two IMPORTING parameters
  "!  corresponding to the internal tables storing the PAI data (new) and
  "!  the PBO data (old), respectively. The results of the comparison are
  "!  exported as internal tables having the same line type
  "!  as the PBO/PAI data.
  "!
  "! @parameter internal_table_new | <p class="shorttext synchronized" lang="en">itab with current data (PAI)</p>
  "! @parameter internal_table_old | <p class="shorttext synchronized" lang="en">itab with old data (PBO)</p>
  "! @parameter inserted | <p class="shorttext synchronized" lang="en">itab with new data</p>
  "! @parameter updated | <p class="shorttext synchronized" lang="en">itab with changed data</p>
  "! @parameter deleted | <p class="shorttext synchronized" lang="en">itab with deleted data</p>
  "! @raising zcx_table_comparison | <p class="shorttext synchronized" lang="en"></p>
  METHODS
    compare
      IMPORTING
        VALUE(internal_table_new) TYPE table
        VALUE(internal_table_old) TYPE table
      EXPORTING
        inserted          TYPE table
        updated          TYPE table
        deleted          TYPE table
      RAISING zcx_table_comparison.


  "! <p class="shorttext synchronized" lang="en">compare current itabs with contents of database table</p>
  "! e.g. old vs. new,  PBO vs. PAI
  "! uses compare method
  "!
  "! @parameter internal_table | <p class="shorttext synchronized" lang="en">itab with current data (PAI)</p>
  "! @parameter inserted | <p class="shorttext synchronized" lang="en">itab with new data</p>
  "! @parameter updated | <p class="shorttext synchronized" lang="en">itab with changed data</p>
  "! @parameter deleted | <p class="shorttext synchronized" lang="en">itab with deleted data</p>
  "! @raising zcx_table_comparison | <p class="shorttext synchronized" lang="en"></p>
  METHODS
    compare_with_database
      IMPORTING
        VALUE(internal_table) TYPE table
      EXPORTING
        inserted          TYPE table
        updated          TYPE table
        deleted          TYPE table
      RAISING zcx_table_comparison.

endinterface.
