*&---------------------------------------------------------------------*
*& Report Z_SDN_ITAB_COMPARISON_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_SDN_ITAB_COMPARISON_TEST.


PARAMETERS:
"! NOTE: Mark the checkboxes in order to simulate a scenario
  p_ins    AS CHECKBOX,
"! NOTE: Mark the checkboxes in order to simulate a scenario
  p_upd    AS CHECKBOX,
"! NOTE: Mark the checkboxes in order to simulate a scenario
  p_del    AS CHECKBOX.


class lcl_interactive_test definition create public.

  public section.
    METHODS
    "! interactive_test( ) generates simplified PBO data and PAI data for
    "! demonstrating the functionality of method COMPARE. PBO data and PAI
    "! data as well as the results are displayed as ALV lists.
    "! The interface of method contains three IMPORTING parameters
    "! (flags) for simulating PAI data that contain new, modified or deleted
    "! entries and any combination thereof.
      interactive_test
        IMPORTING
          id_insert TYPE abap_bool
          id_update TYPE abap_bool
          id_delete TYPE abap_bool.
  protected section.
  private section.

endclass.

class lcl_interactive_test implementation.

  method interactive_test.

*   define local data
    DATA:
      ld_gridtitle TYPE lvc_title,
      ls_knb1      TYPE knb1,
      lt_knb1_old  TYPE STANDARD TABLE OF knb1,
      lt_knb1_new  TYPE STANDARD TABLE OF knb1,
*
      lt_knb1_ins  TYPE STANDARD TABLE OF knb1,
      lt_knb1_upd  TYPE STANDARD TABLE OF knb1,
      lt_knb1_del  TYPE STANDARD TABLE OF knb1.



*   Create 4 sample entries
    DO 4 TIMES.
      ls_knb1-bukrs = '1000'.
      ls_knb1-kunnr = syst-index.
      ls_knb1-loevm = abap_false.

      APPEND ls_knb1 TO lt_knb1_new.
      APPEND ls_knb1 TO lt_knb1_old.
    ENDDO.
*   NOTE: itab's are identical.


*   Sorting is crucial for comparison!!!
    SORT lt_knb1_new BY kunnr bukrs.
    SORT lt_knb1_old BY kunnr bukrs.



*   Delete first entry from old itab => first entry of new itab
*   should be inserted (customer = '1' -> CHIND = 'I').
    IF ( id_insert = abap_true ).
      DELETE lt_knb1_old INDEX 1.
    ENDIF.

*   Modify second entry of new itab =>
*   should be updated (customer = '2' -> CHIND = 'U')
    IF ( id_update = abap_true ).
      ls_knb1-loevm = abap_true.

      MODIFY lt_knb1_new FROM ls_knb1 INDEX 2
        TRANSPORTING loevm.
    ENDIF.

*   Delete third entry from new itab =>
*   should be deleted (customer = '3' -> CHIND = 'D').
    IF ( id_delete = abap_true ).
      DELETE lt_knb1_new INDEX 3.
    ENDIF.
*   NOTE: customer = '4' is identical in old and new itab => ignored


*   Display "old" itab
    ld_gridtitle = 'Old Internal Table Entries (e.g. PBO)'(old).
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_structure_name = 'KNB1'
        i_grid_title     = ld_gridtitle
      TABLES
        t_outtab         = lt_knb1_old
      EXCEPTIONS
        program_error    = 1
        OTHERS           = 2.
*   Display "new" itab
    ld_gridtitle = 'New Internal Table Entries (e.g. PAI)'(new).
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_structure_name = 'KNB1'
        i_grid_title     = ld_gridtitle
      TABLES
        t_outtab         = lt_knb1_new
      EXCEPTIONS
        program_error    = 1
        OTHERS           = 2.



*   Compare old vs. new itab
    zcl_table_comparison_factory=>create_table_comparison( )->compare(
      EXPORTING
        internal_table_new = lt_knb1_new
        internal_table_old = lt_knb1_old
      IMPORTING
        inserted   = lt_knb1_ins
        updated   = lt_knb1_upd
        deleted   = lt_knb1_del ).



*   Display new entries
    ld_gridtitle = 'INSERT: new entries'(ins).
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_structure_name = 'KNB1'
        i_grid_title     = ld_gridtitle
      TABLES
        t_outtab         = lt_knb1_ins
      EXCEPTIONS
        program_error    = 1
        OTHERS           = 2.
*   Display changed entries
    ld_gridtitle = 'UPDATE: changed entries'(upd).
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_structure_name = 'KNB1'
        i_grid_title     = ld_gridtitle
      TABLES
        t_outtab         = lt_knb1_upd
      EXCEPTIONS
        program_error    = 1
        OTHERS           = 2.
*   Display deleted entries
    ld_gridtitle = 'DELETE: deleted entries'(del).
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_structure_name = 'KNB1'
        i_grid_title     = ld_gridtitle
      TABLES
        t_outtab         = lt_knb1_del
      EXCEPTIONS
        program_error    = 1
        OTHERS           = 2.

  ENDMETHOD.
endclass.


START-OF-SELECTION.

  new lcl_interactive_test( )->interactive_test(
    id_insert = p_ins
    id_update = p_upd
    id_delete = p_del ).

END-OF-SELECTION.
