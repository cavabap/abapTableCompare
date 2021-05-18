*&---------------------------------------------------------------------*
*& Report Z_SDN_ITAB_COMPARISON_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_SDN_ITAB_COMPARISON_TEST.


"! NOTE: Mark the checkboxes in order to simulate a scenario
PARAMETERS p_insert AS CHECKBOX.

"! NOTE: Mark the checkboxes in order to simulate a scenario
PARAMETERS p_update AS CHECKBOX.

"! NOTE: Mark the checkboxes in order to simulate a scenario
PARAMETERS p_delete AS CHECKBOX.


class lcl_interactive_test definition create public.

  public section.
    "! interactive_test( ) generates simplified PBO data and PAI data for
    "! demonstrating the functionality of method COMPARE. PBO data and PAI
    "! data as well as the results are displayed as ALV lists.
    "! The interface of method contains three IMPORTING parameters
    "! (flags) for simulating PAI data that contain new, modified or deleted
    "! entries and any combination thereof.
    METHODS
      interactive_test
        IMPORTING
          insert_line TYPE abap_bool
          update_line TYPE abap_bool
          delete_line TYPE abap_bool.
  protected section.
  private section.

endclass.

class lcl_interactive_test implementation.

  method interactive_test.
    TYPES: ty_airlines TYPE STANDARD TABLE OF scarr.

    DATA:
      gridtitle TYPE lvc_title,
      airlines_old  TYPE ty_airlines,
      airlines_new  TYPE ty_airlines,
      airlines_inserted  TYPE ty_airlines,
      airlines_updated  TYPE ty_airlines,
      airlines_deleted  TYPE ty_airlines.

    DATA(field_catalog) = VALUE lvc_t_fcat( ).
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'SCARR'
      CHANGING
        ct_fieldcat            = field_catalog
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.

*   Create 4 sample entries
    airlines_old = airlines_new = VALUE #( ( carrid = 'AA' ) ( carrid = 'AB' )  ( carrid = 'LH' ) ( carrid = 'NZ' ) ).


*   Delete first entry from old itab => first entry of new itab
*   should be inserted (carrid = 'AA' -> CHIND = 'I').
    IF ( insert_line = abap_true ).
      DELETE airlines_old INDEX 1.
    ENDIF.

*   Modify second entry of new itab =>
*   should be updated (carrid = 'AB' -> CHIND = 'U')
    IF ( update_line = abap_true ).
      airlines_new[ 2 ]-carrname = 'Air Berlin'.
    ENDIF.

*   Delete third entry from new itab =>
*   should be deleted (carrid = 'LH' -> CHIND = 'D').
    IF ( delete_line = abap_true ).
      DELETE airlines_new INDEX 3.
    ENDIF.
*   NOTE: carrid = 'NZ' is identical in old and new itab => ignored


*   Display "old" itab
    gridtitle = 'Old Internal Table Entries (e.g. PBO)'(old).
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_structure_name = 'KNB1'
        i_grid_title     = gridtitle
        it_fieldcat_lvc  = field_catalog
      TABLES
        t_outtab         = airlines_old
      EXCEPTIONS
        program_error    = 1
        OTHERS           = 2.
*   Display "new" itab
    gridtitle = 'New Internal Table Entries (e.g. PAI)'(new).
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_structure_name = 'KNB1'
        i_grid_title     = gridtitle
        it_fieldcat_lvc  = field_catalog
      TABLES
        t_outtab         = airlines_new
      EXCEPTIONS
        program_error    = 1
        OTHERS           = 2.



*   Compare old vs. new itab
    zcl_table_comparison_factory=>create_table_comparison( )->compare(
      EXPORTING
        internal_table_new = airlines_new
        internal_table_old = airlines_old
      IMPORTING
        inserted   = airlines_inserted
        updated   = airlines_updated
        deleted   = airlines_deleted ).



*   Display new entries
    gridtitle = 'INSERT: new entries'(ins).
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_structure_name = 'KNB1'
        i_grid_title     = gridtitle
        it_fieldcat_lvc  = field_catalog
      TABLES
        t_outtab         = airlines_inserted
      EXCEPTIONS
        program_error    = 1
        OTHERS           = 2.
*   Display changed entries
    gridtitle = 'UPDATE: changed entries'(upd).
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_structure_name = 'KNB1'
        i_grid_title     = gridtitle
        it_fieldcat_lvc  = field_catalog
      TABLES
        t_outtab         = airlines_updated
      EXCEPTIONS
        program_error    = 1
        OTHERS           = 2.
*   Display deleted entries
    gridtitle = 'DELETE: deleted entries'(del).
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_structure_name = 'KNB1'
        i_grid_title     = gridtitle
        it_fieldcat_lvc  = field_catalog
      TABLES
        t_outtab         = airlines_deleted
      EXCEPTIONS
        program_error    = 1
        OTHERS           = 2.

  ENDMETHOD.
endclass.


START-OF-SELECTION.

  new lcl_interactive_test( )->interactive_test(
    insert_line = p_insert
    update_line = p_update
    delete_line = p_delete ).
