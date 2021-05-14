CLASS zcl_table_comparison DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_table_comparison_factory.

  PUBLIC SECTION.
    INTERFACES: zif_table_comparison.


  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_change_indicator_column_name TYPE string VALUE 'CHIND' ##NO_TEXT.
    METHODS get_sort_order_from_table_keys
      IMPORTING
        database_table_description_new TYPE REF TO cl_abap_tabledescr
      RETURNING
        VALUE(r_result)         TYPE abap_sortorder_tab.
    METHODS get_deleted_lines
      IMPORTING
        itab_old_with_change_ind TYPE table
      EXPORTING
        deleted_lines            TYPE table.
    METHODS get_inserted_and_update_lines
      IMPORTING
        itab_new_with_change_ind TYPE table
      EXPORTING
        et_insert                TYPE table
        et_update                TYPE table.
ENDCLASS.



CLASS zcl_table_comparison IMPLEMENTATION.


  METHOD zif_table_comparison~compare.

    TYPES: BEGIN OF ty_change_indicator,
        chind    TYPE bu_chind,  " change indicator
    END OF ty_change_indicator.

    DATA:
      change_indicator           TYPE ty_change_indicator,
      structure_name_old TYPE string,
      structure_name_new TYPE string,
      table_name         TYPE tabname,
      itab_new_with_change_ind_ref         TYPE REF TO data,
      itab_old_with_change_ind_ref         TYPE REF TO data,
      table_description_new TYPE REF TO cl_abap_tabledescr,
      table_description_old TYPE REF TO cl_abap_tabledescr,
      internal_table_discription     TYPE REF TO cl_abap_tabledescr,
      structure_description TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS:
      <itab_old_with_change_ind>   TYPE table,
      <itab_new_with_change_ind>   TYPE table.

*   Get RTTI of new itab
    table_description_new ?= cl_abap_typedescr=>describe_by_data( internal_table_new ).
    structure_description ?= table_description_new->get_table_line_type( ).
    structure_name_new = structure_description->get_relative_name( ).
    table_name = structure_name_new.  " type conversion for function module

*   Get RTTI of old itab
    table_description_old ?= cl_abap_typedescr=>describe_by_data( internal_table_old ).
    structure_description ?= table_description_old->get_table_line_type( ).
    structure_name_old = structure_description->get_relative_name( ).

    IF structure_name_old <> structure_name_new.
*      RAISE error.  " itab's have different line types
      ##TODO "message text export
      RAISE EXCEPTION TYPE zcx_table_comparison.
    ENDIF.

*   Get components of new/old itab and add component CHIND
    DATA(all_fields) = structure_description->get_components( ).
    structure_description ?= cl_abap_typedescr=>describe_by_data( change_indicator ).
    DATA(change_indicator_components)  = structure_description->get_components( ).
    APPEND LINES OF change_indicator_components TO all_fields.

*   Create variable having line type of new/old itab with additional change indicator field & corresponding itab
    structure_description = cl_abap_structdescr=>create( all_fields ).
    internal_table_discription     = cl_abap_tabledescr=>create( structure_description ).

    CREATE DATA itab_new_with_change_ind_ref    TYPE HANDLE internal_table_discription.
    CREATE DATA itab_old_with_change_ind_ref    TYPE HANDLE internal_table_discription.
*
    ASSIGN itab_new_with_change_ind_ref->*    TO <itab_new_with_change_ind>.
    ASSIGN itab_old_with_change_ind_ref->*    TO <itab_old_with_change_ind>.

*   Shuffle data from new itab into corresponding itab with change indicator (field CHIND)
    MOVE-CORRESPONDING internal_table_new TO <itab_new_with_change_ind>.
*   Shuffle data from old itab into corresponding itab with change indicator (field CHIND)
    MOVE-CORRESPONDING internal_table_old TO <itab_old_with_change_ind>.


    DATA(sort_order) = get_sort_order_from_table_keys( table_description_new ).
    SORT <itab_old_with_change_ind> BY (sort_order).
    SORT <itab_new_with_change_ind> BY (sort_order).

* NOTE: If check_indicator = ' ' then the itab's are condensed meaning
*       that identical entries are removed from both itab's.
* Remaining entries in table_new have the following change indicators:
* - 'I' = INSERT, i.e. a new entry
* - 'U' = UPDATE, i.e. a modified entry
*
* Remaining entries in table_old have the following change indicators:
* - 'D' = DELETE, i.e. a deleted entry
* - ' ' = has a corresponding entry in table_new with CHIND = 'U'
    CALL FUNCTION 'CHANGEDOCUMENT_PREPARE_TABLES'
      EXPORTING
        check_indicator        = abap_false
        tablename              = table_name
*   IMPORTING
*       RESULT                 =
      TABLES
        table_new              = <itab_new_with_change_ind>
        table_old              = <itab_old_with_change_ind>
      EXCEPTIONS
        nametab_error          = 1
        wrong_structure_length = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
*      RAISING function_call_error.
      ##TODO "message text export - however these errors won't occur anymore, if I move away from 'CHANGEDOCUMENT_PREPARE_TABLES'
      RAISE EXCEPTION TYPE zcx_table_comparison.
    ENDIF.

*   Fill the output itab's depending on the change indicator
    get_inserted_and_update_lines(
      EXPORTING
        itab_new_with_change_ind = <itab_new_with_change_ind>
      IMPORTING
        et_insert = inserted
        et_update = updated ).
    get_deleted_lines(
      EXPORTING
        itab_old_with_change_ind = <itab_old_with_change_ind>
      IMPORTING
        deleted_lines = deleted ).

  ENDMETHOD.

  METHOD get_inserted_and_update_lines.

    FIELD-SYMBOLS:
              <change_indictator>  TYPE bu_chind.
    LOOP AT itab_new_with_change_ind ASSIGNING FIELD-SYMBOL(<table_line_with_change_ind2>).  " new itab -> INSERT & UPDATE
      ASSIGN COMPONENT c_change_indicator_column_name OF STRUCTURE <table_line_with_change_ind2> TO <change_indictator>.

      CASE <change_indictator>.
*       New entry (INSERT)
        WHEN 'I'.

          APPEND INITIAL LINE TO et_insert ASSIGNING FIELD-SYMBOL(<inserted_line>).
          MOVE-CORRESPONDING <table_line_with_change_ind2> TO <inserted_line>.

*       Modified entry (UPDATE)
        WHEN 'U'.
          APPEND INITIAL LINE TO et_update ASSIGNING FIELD-SYMBOL(<updated_line>).
          MOVE-CORRESPONDING <table_line_with_change_ind2> TO <updated_line>.

*       should not occur
        WHEN OTHERS.
          ASSERT 1 = 0.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_deleted_lines.

    FIELD-SYMBOLS:
          <change_indictator>  TYPE bu_chind.
    LOOP AT itab_old_with_change_ind ASSIGNING FIELD-SYMBOL(<table_line_with_change_ind>).  " old itab -> DELETE
      ASSIGN COMPONENT c_change_indicator_column_name OF STRUCTURE <table_line_with_change_ind> TO <change_indictator>.

      CASE <change_indictator>.
*       Delete entry (DELETE)
        WHEN 'D'.
          APPEND INITIAL LINE TO deleted_lines ASSIGNING FIELD-SYMBOL(<delete_line>).
          MOVE-CORRESPONDING <table_line_with_change_ind> TO <delete_line>.

*       Modified entry (UPDATE) (already handled above)
        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_sort_order_from_table_keys.

    DATA(keys) = database_table_description_new->get_keys( ).
    LOOP AT keys ASSIGNING FIELD-SYMBOL(<key>) WHERE is_primary = abap_true.
      r_result = VALUE #( BASE r_result FOR key_component IN <key>-components FOR key IN keys ( name = key_component-name ) ).
    ENDLOOP.

  ENDMETHOD.



  METHOD zif_table_comparison~compare_with_database.
    DATA internal_table_description TYPE REF TO cl_abap_tabledescr.
    DATA internal_table_db_ref         TYPE REF TO data.
    DATA structure_description TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS:
      <internal_table_db>   TYPE table.

   internal_table_description ?= cl_abap_typedescr=>describe_by_data( internal_table ).

**   Get RTTI of database table
    structure_description ?= internal_table_description->get_table_line_type( ).
    DATA(table_name) = structure_description->get_relative_name( ).

    CREATE DATA internal_table_db_ref TYPE HANDLE internal_table_description.

    ASSIGN internal_table_db_ref->* TO <internal_table_db>.

    SELECT * FROM (table_name) INTO TABLE @<internal_table_db>. ##TODO "include an optional where clause?

    me->zif_table_comparison~compare(
      EXPORTING
        internal_table_new = internal_table
        internal_table_old = <internal_table_db>
      IMPORTING
        inserted   = inserted
        updated   = updated
        deleted   = deleted ).
  ENDMETHOD.

ENDCLASS.
