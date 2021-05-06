CLASS zcl_table_comparison DEFINITION
  PUBLIC
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_table_comparison_factory.

  PUBLIC SECTION.
    INTERFACES: ZIF_TABLE_COMPARISON.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_table_comparison IMPLEMENTATION.


  METHOD zif_table_comparison~compare.

* define local data
    DATA:
      ld_struc_old TYPE string,
      ld_struc_new TYPE string,
      ld_tabname   TYPE tabname,
      ldo_new      TYPE REF TO data,
      ldo_old      TYPE REF TO data,
*
      ldo_struc    TYPE REF TO data,
      ldo_di_struc TYPE REF TO data,
      ldo_di_new   TYPE REF TO data,
      ldo_di_old   TYPE REF TO data,
*
      ldo_insert   TYPE REF TO data,
      ldo_update   TYPE REF TO data,
      ldo_delete   TYPE REF TO data.


    TYPES: BEGIN OF ty_s_di.
    TYPES:   chind    TYPE bu_chind.  " change indicator
    TYPES: END OF ty_s_di.
    DATA:
      ls_chind           TYPE ty_s_di.


*    TYPES: BEGIN OF ty_s_di.
*    TYPES:   chind    TYPE bu_chind.  " change indicator
*    TYPES: END OF ty_s_di.
*    DATA:
*      ls_chind           TYPE ty_s_di.


    DATA:
      lt_components    TYPE cl_abap_structdescr=>component_table,
      lt_components_di TYPE cl_abap_structdescr=>component_table,
      ls_component     LIKE LINE OF lt_components,
      lo_tab_new       TYPE REF TO cl_abap_tabledescr,
      lo_tab_old       TYPE REF TO cl_abap_tabledescr,
      lo_tab_di        TYPE REF TO cl_abap_tabledescr,
      lo_strucdescr    TYPE REF TO cl_abap_structdescr,
      lo_typedescr     TYPE REF TO cl_abap_typedescr,
      lt_tabkey        TYPE abap_keydescr_tab.

    FIELD-SYMBOLS:
      <ld_chind>  TYPE bu_chind,
*
      <ls_struc>  TYPE any,
      <ls_di>     TYPE any,
*
      <lt_old_di> TYPE table,
      <lt_new_di> TYPE table.



*   Get RTTI of new itab
    lo_tab_new ?= cl_abap_typedescr=>describe_by_data( it_itab_new ).
    lo_strucdescr ?= lo_tab_new->get_table_line_type( ).
    ld_struc_new = lo_strucdescr->get_relative_name( ).
    ld_tabname = ld_struc_new.  " type conversion for function module

*   Get RTTI of old itab
    lo_tab_old ?= cl_abap_typedescr=>describe_by_data( it_itab_old ).
    lo_strucdescr ?= lo_tab_old->get_table_line_type( ).
    ld_struc_old = lo_strucdescr->get_relative_name( ).

    IF ( ld_struc_old NE ld_struc_new ).
*      RAISE error.  " itab's have different line types
      ##TODO "message text export
      RAISE EXCEPTION TYPE zcx_table_comparison.
    ENDIF.


*   Create variable having line type of new/old itab
    CREATE DATA ldo_struc TYPE HANDLE lo_strucdescr.
    ASSIGN ldo_struc->* TO <ls_struc>.  " line type of new/old itab


*   Get components of new/old itab and add component CHIND
    lt_components_di = lo_strucdescr->get_components( ).

    REFRESH: lt_components.
    CLEAR: ls_chind.
    lo_strucdescr ?= cl_abap_typedescr=>describe_by_data( ls_chind ).
    lt_components  = lo_strucdescr->get_components( ).
    APPEND LINES OF lt_components TO lt_components_di.



*   Create variable having line type of new/old itab with additional
*   change indicator field & corresponding itab
    lo_strucdescr = cl_abap_structdescr=>create( lt_components_di ).
    lo_tab_di     = cl_abap_tabledescr=>create( lo_strucdescr ).

    CREATE DATA ldo_di_struc  TYPE HANDLE lo_strucdescr.
    CREATE DATA ldo_di_new    TYPE HANDLE lo_tab_di.
    CREATE DATA ldo_di_old    TYPE HANDLE lo_tab_di.
*
    ASSIGN ldo_di_struc->*  TO <ls_di>.
    ASSIGN ldo_di_new->*    TO <lt_new_di>.
    ASSIGN ldo_di_old->*    TO <lt_old_di>.



*   Shuffle data from new itab into corresponding itab
*   with change indicator (field CHIND)
    LOOP AT it_itab_new INTO <ls_struc>.
      MOVE-CORRESPONDING <ls_struc> TO <ls_di>.
      APPEND <ls_di> TO <lt_new_di>.
    ENDLOOP.
*   Shuffle data from old itab into corresponding itab
*   with change indicator (field CHIND)
    LOOP AT it_itab_old INTO <ls_struc>.
      MOVE-CORRESPONDING <ls_struc> TO <ls_di>.
      APPEND <ls_di> TO <lt_old_di>.
    ENDLOOP.

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
        tablename              = ld_tabname
*   IMPORTING
*       RESULT                 =
      TABLES
        table_new              = <lt_new_di>
        table_old              = <lt_old_di>
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
*   of the records
    LOOP AT <lt_new_di> INTO <ls_di>.  " new itab -> INSERT & UPDATE
      MOVE-CORRESPONDING <ls_di> TO <ls_struc>.
      ASSIGN COMPONENT 'CHIND' OF STRUCTURE <ls_di> TO <ld_chind>.

      CASE <ld_chind>.
*       New entry (INSERT)
        WHEN 'I'.
          APPEND <ls_struc> TO et_insert.

*       Modified entry (UPDATE)
        WHEN 'U'.
          APPEND <ls_struc> TO et_update.

*       should not occur
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

    ENDLOOP.

    LOOP AT <lt_old_di> INTO <ls_di>.  " old itab -> DELETE
      MOVE-CORRESPONDING <ls_di> TO <ls_struc>.
      ASSIGN COMPONENT 'CHIND' OF STRUCTURE <ls_di> TO <ld_chind>.

      CASE <ld_chind>.
*       Delete entry (DELETE)
        WHEN 'D'.
          APPEND <ls_struc> TO et_delete.

*       Modified entry (old values)
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

    ENDLOOP.


  ENDMETHOD.                    "compare

ENDCLASS.
