*"* use this source file for your ABAP unit test classes

class ltc_compare_t001 definition create public FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  public section.
  protected section.
  private section.

    METHODS setup.
    METHODS one_item_inserted FOR TESTING RAISING cx_static_check.
    METHODS one_item_updated FOR TESTING RAISING cx_static_check.
    METHODS one_item_deleted FOR TESTING RAISING cx_static_check.

    TYPES: company_code_table TYPE STANDARD TABLE OF t001 with EMPTY KEY.
    DATA:
      company_codes_old  TYPE company_code_table,
      company_codes_new  TYPE company_code_table,
      company_codes_inserted  TYPE company_code_table,
      company_codes_updated  TYPE company_code_table,
      company_codes_deleted  TYPE company_code_table,
      cut TYPE REF TO zif_table_comparison.
endclass.

class ltc_compare_t001 implementation.

  METHOD setup.

*   Create 4 sample entries
    DO 4 TIMES.
      DATA(company) = VALUE t001( bukrs = 1000 * sy-index + sy-index rcomp = abap_false ).
      APPEND company TO company_codes_new.
      APPEND company TO company_codes_old.
    ENDDO.

*   Sorting is (currently) crucial for comparison!!!
    SORT company_codes_new BY bukrs.
    SORT company_codes_old BY bukrs.
    cut = zcl_table_comparison_factory=>create_table_comparision( ).
  ENDMETHOD.

  METHOD one_item_inserted.

*   Delete first entry from old itab => first entry of new itab
*   should be inserted (company = '1001' -> CHIND = 'I').
    DELETE company_codes_old INDEX 1.

    cut->compare(
      EXPORTING
        it_itab_new         = company_codes_new
        it_itab_old         = company_codes_old
      IMPORTING
        et_insert           = company_codes_inserted
        et_update           = company_codes_updated
        et_delete           = company_codes_deleted
      EXCEPTIONS
        error               = 1
        function_call_error = 2
        others              = 3
    ).
    ASSERT SY-SUBRC = 0.

    cl_abap_unit_assert=>assert_initial( company_codes_updated ).
    cl_abap_unit_assert=>assert_initial( company_codes_deleted ).
    cl_abap_unit_assert=>assert_equals( act = company_codes_inserted exp = VALUE company_code_table( ( company_codes_new[ 1 ] ) ) ).

  ENDMETHOD.


  METHOD one_item_updated.
*   Modify second entry of new itab =>
*   should be updated (customer = '2' -> CHIND = 'U')
    company_codes_new[ 2 ]-rcomp = abap_true.

    cut->compare(
      EXPORTING
        it_itab_new         = company_codes_new
        it_itab_old         = company_codes_old
      IMPORTING
        et_insert           = company_codes_inserted
        et_update           = company_codes_updated
        et_delete           = company_codes_deleted
      EXCEPTIONS
        error               = 1
        function_call_error = 2
        others              = 3
    ).
    ASSERT SY-SUBRC = 0.

    cl_abap_unit_assert=>assert_initial( company_codes_inserted ).
    cl_abap_unit_assert=>assert_initial( company_codes_deleted ).
    cl_abap_unit_assert=>assert_equals( act = company_codes_updated exp = VALUE company_code_table( ( company_codes_new[ 2 ] ) ) ).

  ENDMETHOD.



  METHOD one_item_deleted.
**   Delete third entry from new itab =>
**   should be deleted (customer = '3' -> CHIND = 'D').
      DELETE company_codes_new INDEX 3.

    cut->compare(
      EXPORTING
        it_itab_new         = company_codes_new
        it_itab_old         = company_codes_old
      IMPORTING
        et_insert           = company_codes_inserted
        et_update           = company_codes_updated
        et_delete           = company_codes_deleted
      EXCEPTIONS
        error               = 1
        function_call_error = 2
        others              = 3
    ).
    ASSERT SY-SUBRC = 0.

    cl_abap_unit_assert=>assert_initial( company_codes_inserted ).
    cl_abap_unit_assert=>assert_initial( company_codes_updated ).
    cl_abap_unit_assert=>assert_equals( act = company_codes_deleted exp = VALUE company_code_table( ( company_codes_old[ 3 ] ) ) ).

  ENDMETHOD.


endclass.
