*"* use this source file for your ABAP unit test classes

class ltc_compare_t001 definition create public FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  public section.
  protected section.
  private section.

    METHODS setup.
    METHODS one_item_inserted FOR TESTING RAISING cx_static_check.
    METHODS one_item_updated FOR TESTING RAISING cx_static_check.
    METHODS one_item_deleted FOR TESTING RAISING cx_static_check.
    METHODS two_items_inserted_one_changed FOR TESTING RAISING cx_static_check.

    TYPES: variant_variable_table TYPE STANDARD TABLE OF tvarvc with EMPTY KEY.
    DATA:
      variant_variables_old  TYPE variant_variable_table,
      variant_variables_new  TYPE variant_variable_table,
      variant_variables_inserted  TYPE variant_variable_table,
      variant_variables_updated  TYPE variant_variable_table,
      variant_variables_deleted  TYPE variant_variable_table,
      cut TYPE REF TO zif_table_comparison.
endclass.

class ltc_compare_t001 implementation.

  METHOD setup.

*   Create 4 sample entries
    DO 4 TIMES.
      DATA(variant_variable) = VALUE tvarvc( name = 1000 * sy-index + sy-index opti = 'I' ).
      APPEND variant_variable TO variant_variables_new.
      APPEND variant_variable TO variant_variables_old.
    ENDDO.

*   Sorting is (currently) crucial for comparison!!!
    SORT variant_variables_new BY name type numb.
    SORT variant_variables_old BY name type numb.
    cut = zcl_table_comparison_factory=>create_table_comparision( ).
  ENDMETHOD.

  METHOD one_item_inserted.

*   Delete first entry from old itab => first entry of new itab
*   should be inserted (name = '1001' -> CHIND = 'I').
    DELETE variant_variables_old INDEX 1.

    cut->compare(
      EXPORTING
        it_itab_new         = variant_variables_new
        it_itab_old         = variant_variables_old
      IMPORTING
        et_insert           = variant_variables_inserted
        et_update           = variant_variables_updated
        et_delete           = variant_variables_deleted ).

    cl_abap_unit_assert=>assert_initial( variant_variables_updated ).
    cl_abap_unit_assert=>assert_initial( variant_variables_deleted ).
    cl_abap_unit_assert=>assert_equals( act = variant_variables_inserted exp = VALUE variant_variable_table( ( variant_variables_new[ 1 ] ) ) ).

  ENDMETHOD.

  METHOD two_items_inserted_one_changed.

*   Delete first entry from old itab => first entry of new itab
*   should be inserted (name = '1001' -> CHIND = 'I').
    DELETE variant_variables_old INDEX 3.
    DELETE variant_variables_old INDEX 1.
    variant_variables_new[ 2 ]-low = 'Hello World'.

    cut->compare(
      EXPORTING
        it_itab_new         = variant_variables_new
        it_itab_old         = variant_variables_old
      IMPORTING
        et_insert           = variant_variables_inserted
        et_update           = variant_variables_updated
        et_delete           = variant_variables_deleted ).

    cl_abap_unit_assert=>assert_equals( act = variant_variables_updated exp = VALUE variant_variable_table( ( variant_variables_new[ 2 ] ) ) ).
    cl_abap_unit_assert=>assert_initial( variant_variables_deleted ).
    cl_abap_unit_assert=>assert_equals( act = variant_variables_inserted exp = VALUE variant_variable_table( ( variant_variables_new[ 1 ] ) ( variant_variables_new[ 3 ] ) ) ).

  ENDMETHOD.


  METHOD one_item_updated.
*   Modify second entry of new itab =>
*   should be updated (sign = 'E' -> CHIND = 'U')
    variant_variables_new[ 2 ]-sign = 'E'.

    cut->compare(
      EXPORTING
        it_itab_new         = variant_variables_new
        it_itab_old         = variant_variables_old
      IMPORTING
        et_insert           = variant_variables_inserted
        et_update           = variant_variables_updated
        et_delete           = variant_variables_deleted ).

    cl_abap_unit_assert=>assert_initial( variant_variables_inserted ).
    cl_abap_unit_assert=>assert_initial( variant_variables_deleted ).
    cl_abap_unit_assert=>assert_equals( act = variant_variables_updated exp = VALUE variant_variable_table( ( variant_variables_new[ 2 ] ) ) ).

  ENDMETHOD.



  METHOD one_item_deleted.
**   Delete third entry from new itab =>
**   should be deleted (name = '3003' -> CHIND = 'D').
    DELETE variant_variables_new INDEX 3.

    cut->compare(
      EXPORTING
        it_itab_new         = variant_variables_new
        it_itab_old         = variant_variables_old
      IMPORTING
        et_insert           = variant_variables_inserted
        et_update           = variant_variables_updated
        et_delete           = variant_variables_deleted ).

    cl_abap_unit_assert=>assert_initial( variant_variables_inserted ).
    cl_abap_unit_assert=>assert_initial( variant_variables_updated ).
    cl_abap_unit_assert=>assert_equals( act = variant_variables_deleted exp = VALUE variant_variable_table( ( variant_variables_old[ 3 ] ) ) ).

  ENDMETHOD.


endclass.
