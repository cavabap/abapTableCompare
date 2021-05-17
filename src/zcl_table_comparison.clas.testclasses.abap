
CLASS ltc_function_tests DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PROTECTED SECTION.


    TYPES t_itab TYPE STANDARD TABLE OF scarr WITH KEY carrid.
    DATA scarr_old TYPE t_itab.
    DATA scarr_new TYPE t_itab.

    DATA: inserts TYPE t_itab,
          updates TYPE t_itab,
          deletes TYPE t_itab.

    METHODS compare_tables RAISING zcx_table_comparison.
    METHODS fill_client
      CHANGING
        scarr TYPE t_itab.

    METHODS no_change FOR TESTING RAISING cx_static_check.
    METHODS delete_row FOR TESTING RAISING cx_static_check.
    METHODS add_row FOR TESTING RAISING cx_static_check.
    METHODS change_row FOR TESTING RAISING cx_static_check.
    METHODS unsorted_rows_are_the_same FOR TESTING RAISING cx_static_check.
    METHODS multiple_changes FOR TESTING RAISING cx_static_check.

  PRIVATE SECTION.

    METHODS deletes_should_be IMPORTING VALUE(scarr) TYPE t_itab OPTIONAL.
    METHODS changes_should_be IMPORTING VALUE(scarr) TYPE t_itab OPTIONAL.
    METHODS inserts_should_be IMPORTING VALUE(scarr) TYPE t_itab OPTIONAL.
    METHODS deleted_wo_param_then_no_dump FOR TESTING RAISING cx_static_check.
    METHODS inserted_wo_param_then_no_dump FOR TESTING RAISING cx_static_check.
    METHODS updated_wo_param_then_no_dump FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_function_tests IMPLEMENTATION.

  METHOD no_change.

    scarr_old = VALUE #( ( carrid = 'AA' ) ).
    scarr_new = VALUE #( ( carrid = 'AA' ) ).

    compare_tables( ).

    inserts_should_be( ).
    changes_should_be( ).
    deletes_should_be( ).

  ENDMETHOD.

  METHOD add_row.

    scarr_old = VALUE #( ( carrid = 'AA' ) ).
    scarr_new = VALUE #( ( carrid = 'AA' ) ( carrid = 'BB' ) ).

    compare_tables( ).

    inserts_should_be( VALUE #( ( carrid = 'BB' ) ) ).
    changes_should_be( ).
    deletes_should_be( ).

  ENDMETHOD.

  METHOD change_row.

    scarr_old = VALUE #( ( carrid = 'AA' ) ).
    scarr_new = VALUE #( ( carrid = 'AA' carrname = 'ABC' ) ).

    compare_tables( ).

    inserts_should_be( ).
    changes_should_be( VALUE #( ( carrid = 'AA' carrname = 'ABC' ) ) ).
    deletes_should_be( ).

  ENDMETHOD.

  METHOD delete_row.

    scarr_old = VALUE #( ( carrid = 'AA' ) ).
    scarr_new = VALUE #( ).

    compare_tables( ).

    inserts_should_be( ).
    changes_should_be( ).
    deletes_should_be( VALUE #( ( carrid = 'AA' ) ) ).

  ENDMETHOD.

  METHOD unsorted_rows_are_the_same.

    scarr_old = VALUE #( ( carrid = 'AA' ) ( carrid = 'BB' ) ).
    scarr_new = VALUE #( ( carrid = 'BB' ) ( carrid = 'AA' ) ).

    compare_tables( ).

    inserts_should_be( ).
    changes_should_be( ).
    deletes_should_be( ).

  ENDMETHOD.


  METHOD multiple_changes.

    scarr_old = VALUE #( ( carrid = `AA` ) ( carrid = `BB` ) ( carrid = `CC` ) ( carrid = `DD` )                   ( carrid = 'EE' carrname = 'Foo' )                   ).
    scarr_new = VALUE #( ( carrid = `AA` )                   ( carrid = `CC` ) ( carrid = `DD` ) ( carrid = `EA` ) ( carrid = 'EE' carrname = 'Bar' ) ( carrid = `ZZ` ) ).

    compare_tables( ).

    inserts_should_be( VALUE #( ( carrid = `EA` ) ( carrid = `ZZ` ) ) ).
    changes_should_be( VALUE #( ( carrid = `EE` carrname = `Bar` ) ) ).
    deletes_should_be( VALUE #( ( carrid = `BB` ) ) ).

  ENDMETHOD.


  METHOD compare_tables.
    fill_client( CHANGING scarr = scarr_old ).
    fill_client( CHANGING scarr = scarr_new ).

    zcl_table_comparison_factory=>create_table_comparison( )->compare(
      EXPORTING
        internal_table_new = scarr_new
        internal_table_old = scarr_old
      IMPORTING
        inserted   = inserts
        updated   = updates
        deleted   = deletes ).

  ENDMETHOD.


  METHOD fill_client.
    LOOP AT scarr ASSIGNING FIELD-SYMBOL(<scarr_line>).
      <scarr_line>-mandt = sy-mandt.
    ENDLOOP.
  ENDMETHOD.


  METHOD inserts_should_be.
    fill_client( CHANGING scarr = scarr ).
    cl_abap_unit_assert=>assert_equals( act = inserts
                                        exp = scarr
                                        msg = `Inserts not as expected` ).
  ENDMETHOD.


  METHOD changes_should_be.
    fill_client( CHANGING scarr = scarr ).
    cl_abap_unit_assert=>assert_equals( act = updates
                                        exp = scarr
                                        msg = `Changes not as expected` ).
  ENDMETHOD.


  METHOD deletes_should_be.
    fill_client( CHANGING scarr = scarr ).
    cl_abap_unit_assert=>assert_equals( act = deletes
                                        exp = scarr
                                        msg = `Deletes not as expected` ).
  ENDMETHOD.


  METHOD deleted_wo_param_then_no_dump.

    scarr_old = VALUE #( ( carrid = 'AA' ) ).
    scarr_new = VALUE #( ).

    zcl_table_comparison_factory=>create_table_comparison( )->compare(
        EXPORTING
          internal_table_new = scarr_new
          internal_table_old = scarr_old ).

  ENDMETHOD.


  METHOD inserted_wo_param_then_no_dump.

    scarr_old = VALUE #( ).
    scarr_new = VALUE #( ( carrid = 'AA' ) ).

    zcl_table_comparison_factory=>create_table_comparison( )->compare(
        EXPORTING
          internal_table_new = scarr_new
          internal_table_old = scarr_old ).

  ENDMETHOD.

  METHOD updated_wo_param_then_no_dump.

    scarr_old = VALUE #( ( carrid = 'AA' ) ).
    scarr_new = VALUE #( ( carrid = 'AA' carrname = 'Any Airline' ) ).

    zcl_table_comparison_factory=>create_table_comparison( )->compare(
        EXPORTING
          internal_table_new = scarr_new
          internal_table_old = scarr_old ).

  ENDMETHOD.
ENDCLASS.

CLASS ltc_db_compare DEFINITION CREATE PUBLIC FOR TESTING RISK LEVEL HARMLESS DURATION SHORT INHERITING FROM ltc_function_tests.

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS compare_tables REDEFINITION.
  PRIVATE SECTION.

    CLASS-METHODS class_teardown.
    CLASS-METHODS class_setup.
    METHODS teardown.
    CLASS-DATA sql_environment TYPE REF TO if_osql_test_environment.


ENDCLASS.

CLASS ltc_db_compare IMPLEMENTATION.

  METHOD class_setup.
    sql_environment = cl_osql_test_environment=>create(
        i_dependency_list = VALUE #( ( 'SCARR' ) ) ).
    sql_environment->clear_doubles( ).

  ENDMETHOD.

  METHOD class_teardown.
    sql_environment->destroy( ).
  ENDMETHOD.

  METHOD compare_tables.


    fill_client( CHANGING scarr = scarr_old ).
    fill_client( CHANGING scarr = scarr_new ).

    sql_environment->insert_test_data( i_data = scarr_old ).

    zcl_table_comparison_factory=>create_table_comparison( )->compare_with_database(
      EXPORTING
        internal_table = scarr_new
      IMPORTING
        inserted   = inserts
        updated   = updates
        deleted   = deletes ).

  ENDMETHOD.

  METHOD teardown.

    sql_environment->clear_doubles( ).
  ENDMETHOD.

ENDCLASS.
