
CLASS ltc_function_tests DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    TYPES t_itab TYPE STANDARD TABLE OF scarr WITH KEY carrid.

    DATA scarr_old TYPE t_itab.
    DATA scarr_new TYPE t_itab.

    DATA: inserts TYPE t_itab,
          updates TYPE t_itab,
          deletes TYPE t_itab.

    METHODS no_change FOR TESTING RAISING cx_static_check.
    METHODS delete_row FOR TESTING RAISING cx_static_check.
    METHODS add_row FOR TESTING RAISING cx_static_check.
    METHODS change_row FOR TESTING RAISING cx_static_check.
    METHODS unsorted_fails FOR TESTING RAISING cx_static_check.
    METHODS multiple_changes FOR TESTING RAISING cx_static_check.

    METHODS deletes_should_be IMPORTING i_scarr TYPE t_itab OPTIONAL.
    METHODS changes_should_be IMPORTING i_scarr TYPE t_itab OPTIONAL.
    METHODS inserts_should_be IMPORTING i_scarr TYPE t_itab OPTIONAL.

    METHODS compare_tables RAISING zcx_table_comparison.

ENDCLASS.


CLASS ltc_function_tests IMPLEMENTATION.

  METHOD no_change.

    scarr_old = VALUE #( ( |   AA| ) ).
    scarr_new = VALUE #( ( |   AA| ) ).

    compare_tables( ).

    inserts_should_be( ).
    changes_should_be( ).
    deletes_should_be( ).

  ENDMETHOD.

  METHOD add_row.

    scarr_old = VALUE #( ( |   AA| ) ).
    scarr_new = VALUE #( ( |   AA| ) ( |   BB| ) ).

    compare_tables( ).

    inserts_should_be( VALUE #( ( |   BB| ) ) ).
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

    scarr_old = VALUE #( ( |   AA| ) ).
    scarr_new = VALUE #( ).

    compare_tables( ).

    inserts_should_be( ).
    changes_should_be( ).
    deletes_should_be( VALUE #( ( |   AA| ) ) ).

  ENDMETHOD.

  METHOD unsorted_fails.

    scarr_old = VALUE #( ( |   BB| ) ( |   AA| ) ).

    TRY.
        compare_tables( ).
        cl_abap_unit_assert=>fail( msg = 'Todo: Raise exception for unsorted tables'
                                   level = if_aunit_constants=>tolerable ).
      CATCH zcx_table_comparison ##NO_HANDLER.
        "as expected.
    ENDTRY.

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

    zcl_table_comparison_factory=>create_table_comparison(
      )->compare(
           EXPORTING
             it_itab_new = scarr_new
             it_itab_old = scarr_old
           IMPORTING
             et_insert   = inserts
             et_update   = updates
             et_delete   = deletes ).

  ENDMETHOD.


  METHOD inserts_should_be.
    cl_abap_unit_assert=>assert_equals( act = inserts
                                        exp = i_scarr
                                        msg = `Inserts not as expected` ).
  ENDMETHOD.


  METHOD changes_should_be.
    cl_abap_unit_assert=>assert_equals( act = updates
                                        exp = i_scarr
                                        msg = `Changes not as expected` ).
  ENDMETHOD.


  METHOD deletes_should_be.
    cl_abap_unit_assert=>assert_equals( act = deletes
                                        exp = i_scarr
                                        msg = `Deletes not as expected` ).
  ENDMETHOD.


ENDCLASS.
