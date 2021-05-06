class ZCL_TABLE_COMPARISON_FACTORY definition
  public
  final
  create public .

public section.
  CLASS-METHODS create_table_comparison RETURNING VALUE(r_table_comparison) TYPE REF TO zif_table_comparison.
protected section.
private section.
ENDCLASS.



CLASS ZCL_TABLE_COMPARISON_FACTORY IMPLEMENTATION.
  METHOD create_table_comparison.
    r_table_comparison = new zcl_table_comparison( ).
  ENDMETHOD.

ENDCLASS.
