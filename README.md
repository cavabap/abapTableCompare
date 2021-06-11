# abapTableCompare
Compares two tables and reports which lines are inserted, deleted or changed.

Target Platform ABAP 7.4+

# Installation

Download this project to your SAP Server with ABAPGit.

# Example
```
    TYPES t_itab TYPE STANDARD TABLE OF scarr WITH KEY carrid.
    DATA scarr_old TYPE t_itab.
    DATA scarr_new TYPE t_itab.

    DATA: inserts TYPE t_itab,
          updates TYPE t_itab,
          deletes TYPE t_itab.
          
    zcl_table_comparison_factory=>create_table_comparison( )->compare(
      EXPORTING
        internal_table_new = scarr_new
        internal_table_old = scarr_old
      IMPORTING
        inserted   = inserts
        updated   = updates
        deleted   = deletes ).
```

based on a blog post from Uwe Schieferstein (21.06.2007) https://wiki.scn.sap.com/wiki/display/Snippets/Comparing+Two+Internal+Tables+-+A+Generic+Approach
