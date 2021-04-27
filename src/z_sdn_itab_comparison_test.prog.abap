*&---------------------------------------------------------------------*
*& Report Z_SDN_ITAB_COMPARISON_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_SDN_ITAB_COMPARISON_TEST.


PARAMETERS:
  p_ins    AS CHECKBOX  DEFAULT ' ',
  p_upd    AS CHECKBOX  DEFAULT ' ',
  p_del    AS CHECKBOX  DEFAULT ' '.

" NOTE: Mark the checkboxes in order to simulate a scenario.

START-OF-SELECTION.


*The static method TEST generates simplified PBO data and PAI data for
*demonstrating the functionality of method COMPARE. PBO data and PAI
*data as well as the results are displayed as ALV lists.
*The interface of method TEST contains three IMPORTING parameters
*(flags) for simulating PAI data that contain new, modified or deleted
*entries and any combination thereof.
  zcl_table_comparison=>interactive_test( id_insert = p_ins
                                               id_update = p_upd
                                               id_delete = p_del ).


END-OF-SELECTION.

**The following coding shows how to call method COMPARE. Sorting of the PAI and PBO data is crucial for correct comparison.
*
*DATA:
*  gt_pbo      TYPE STANDARD TABLE OF knb1,  " PBO data (old)
*  gt_pai      TYPE STANDARD TABLE OF knb1,  " PAI data (new)
*  gt_insert   TYPE STANDARD TABLE OF knb1,  " new entries
*  gt_update   TYPE STANDARD TABLE OF knb1,  " changed entries
*  gt_delete   TYPE STANDARD TABLE OF knb1.  " deleted entries
*" NOTE: all itab's have the same line type.
*" No need to define additional structures for data comparison
** Sorting is crucial for comparison!!!
*    SORT gt_pai BY kunnr bukrs.
*    SORT gt_pbo BY kunnr bukrs.
*  CALL METHOD zcl_sdn_itab_comparison=>compare
*    EXPORTING
*      it_itab_new  = gt_pai
*      it_itab_old   = gt_pbo
*    IMPORTING
*      et_insert     = gt_insert
*      et_update   = gt_update
*      et_delete    = gt_delete.
