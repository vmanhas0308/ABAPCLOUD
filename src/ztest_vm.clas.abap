CLASS ztest_vm DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    METHODS: delete_data.
    METHODS: run.
ENDCLASS.

CLASS ztest_vm IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.
    run( ).
  ENDMETHOD.

  METHOD run.
    delete_data( ).
  ENDMETHOD.

  METHOD delete_data.
    DELETE FROM zfile_table.
    COMMIT WORK.
  ENDMETHOD.

ENDCLASS.

