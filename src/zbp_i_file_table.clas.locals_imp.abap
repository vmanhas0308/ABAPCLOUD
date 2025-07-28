CLASS lhc_ZI_FILE_TABLE DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR zi_file_table RESULT result.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zi_file_table RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR zi_file_table RESULT result.

    METHODS fields FOR DETERMINE ON MODIFY
      IMPORTING keys FOR zi_file_table~fields.

    METHODS uploaddata FOR MODIFY
      IMPORTING keys FOR ACTION zi_file_table~uploaddata RESULT result.

ENDCLASS.

CLASS lhc_ZI_FILE_TABLE IMPLEMENTATION.
  METHOD get_instance_features.
  ENDMETHOD.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD fields.
*    SELECT SINGLE @abap_true FROM zfile_table
*      WHERE end_user = @sy-uname
*      INTO @DATA(lv_valid).
*
*    IF lv_valid <> abap_true.
*      MODIFY ENTITIES OF zi_file_table IN LOCAL MODE
*      ENTITY zi_file_table
*         CREATE
*          AUTO FILL CID
*           WITH VALUE #( (
*                           %data     = VALUE #( end_user = sy-uname ) ) ).
*
*    ENDIF.

    IF keys[ 1 ]-%is_draft = '01'.

      MODIFY ENTITIES OF zi_file_table IN LOCAL MODE
             ENTITY zi_file_table
             EXECUTE uploaddata

             FROM CORRESPONDING #( keys ) MAPPED DATA(lt_mapped_update)
             REPORTED DATA(lt_reported_update)
             FAILED DATA(lt_failed_update).

    ENDIF.
  ENDMETHOD.

  METHOD uploadData.
*    " CHECK if there exist an entry WITH current logged in username in parent table
*    SELECT SINGLE @abap_true FROM zfile_table
*      WHERE end_user = @sy-uname
*      INTO @DATA(lv_valid).
*
*    " Create one entry, if it does not exist
*    IF lv_valid <> abap_true.
*      MODIFY ENTITIES OF zi_file_table IN LOCAL MODE
*             ENTITY zi_file_table
*             CREATE
*             AUTO FILL CID
*             WITH VALUE #( ( %data = VALUE #( end_user = sy-uname ) ) ).
*    ENDIF.
    " Read the parent instance
    READ ENTITIES OF zi_file_table IN LOCAL MODE
         ENTITY zi_file_table
         ALL FIELDS WITH
         CORRESPONDING #( keys )
         RESULT DATA(lt_inv)
    REPORTED DATA(lt_reported_update)
    FAILED DATA(lt_failed_update).
    " Get attachment value from the instance
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lv_attachment) = lt_inv[ 1 ]-attachment.

    " Pass Attachment data to DOX
    DATA(lo_instance) = NEW zcl_dox_api_process( ).
    TRY.
        IF lo_instance->authenticate( ) = abap_true.

          lo_instance->send_file_2_DOXSRV( lv_attachment ).

        ENDIF.
      CATCH cx_http_dest_provider_error INTO DATA(lo_exception).
      CATCH cx_web_http_client_error INTO DATA(lo_exception_client).
        " Handle the exception if needed
        DATA(lv_text) = lo_exception->get_text( ).
        DATA(lv_text_client) = lo_exception_client->get_text( ).
    ENDTRY.

    "Get extracted data from DOX and update child entity.
  ENDMETHOD.

ENDCLASS.
