
CLASS zcl_dox_api_process DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS c_api_url       TYPE string VALUE 'https://aiservices-trial-dox.cfapps.us10.hana.ondemand.com'.
    CONSTANTS c_api_path      TYPE string VALUE '/document-information-extraction/v1'.
    CONSTANTS c_client_id     TYPE string VALUE 'sb-e86a6e06-803e-4ade-8ec6-69a08cad56ea!b467578|dox-xsuaa-std-trial!b10844'.
    CONSTANTS c_client_secret TYPE string
                              VALUE '4f3e9e11-b6ad-4cf8-82a5-3227d8481a1d$V6YLC2Gmp6kjOxPFtqZ1sGmChTZ7TaF_Ze4kzmK5cCQ='.

    TYPES: BEGIN OF ty_token,
             access_token TYPE string,
             token_type   TYPE string,
             expires_in   TYPE string,
             created_at   TYPE string,
           END OF ty_token.

    TYPES: BEGIN OF ty_data,
             success TYPE string,
             info    TYPE string,
           END OF ty_data.
    TYPES: tt_extracted_data TYPE STANDARD TABLE OF zfile_uploaddt WITH DEFAULT KEY.
    DATA:gt_extracted_data TYPE tt_extracted_data.
    DATA ls_token TYPE ty_token.
    DATA ls_data  TYPE ty_data.
    DATA gv_oauth TYPE string.

    METHODS get_client
      RETURNING VALUE(ro_result) TYPE REF TO if_web_http_client
      RAISING   cx_http_dest_provider_error
                cx_web_http_client_error.

    METHODS authenticate
      RETURNING VALUE(r_authenticated) TYPE abap_bool
      RAISING   cx_http_dest_provider_error.

    METHODS send_file_2_DOXSRV
      IMPORTING i_file_content           TYPE xstring
      RETURNING VALUE(rt_extracted_data) TYPE  tt_extracted_data.


    METHODS post_document
      IMPORTING i_file_content TYPE xstring
      RETURNING VALUE(r_job)   TYPE string.

    METHODS get_job_status
      IMPORTING i_job               TYPE string
      RETURNING VALUE(r_job_status) TYPE string.

ENDCLASS.


CLASS zcl_dox_api_process IMPLEMENTATION.
  METHOD get_client.
    " Step 1: Create HTTP client using destination
    " Create HTTP Client
    TRY.
        DATA(lo_http_destination) = cl_http_destination_provider=>create_by_url( i_url = c_api_url ).
      CATCH cx_http_dest_provider_error INTO DATA(lx_desterror). " TODO: variable is assigned but never used (ABAP cleaner)
        RETURN.
        " handle exception
    ENDTRY.

    TRY.
        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_http_destination ).
      CATCH cx_web_http_client_error INTO DATA(lx_client_error). " TODO: variable is assigned but never used (ABAP cleaner)
        RETURN.
        " handle exception
    ENDTRY.

    " Return the HTTP client instance
    ro_result = lo_http_client.
  ENDMETHOD.

  METHOD authenticate.
**********************NEW LOGIC
    "URL from UAA part of service key
    DATA(lv_url_token) = |https://bde4bf6atrial.authentication.us10.hana.ondemand.com/oauth/token|.

    TRY.
        " Setup request and authorization
        DATA(lo_http_client) = cl_web_http_client_manager=>create_by_http_destination(
            i_destination = cl_http_destination_provider=>create_by_url( i_url = lv_url_token ) ).

        " Execute GET method
        " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
        DATA(lo_request) = lo_http_client->get_http_request(
        )->set_header_field( i_name  = 'Content-Type'
                             i_value = 'application/x-www-form-urlencoded'
        )->set_form_field( i_name  = 'grant_type'
                           i_value = 'client_credentials'
        )->set_form_field( i_name  = 'client_id'
                           i_value = c_client_id
        )->set_form_field( i_name  = 'client_secret'
                           i_value = c_client_secret ).

        DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>post ).
        " Retrieve the status code:
        DATA(lo_status) = lo_response->get_status( ).

        " Checking status code is acceptable
        CASE lo_status-code.
          WHEN 200.

            DATA(lv_response_body) = lo_response->get_text( ).

            " Parse response body
            DATA(lo_json) = NEW /ui2/cl_json( ).
            lo_json->deserialize( EXPORTING json = lv_response_body
                                  CHANGING  data = ls_token ).

            gv_oauth = ls_token-access_token.
            r_authenticated = abap_true.
          WHEN OTHERS.
        ENDCASE.
      CATCH cx_web_http_client_error.
      CATCH cx_http_dest_provider_error.

    ENDTRY.

*********************************************END OF NEW LOGIC
    " Step 1: Get HTTP client instance
*    TRY.
*        DATA(lo_http_client) = get_client( ).
*      CATCH cx_web_http_client_error INTO DATA(lx_client_error). " TODO: variable is assigned but never used (ABAP cleaner)
*        " handle exception
*    ENDTRY.
*
*    " Step 2: Get the HTTP request object
*    DATA(lo_request) = lo_http_client->get_http_request( ).
*
*    " Step 3: Set headers
*    " if_http_header_fields_sap=>request_method
*    lo_request->set_header_field( i_name  = '~request_method'
*                                  i_value = 'POST' ).
*    lo_request->set_header_field( i_name  = 'grant_type'
*                                  i_value = 'client_credentials' ).
*
*    " if_http_header_fields_sap=>request_uri
*    lo_request->set_header_field( i_name  = '~request_uri'
*                                  i_value = '/oauth/token?grant_type=client_credentials' ). " Set only the path, not full URL

*    " Step 5: Execute HTTP request
*    TRY.
*        DATA(lo_response) = lo_http_client->execute( if_web_http_client=>post ).
*      CATCH cx_web_http_client_error.
*        " handle exception
*        r_authenticated = abap_false.
*        RETURN.
*    ENDTRY.
*
*    " Step 6: Check status
*    DATA(lv_status) = lo_response->get_status( ).
*
*    IF lv_status-code = 200.
*
*      " Step 7: Get response text
*      DATA(lv_responsetext) = lo_response->get_text( ).
*
*      " Optional: extract token using simple split (for demo), better: parse JSON
*      DATA(lv_token) = ||.
*      " TODO: variable is assigned but never used (ABAP cleaner)
*      SPLIT lv_responsetext AT '"access_token":"' INTO DATA(lv_garbage) lv_token.
*      " TODO: variable is assigned but never used (ABAP cleaner)
*      SPLIT lv_token AT '"' INTO lv_token DATA(lv_remainder).
*
*      r_authenticated = abap_true.
*      " TODO: variable is assigned but never used (ABAP cleaner)
*      DATA(m_oauth) = lv_token.  " Save token if needed
*      gv_oauth = lv_token. " Save token globally for later use
*
*    ELSE.
*      r_authenticated = abap_false.
*    ENDIF.
  ENDMETHOD.

  METHOD send_file_2_DOXSRV.
    DATA l_job        TYPE string.
    DATA l_status_job TYPE string.

    l_job = post_document( i_file_content ).
    IF l_job IS NOT INITIAL.

      l_status_job = get_job_status( l_job ).

      WHILE l_status_job <> 'DONE' AND l_status_job <> 'FAILED'.
        WAIT UP TO 3 SECONDS.
        l_status_job = get_job_status( l_job ).
        "return extracted data.
        rt_extracted_data = gt_extracted_data.
      ENDWHILE.
    ENDIF.
  ENDMETHOD.

  METHOD post_document.
    " Placeholder: Post document to DOX or other extraction service
    " Step 1: Get HTTP client instance
    DATA: BEGIN OF ls_create_job_response,
            id            TYPE string,
            status        TYPE string,
            processedtime TYPE string,
          END OF ls_create_job_response.
    DATA lv_options             TYPE string.
    DATA lv_content_disposition TYPE string.
    DATA len                    TYPE i.
    DATA:lv_template TYPE string VALUE 'DELNOTE',
         lv_schemaid TYPE string VALUE 'Delivery_Note_Schema'.

    TRY.
        DATA(lo_http_client) = get_client( ).
      CATCH cx_http_dest_provider_error INTO DATA(lx_dest_error). " TODO: variable is assigned but never used (ABAP cleaner)
        " handle exception
      CATCH cx_web_http_client_error INTO DATA(lx_client_error). " TODO: variable is assigned but never used (ABAP cleaner)
        " handle exception
        RETURN.
    ENDTRY.

    " Step 2: Get the HTTP request object
    DATA(lo_request) = lo_http_client->get_http_request( ).

*    " Step 3: Set headers
*    lo_request->set_header_field( i_name  = '~request_method'
*                                  i_value = 'POST' ).

*    lo_request->set_header_field( i_name  = '~request_uri'
*                                  i_value = |{ c_api_path }/document/jobs| ).
    " Set the correct URI
    lo_request->set_uri_path( |{ c_api_path }/document/jobs| ).
    " Use the token obtained from authenticate method
    lo_request->set_header_field( i_name  = 'Authorization'
                                  i_value = |Bearer { gv_oauth }| ).

    lo_request->set_content_type( content_type = 'multipart/form-data' ).

    lo_request->set_formfield_encoding( formfield_encoding = 0 ).

    lo_request->set_header_field( i_name  = 'Accept'
                                  i_value = 'application/json' ).

    " add multi-part request
    DATA(lo_request_part2) = lo_request->add_multipart( ).
    "use Schema ID and Template ID
    lv_options = '{ "clientId": "default", "documentType": "custom", "templateId": "DELNOTE", ' &&
                 '"schemaId": "Delivery_Note_Schema", ' &&
                 '"receivedDate": "2025-07-28"' && '}'.

    "If no schema/template ID then prepare JSON Pay-load for header Item
*    lv_options = '{ "extraction": { "headerFields": [ "deliveryNoteNumber", "purchaseOrderNumber", "deliveryDate" ], "lineItemFields": [ "materialNumber", "quantity", "unitOfMeasure" ] },' &&
*                   '"clientId": "default", "documentType": "custom", "receivedDate": "2025-07-28", "enrichment": { "sender": { "top": 5, "type": ' &&
*                   '"businessEntity", "subtype": "supplier" }, "employee": { "type": "employee" } }}'.
*    lv_options = |\{ "extraction": \{ "headerFields": [ "deliveryNoteNumber", "purchaseOrderNumber", "deliveryDate" ], "lineItemFields": [ "materialNumber", "quantity", "unitOfMeasure" ] \},| &&
*                 |"clientId": "default", "documentType": "Custom", "receivedDate": "2025-07-28", "enrichment": \{ "sender": \{ "top": 5, "type":| &&
*                 |"businessEntity", "subtype": "supplier" \}, "employee": \{ "type": "employee" \} \}\}|.

    lo_request_part2->set_header_field( i_name  = 'Content-Disposition' ##NO_TEXT
                                        i_value = 'form-data; name="options"' ). "; type=application/json
    lo_request_part2->set_header_field(
                                            i_name  = 'Content-Type'
                                            i_value = 'application/json' ).
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lv_value) = lo_request_part2->set_text( i_text = lv_options ).

    "File Part
    DATA(lo_request_part) =  lo_request->add_multipart( ).
    lv_content_disposition = 'form-data; name="file"; filename="delivery_note.pdf'.
    lo_request_part->set_header_field( i_name  = 'Content-Disposition' ##NO_TEXT
                                       i_value = lv_content_disposition ).
    lo_request_part->set_content_type( 'application/pdf' ).

    len = xstrlen( i_file_content ).

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lv_value1) = lo_request_part->set_binary( i_data   = i_file_content
                                                   i_offset = 0
                                                   i_length = len ).

    TRY.
        DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>post ).
      CATCH cx_web_http_client_error INTO lx_client_error.
        " handle exception
    ENDTRY.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(ls_status) = lo_response->get_status( ).
*CATCH cx_web_message_error.
    DATA(lv_response_json) = lo_response->get_text( ).
    /ui2/cl_json=>deserialize( EXPORTING json        = lv_response_json
                                         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                               CHANGING  data        = ls_create_job_response ).

    " Step 6: Check status
    DATA(lv_status) = lo_response->get_status( ).

    IF lv_status-code = 201.
      r_job = ls_create_job_response-id.

      TRY.
          lo_http_client->close( ).
        CATCH cx_web_http_client_error INTO lx_client_error.
          " handle exception
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD get_job_status.
    DATA lv_status_job   TYPE string.
    DATA l_json_response TYPE string.
    DATA lr_data         TYPE REF TO data.

    CLEAR r_job_status.

    " Step 1: Get HTTP client instance
    TRY.
        DATA(lo_http_client) = get_client( ).
      CATCH cx_http_dest_provider_error INTO DATA(lx_dest_error). " TODO: variable is assigned but never used (ABAP cleaner)
        " handle exception
      CATCH cx_web_http_client_error INTO DATA(lx_client_error). " TODO: variable is assigned but never used (ABAP cleaner)
        " handle exception
        RETURN.
    ENDTRY.

    " Step 2: Get the HTTP request object
    DATA(lo_http_request) = lo_http_client->get_http_request( ).
    " Step 3: Set headers
*    lo_http_request->set_header_field( i_name  = '~request_method'
*                                       i_value = 'GET' ).
*
*    lo_http_request->set_header_field( i_name  = '~request_uri'
*                                       i_value = |{ c_api_path }/document/jobs/{ i_job }| ).
    " Set the correct URI
    lo_http_request->set_uri_path( |{ c_api_path }/document/jobs/{ i_job }| ).

*    lo_http_request->set_header_field( i_name  =  if_http_header_fields_sap=>request_method  i_value =  if_http_request=>co_request_method_get ).
*    lo_http_request->set_header_field( i_name  =  if_http_header_fields_sap=>request_uri  i_value =  |{ c_api_path }/document/jobs/{ i_job }| ).
    lo_http_request->set_header_field( i_name  = 'Authorization'
                                       i_value = |Bearer { gv_oauth }| ).

    TRY.
        DATA(lo_response) = lo_http_client->execute( i_method = if_web_http_client=>get ).
      CATCH cx_web_http_client_error INTO lx_client_error.
        " handle exception
    ENDTRY.
    DATA(ls_status) = lo_response->get_status( ).
*CATCH cx_web_message_error.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lv_response_json) = lo_response->get_text( ).
    /ui2/cl_json=>deserialize( EXPORTING json        = lv_response_json
                                         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                               CHANGING  data        = lr_data ).

    IF ls_status-code = '200'.

      IF lv_status_job = 'DONE'.
        " Extract data from the response and set to global table  gt_extracted_data

        r_job_status = lv_status_job.

      ENDIF.
    ELSE.
      r_job_status = 'FAILED'.
    ENDIF.

    TRY.
        lo_http_client->close( ).
      CATCH cx_web_http_client_error.
        " handle exception
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
