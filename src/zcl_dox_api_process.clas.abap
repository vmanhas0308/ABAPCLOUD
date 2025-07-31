
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
    TYPES tt_extracted_data TYPE STANDARD TABLE OF zfile_uploaddt WITH DEFAULT KEY.

    DATA gt_extracted_data TYPE tt_extracted_data.
    DATA ls_token          TYPE ty_token.
    DATA ls_data           TYPE ty_data.
    DATA gv_oauth          TYPE string.

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
    " NEW LOGIC
    " URL from UAA part of service key
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

    " END OF NEW LOGIC
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
        " return extracted data.
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

*    " NEW CODE ----------------------------------
*    CONSTANTS c_boundary TYPE string VALUE '----DOCBOUNDARY123456'.
*    CONSTANTS c_ct_opts  TYPE string VALUE 'application/json'.
*    CONSTANTS c_ct_file  TYPE string VALUE 'application/pdf'.
*    TYPES: BEGIN OF ty_options,
*             clientId     TYPE string,
*             documentType TYPE string,
*             schemaName   TYPE string,
*             templateId   TYPE string, " or type d, but for JSON usually string
*             receivedDate TYPE string, " or type d, but for JSON usually string
*           END OF ty_options.
*
*    DATA ls_opts TYPE ty_options.
*    " Prepare JSON payload for options
*    ls_opts = VALUE #( clientId     = 'default'
*                       documentType = 'custom'   " or 'custom'
*                       schemaName   = 'Delivery_Note_Schema'  " required if custom
*                       templateId   = 'detect'
*                       receivedDate = sy-datum ). " or any date you want to set, format as needed
*    DATA lv_opts_json TYPE string.
*    lv_opts_json = /ui2/cl_json=>serialize( data = ls_opts ).
*
*    " Build multipart body
*    DATA(lv_body) = |--{ c_boundary }\r\n| &&
*                     |Content-Disposition: form-data; name="options"\r\n| &&
*                     |Content-Type: { c_ct_opts }\r\n\r\n| &&
*                     |{ lv_opts_json } \r\n| &&
*                     |--{ c_boundary }\r\n| &&
*                     |Content-Disposition: form-data; name="file"; filename="delivery_note.pdf"\r\n| &&
*                     |Content-Type: { c_ct_file }\r\n\r\n| &&
*                     |{  i_file_content }\r\n| &&
*                     |--{ c_boundary }--|.
*
**    " Setup HTTP destination
**    DATA(lo_dest) = cl_http_destination_provider=>create_by_url(
**                       i_url = |{ gv_base_url }/document-information-extraction/v1/document/jobs|
**                     ).
**    DATA lo_client = cl_web_http_client_manager=>create_by_http_destination( lo_dest ).
**
**    lo_http_client->get_http_request( )->set_method( if_web_http_request=>co_request_method_post ).
*    lo_http_client->get_http_request( )->set_uri_path( |{ c_api_path }/document/jobs| ).
*    lo_http_client->get_http_request( )->set_header_field( i_name  = 'Authorization'
*                                                           i_value = |Bearer { gv_oauth }| ).
*    lo_http_client->get_http_request( )->set_header_field( i_name  = 'Accept'
*                                                           i_value = 'application/json' ).
*    lo_http_client->get_http_request( )->set_header_field( i_name  = 'Content-Type'
*                                                           i_value = |multipart/form-data; boundary={ c_boundary }| ).
*
*    lo_http_client->get_http_request( )->set_text( lv_body ).
*
*    TRY.
*        DATA(lo_response) = lo_http_client->execute( if_web_http_client=>post ).
*      CATCH cx_web_http_client_error.
*        " handle exception
*    ENDTRY.
*
*    DATA(lv_status) = lo_response->get_status( ).
*
*    IF lv_status-code = 202 OR lv_status-code = 201.
*      " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
*      DATA(lv_response_text) = lo_response->get_text( ).
*      " Parse JSON to extract job id
**      DATA(ls_resp) TYPE your_response_struct.
**      CALL METHOD /ui2/cl_json=>deserialize
**        EXPORTING
**          json = lv_response_text
**        CHANGING
**          data = ls_resp.
**      r_job = ls_resp-id.
**      r_success = abap_true.
**    ELSE.
**      DATA(lv_error) = lo_response->get_text( ).
**      " Log or raise for diagnostics
**      r_success = abap_false.
*    ENDIF.
*
*    TRY.
*        lo_http_client->close( ).
*      CATCH cx_web_http_client_error.
*        " handle exception
*    ENDTRY.

*******************************NEW CODE **********************************
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
    " use Schema Name and Template ID
    lv_options = |\{ "clientId": "default", "documentType": "custom", "templateId":"detect",| &&
                 |"schemaName": "Delivery_Note_Schema",| &&
                 |"receivedDate": "2025-07-28"\}|.

*    "If no schema/template ID then prepare JSON Pay-load for header Item
*    lv_options = '{ "extraction": { "headerFields": [ "deliveryNoteNumber", "purchaseOrderNumber", "deliveryDate" ], "lineItemFields": [ "materialNumber", "quantity", "unitOfMeasure" ] },' &&
*                   '"clientId": "default", "documentType": "custom", "receivedDate": "2025-07-28", "enrichment": { }}'.

*lv_options = '{ "extraction": { "headerFields": [ "deliveryNoteNumber", "purchaseOrderNumber", "deliveryDate" ], "lineItemFields": [ "materialNumber", "quantity", "unitOfMeasure" ] },' &&
*                   '"clientId": "default", "documentType": "custom", "receivedDate": "2025-07-28", "enrichment": { "sender": { "top": 5, "type": ' &&
*                   '"businessEntity", "subtype": "supplier" }, "employee": { "type": "employee" } }}'.
*    lv_options = |\{ "extraction": \{ "headerFields": [ "deliveryNoteNumber", "purchaseOrderNumber", "deliveryDate" ], "lineItemFields": [ "materialNumber", "quantity", "unitOfMeasure" ] \},| &&
*                 |"clientId": "default", "documentType": "Custom", "receivedDate": "2025-07-28", "enrichment": \{ "sender": \{ "top": 5, "type":| &&
*                 |"businessEntity", "subtype": "supplier" \}, "employee": \{ "type": "employee" \} \}\}|.

    lo_request_part2->set_header_field( i_name  = 'Content-Disposition' ##NO_TEXT
                                        i_value = 'form-data; name="options"' ). "; type=application/json
    lo_request_part2->set_header_field( i_name  = 'Content-Type'
                                        i_value = 'application/json' ).
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(lv_value) = lo_request_part2->set_text( i_text = lv_options ).

    " File Part
    DATA(lo_request_part) =  lo_request->add_multipart( ).
    lv_content_disposition = 'form-data; name="file"; filename="delivery_note.pdf"'.
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
*    DATA lr_data         TYPE REF TO data.
    " TODO: variable is never used (ABAP cleaner)
    DATA: BEGIN OF ls_job_status,
            status TYPE string,
          END OF ls_job_status.

    TYPES: BEGIN OF ty_coordinates,
             x TYPE f,
             y TYPE f,
             w TYPE f,
             h TYPE f,
           END OF ty_coordinates.

    TYPES: BEGIN OF ty_header_field,
             name        TYPE string,
             category    TYPE string,
             value       TYPE string,
             raw_value   TYPE string,
             type        TYPE string,
             page        TYPE i,
             confidence  TYPE f,
             coordinates TYPE ty_coordinates,
           END OF ty_header_field.

    TYPES: BEGIN OF ty_line_item,
             material_number TYPE string,
             quantity        TYPE string,
             unit_of_measure TYPE string,
           END OF ty_line_item.

    TYPES tt_header_fields TYPE STANDARD TABLE OF ty_header_field WITH DEFAULT KEY.
    TYPES tt_line_items    TYPE STANDARD TABLE OF ty_line_item WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_extraction_data,
             status        TYPE string,
             header_fields TYPE tt_header_fields,
             line_items    TYPE tt_line_items,
           END OF ty_extraction_data.

    DATA ls_extraction_data TYPE ty_extraction_data.
    DATA ls_line_item       TYPE ty_line_item.
    DATA lt_json_data       TYPE REF TO data.
*    DATA lv_value           TYPE string.

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
    DATA(lv_response_json) = lo_response->get_text( ).
*    /ui2/cl_json=>deserialize( EXPORTING json        = lv_response_json
*                                         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
*                               CHANGING  data        = lr_data ).

    IF ls_status-code = '200'.

      " Extract data from the response and set to global table  gt_extracted_data

      " Your existing code to get the response...

      /ui2/cl_json=>deserialize( EXPORTING json        = lv_response_json
                                           pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                 CHANGING  data        = lt_json_data ).

      " Extract status
      ASSIGN lt_json_data->('STATUS') TO FIELD-SYMBOL(<status>).
      IF <status> IS ASSIGNED.
*      lv_value = <status>.
        ls_extraction_data-status = <status>->*.
      ENDIF.
      IF ls_extraction_data-status = 'DONE'.
        " Extract header fields
        ASSIGN lt_json_data->('EXTRACTION') TO FIELD-SYMBOL(<extraction>).
        IF <extraction> IS ASSIGNED.
          ASSIGN <extraction>->('HEADER_FIELDS') TO FIELD-SYMBOL(<header_fields>).
          IF <header_fields> IS ASSIGNED AND <header_fields> IS NOT INITIAL.
            FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
            ASSIGN <header_fields>->* TO <table>.
            IF sy-subrc = 0.
              LOOP AT <table> ASSIGNING FIELD-SYMBOL(<header_field>).
                APPEND INITIAL LINE TO ls_extraction_data-header_fields ASSIGNING FIELD-SYMBOL(<hf>).

                ASSIGN <header_field>->('NAME') TO FIELD-SYMBOL(<name>).
                ASSIGN <header_field>->('VALUE') TO FIELD-SYMBOL(<value>).
                IF sy-subrc = 0 AND <value> IS NOT INITIAL.
                  <hf>-name  = <name>->*.
                  <hf>-value = <value>->*.

                  " Map other fields if needed
*                  ASSIGN COMPONENT 'CATEGORY' OF STRUCTURE <header_field> TO FIELD-SYMBOL(<category>).
*                  IF sy-subrc = 0.
*                    <hf>-category = <category>->*.
*                  ENDIF.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.

        " Extract line items
        " Extract line items
        ASSIGN lt_json_data->('EXTRACTION') TO <extraction>.
        IF <extraction> IS ASSIGNED.
          ASSIGN <extraction>->('LINE_ITEMS') TO FIELD-SYMBOL(<line_items>).
          IF <line_items> IS ASSIGNED AND <line_items> IS NOT INITIAL.
            FIELD-SYMBOLS <items_table> TYPE STANDARD TABLE.
            ASSIGN <line_items>->* TO <items_table>.
            IF sy-subrc = 0.
              LOOP AT <items_table> ASSIGNING FIELD-SYMBOL(<item_group>).
                CLEAR ls_line_item.

                FIELD-SYMBOLS <item_fields> TYPE STANDARD TABLE.
                ASSIGN <item_group>->* TO <item_fields>.
                IF sy-subrc <> 0.
                  CONTINUE.
                ENDIF.

                LOOP AT <item_fields> ASSIGNING FIELD-SYMBOL(<field>).
                  ASSIGN <field>->('NAME') TO <name>.
                  ASSIGN <field>->('VALUE') TO <value>.

                  IF sy-subrc = 0 AND <value> IS NOT INITIAL.
                    CASE <name>->*.
                      WHEN 'materialNumber'.
                        ls_line_item-material_number = <value>->*.
                      WHEN 'quantity'.
                        ls_line_item-quantity = <value>->*.
                      WHEN 'unitOfMeasure'.
                        ls_line_item-unit_of_measure = <value>->*.
                    ENDCASE.
                  ENDIF.
                ENDLOOP.

                APPEND ls_line_item TO ls_extraction_data-line_items.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.

        " Assign the extracted data to the global table and return job statu
        r_job_status = ls_extraction_data-status.
        " First clear the target table
        CLEAR gt_extracted_data.

        " Map header fields
        LOOP AT ls_extraction_data-header_fields INTO DATA(ls_header).
          APPEND INITIAL LINE TO gt_extracted_data ASSIGNING FIELD-SYMBOL(<fs_target>).

          CASE ls_header-name.
            WHEN 'deliveryNoteNumber'.
              <fs_target>-delnotenum = ls_header-value.
            WHEN 'purchaseOrderNumber'.
              <fs_target>-ponumber = ls_header-value.
            WHEN 'deliveryDate'.
              <fs_target>-deliverydate = ls_header-value.
          ENDCASE.
        ENDLOOP.

        " Map line items
        LOOP AT ls_extraction_data-line_items INTO DATA(ls_item).
          LOOP AT gt_extracted_data ASSIGNING <fs_target>.

            " Map the line item fields
            <fs_target>-material = ls_item-material_number.
            <fs_target>-quantity = ls_item-quantity.
            <fs_target>-unit     = ls_item-unit_of_measure.
          ENDLOOP.
        ENDLOOP.
      ENDIF.
      TRY.
          lo_http_client->close( ).
        CATCH cx_web_http_client_error.
          " handle exception
      ENDTRY.
    ELSE.
      r_job_status = 'FAILED'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
