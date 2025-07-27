@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection View File Metadata'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZC_FILE_TABLE
  provider contract transactional_query
  as projection on ZI_FILE_TABLE
{
  key end_user,
      @Semantics.largeObject:
         { mimeType: 'MimeType',
         fileName: 'Filename',
         acceptableMimeTypes: [ 'application/pdf', 'image/jpeg' ],
         contentDispositionPreference: #INLINE } // This will store the File into our table
      Attachment,
      @Semantics.mimeType: true
      MimeType,
      Filename,
      Local_Created_By,
      Local_Created_At,
      Local_Last_Changed_By,
      @EndUserText.label: 'Last Action On'
      Local_Last_Changed_At,
      Last_Changed_At,
      /* Associations */
      _ses_excel : redirected to composition child ZC_FILE_UPLOAD_DATA
}
