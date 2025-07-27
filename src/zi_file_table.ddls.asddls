@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface View for File Data'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZI_FILE_TABLE
  as select from    I_Language  as _language
    left outer join zfile_table as _ses_file on _language.Language = $session.system_language
  //as select from zfile_table as _ses_file
  composition [0..*] of ZI_FILE_UPLOAD_DATA as _ses_excel
{
      //key _user.UserID                     as end_user,
  key $session.user                   as end_user,
      @Semantics.largeObject:
      { mimeType: 'MimeType',
      fileName: 'Filename',
      acceptableMimeTypes: [ 'application/pdf', 'image/jpeg' ],
      contentDispositionPreference: #INLINE } // This will store the File into our table
      _ses_file.attachment            as Attachment,
      @Semantics.mimeType: true
      _ses_file.mimetype              as MimeType,
      _ses_file.filename              as Filename,
      @Semantics.user.createdBy: true
      _ses_file.local_created_by      as Local_Created_By,
      @Semantics.systemDateTime.createdAt: true
      _ses_file.local_created_at      as Local_Created_At,
      @Semantics.user.lastChangedBy: true
      _ses_file.local_last_changed_by as Local_Last_Changed_By,
      //local ETag field --> OData ETag
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      _ses_file.local_last_changed_at as Local_Last_Changed_At,
      //total ETag field
      @Semantics.systemDateTime.lastChangedAt: true
      _ses_file.last_changed_at       as Last_Changed_At,
      //Expose Associations
      _ses_excel

}
where
  _language.Language = $session.system_language
