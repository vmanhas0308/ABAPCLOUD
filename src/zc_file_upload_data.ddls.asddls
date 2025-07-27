@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection View File Actual Data'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZC_FILE_UPLOAD_DATA
  as projection on ZI_FILE_UPLOAD_DATA
{
  key EndUser,
  key Delnotenum,
  key Itemno,
      Ponumber,
      Deliverydate,
      Material,
      @Semantics.quantity.unitOfMeasure : 'unit'
      Quantity,
      Unit,
      Error,
      ErrorMessage,
      /* Associations */
      _ses_file : redirected to parent ZC_FILE_TABLE
}
