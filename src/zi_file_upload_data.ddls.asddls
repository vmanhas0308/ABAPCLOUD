@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interafce View FIle Actual Data'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_FILE_UPLOAD_DATA
  as select from zfile_uploaddt
  association to parent ZI_FILE_TABLE as _ses_file on $projection.EndUser = _ses_file.end_user
{
  key end_user      as EndUser,
  key delnotenum    as Delnotenum,
  key itemno        as Itemno,
      ponumber      as Ponumber,
      deliverydate  as Deliverydate,
      material      as Material,
      @Semantics.quantity.unitOfMeasure : 'unit'
      quantity      as Quantity,
      unit          as Unit,
      error         as Error,
      error_message as ErrorMessage,
      //Expose Associations
      _ses_file
}
