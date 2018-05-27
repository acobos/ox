#' Title Item references
#'
#' @param parsed_xml an object of class XMLInternalDocument
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_item_ref <- function(parsed_xml) {

  bind_rows(lapply(xpathApply(parsed_xml,
                              "//ns:ItemRef",
                              namespaces = ox_alias_default_ns(parsed_xml),
                              fun=xmlAncestors,
                              xmlAttrs),
                   data.frame,
                   stringsAsFactors=FALSE)) %>%
    select(study_oid = OID,
           version = OID.1,
           metadata_version = Name,
           group_oid = OID.2,
           group_name = Name.1,
           group_repeating = Repeating,
           sas_dataset_name = SASDatasetName,
           item_oid = ItemOID,
           item_order_number = OrderNumber,
           item_mandatory = Mandatory)

}


