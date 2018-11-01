#' Title
#'
#' @param parsed_xml an object of class XMLInternalDocument
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_group_repeat <- function(parsed_xml) {
  # pending simplify
  # oid.2 seems to be identical to group_oid... why ??

  bind_rows(lapply(xpathApply(parsed_xml,
                              "//oc:ItemGroupRepeat",
                              namespaces = .ns_alias(parsed_xml, "oc"),
                              fun=xmlAncestors,
                              xmlAttrs),
                   data.frame,
                   stringsAsFactors=FALSE)) %>%
    select(study_oid = OID,
           version = OID.1,
           metadata_version = Name,
           oid.2 = OID.2,
           group_name = Name.1,
           group_repeating = Repeating,
           sas_dataset_name = SASDatasetName,
           group_oid = ItemGroupOID,
           form_oid = FormOID,
           show_group = ShowGroup,
           repeat_number = RepeatNumber,
           repeat_max = RepeatMax)

}

