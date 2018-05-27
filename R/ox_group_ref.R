#' Title ItemGroup references
#'
#' @param parsed_xml an object of class XMLInternalDocument
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_group_ref <- function (parsed_xml) {

  bind_rows(lapply(xpathApply(parsed_xml,
                              "//ns:ItemGroupRef",
                              namespaces = ox_alias_default_ns(parsed_xml),
                              fun=xmlAncestors,
                              xmlAttrs),
                   data.frame,
                   stringsAsFactors=FALSE)) %>%
    select(study_oid = OID,
           version = OID.1,
           metadata_version = Name,
           form_oid = OID.2,
           form_name = Name.1,
           form_repeating = Repeating,
           group_oid = ItemGroupOID,
           group_mandatory = Mandatory)
}



