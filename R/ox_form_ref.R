#' Title Form references
#'
#' @param parsed_xml an object of class XMLInternalDocument
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_form_ref <- function (parsed_xml) {
  bind_rows(lapply(xpathApply(parsed_xml,
                              "//ns:FormRef",
                              namespaces = ox_alias_default_ns(parsed_xml),
                              fun=xmlAncestors,
                              xmlAttrs),
                   data.frame, stringsAsFactors=FALSE)) %>%
    select(study_oid = OID,
           version = OID.1,
           metadata_version = Name,
           event_oid = OID.2,
           event_name = Name.1,
           event_repeating = Repeating,
           event_type = Type,
           form_oid = FormOID,
           form_mandatory = Mandatory,
           FormOID, Mandatory)

}



