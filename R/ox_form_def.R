#' Title Form definitions
#'
#' @param parsed_xml an object of class XMLInternalDocument
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_form_def <- function (parsed_xml) {
  bind_rows(lapply(xpathApply(parsed_xml,
                              "//ns:FormDef",
                              namespaces = ox_alias_default_ns(parsed_xml),
                              fun=xmlAncestors,
                              xmlAttrs),
                   data.frame, stringsAsFactors=FALSE)) %>%
    select(study_oid = OID,
           version = OID.1,
           metadata_version = Name,
           form_oid = OID.2,
           form_name = Name.1,
           form_repeating = Repeating)

}


