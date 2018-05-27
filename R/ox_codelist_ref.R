#' Title
#'
#' @param parsed_xml an object of class XMLInternalDocument
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_codelist_ref <- function(parsed_xml) {

  bind_rows(lapply(xpathApply(parsed_xml,
                              "//ns:CodeListRef",
                              namespaces = ox_alias_default_ns(parsed_xml),
                              fun=xmlAncestors,
                              xmlAttrs),
                   data.frame,
                   stringsAsFactors=FALSE)) %>%
    select(form_oid = FormOIDs,
           item_oid = OID.2,
           codelist_oid = CodeListOID)

}

