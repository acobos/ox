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

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  bind_rows(lapply(xpathApply(parsed_xml,
                              "//ns:CodeListRef",
                              namespaces = .ns_alias(parsed_xml, "ns"),
                              fun=xmlAncestors,
                              xmlAttrs),
                   data.frame,
                   stringsAsFactors=FALSE)) %>%
    select(form_oid = FormOIDs,
           item_oid = OID.2,
           codelist_oid = CodeListOID)

}

