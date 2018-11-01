
#' Title
#'
#' @param parsed_xml an object of class XMLInternalDocument
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_codelist <- function (parsed_xml) {

  bind_rows(lapply(xpathApply(parsed_xml,
                              "//ns:CodeList",
                              namespaces = .ns_alias(parsed_xml, "ns"),
                              fun=xmlAncestors,
                              xmlAttrs),
                   data.frame,
                   stringsAsFactors=FALSE)) %>%
    select(codelist_oid = OID.2,
           codelist_name = Name.1,
           codelist_data_type = DataType,
           sas_format_name = SASFormatName)

}

