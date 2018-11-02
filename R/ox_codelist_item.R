#' Codelists items in a dataframe
#'
#' Returns a dataframe with study codelist items from a parsed OpenClinica
#' odm1.3_full .xml export file.
#'
#' @param parsed_xml An object of class \code{XMLInternalDocument}, as returned
#' by \code{XML::xmlParse()}.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # The example xml file address
#' file <- system.file("extdata",
#'                     "odm1.3_clinical_ext_example.xml",
#'                     package = "ox",
#'                     mustWork = TRUE)
#'
#' # Parsing the xml file
#' library(XML)
#' doc <- xmlParse(file_address)
#'
#' # Codelists items in a dataframe
#' codelist_item <- ox_codelist_item(doc)
#' View(codelist)
ox_codelist_item <- function (parsed_xml) {

  cli <- bind_rows(lapply(xpathApply(parsed_xml,
                                     "//ns:CodeList/ns:CodeListItem/ns:Decode/ns:TranslatedText",
                                     namespaces = .ns_alias(parsed_xml, "ns"),
                                     fun=xmlAncestors,
                                     xmlAttrs),
                          data.frame,
                          stringsAsFactors=FALSE)) %>%
    select(codelist_oid = OID.2,
           codelist_name = Name.1,
           codelist_data_type = DataType,
           sas_format_name = SASFormatName,
           coded_value = CodedValue)

  labels <- data.frame(sapply(xpathApply(parsed_xml,
                                         "//ns:CodeList/ns:CodeListItem/ns:Decode/ns:TranslatedText",
                                         namespaces = .ns_alias(parsed_xml, "ns"),
                                         fun=xmlValue),
                              unlist),
                       stringsAsFactors=FALSE) %>%
    select(code_label = 1)

  cli %>%
    cbind(labels)

}


