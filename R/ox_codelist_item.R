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
#' # The example odm1.3 xml file address
#' my_file <- system.file("extdata",
#'                        "odm1.3_full_example.xml",
#'                        package = "ox",
#'                        mustWork = TRUE)
#'
#' # Parsing the xml file
#' library(XML)
#' doc <- xmlParse(my_file)
#'
#' # Codelists items in a dataframe
#' codelist_items <- ox_codelist_item(doc)
#' head(codelist_items)
ox_codelist_item <- function (parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  cli <- dplyr::bind_rows(
    lapply(
      XML::xpathApply(
        parsed_xml,
        "//ns:CodeList/ns:CodeListItem/ns:Decode/ns:TranslatedText",
        namespaces = .ns_alias(parsed_xml, "ns"),
        fun = XML::xmlAncestors, XML::xmlAttrs),
      data.frame, stringsAsFactors=FALSE) ) %>%
    dplyr::select(codelist_oid = OID.2,
                  codelist_name = Name.1,
                  codelist_data_type = DataType,
                  sas_format_name = SASFormatName,
                  coded_value = CodedValue)

  labels <- data.frame(
    sapply(
      XML::xpathApply(
        parsed_xml,
        "//ns:CodeList/ns:CodeListItem/ns:Decode/ns:TranslatedText",
        namespaces = .ns_alias(parsed_xml, "ns"),
        fun=XML::xmlValue),
      unlist),
    stringsAsFactors=FALSE) %>%
    dplyr::select(code_label = 1)

  cli %>%
    cbind(labels)

}


