#' Codelist references in a dataframe
#'
#' Returns a dataframe with study codelist references from a parsed OpenClinica
#' odm1.3 .xml export file.
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
#' # Form references in a dataframe
#' codelist_ref <- ox_codelist_ref(doc)
#' head(codelist_ref)
#'
ox_codelist_ref <- function(parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  dplyr::bind_rows(
    lapply(
      XML::xpathApply(parsed_xml,
                      "//ns:CodeListRef",
                      namespaces = .ns_alias(parsed_xml, "ns"),
                      fun = XML::xmlAncestors,
                      XML::xmlAttrs),
      data.frame,
      stringsAsFactors=FALSE)) %>%
    dplyr::select(item_oid = OID.2,
                  codelist_oid = CodeListOID)

}

