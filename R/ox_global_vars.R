#' Global variables in a list
#'
#' Returns a list with study global variables from a parsed OpenClinica
#' odm1.3 .xml export file.
#'
#' @param parsed_xml An object of class \code{XMLInternalDocument}, as returned
#' by \code{XML::xmlParse()}.
#'
#' @return A list.
#' @export
#'
#' @examples
#' # The example odm1.3 xml file address
#' my_file <- system.file("extdata",
#'                        "odm1.3_clinical_ext_example.xml",
#'                        package = "ox",
#'                        mustWork = TRUE)
#'
#' # Parsing the xml file
#' library(XML)
#' doc <- xmlParse(my_file)
#'
#' # Global variables in a list
#' global_vars <- ox_global_vars(doc)
#' View(global_vars)
ox_global_vars <- function (parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  as.list(lapply(XML::xpathApply(parsed_xml,
                            "//ns:Study/ns:GlobalVariables",
                            namespaces = .ns_alias(parsed_xml, "ns"),
                            fun = XML::xmlChildren)[[1]],
                 XML::xmlValue))
}


