#' Title Global study variables
#'
#' @param parsed_xml an object of class XMLInternalDocument
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_global_vars <- function (parsed_xml) {
  as.list(lapply(xpathApply(parsed_xml,
                            "//ns:Study/ns:GlobalVariables",
                            namespaces = .ns_alias(parsed_xml, "ns"),
                            fun=xmlChildren)[[1]],
                 xmlValue))
}


