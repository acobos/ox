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
  as.list(sapply(xpathApply(parsed_xml,
                            "//ns:Study/ns:GlobalVariables",
                            namespaces = ox_alias_default_ns(parsed_xml),
                            fun=xmlChildren)[[1]],
                 xmlValue))
}


