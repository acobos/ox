#' Title
#'
#' @param parsed_xml an object of class XMLInternalDocument
#'
#' @return default namespace alias
#' @export
#'
#' @examples
#'
ox_alias_default_ns <- function (parsed_xml) {
  c(ns=xmlNamespaces(parsed_xml)[[1]]$uri)
}
ox_alias_oc_ns <- function (parsed_xml) {
  c(oc=xmlNamespaces(parsed_xml)[[2]]$uri)
}
