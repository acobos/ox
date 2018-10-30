#' Title Subject data
#'
#' @param parsed_xml an object of class XMLInternalDocument
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_subject_data <- function (parsed_xml) {

  sd <- xpathApply(parsed_xml, "//ns:ClinicalData/ns:SubjectData",
                   namespaces=ox_alias_default_ns(doc),
                   fun=xmlAttrs)

  as.data.frame(do.call(rbind, sd),
                stringsAsFactors = FALSE)
}

