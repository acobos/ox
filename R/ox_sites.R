#' Extracts sites from a parsed OpenClinica xml file
#'
#' @param parsed_xml an object of class XMLInternalDocument
#' @param encoding
#'
#' @return dataframe with study_oid and site (names)
#' @export
#'
#' @examples
#'
ox_sites <- function (parsed_xml, encoding="UTF-8"){
  s <- as.character(xpathApply(parsed_xml, "//ns:StudyName",
                               namespaces=ox_alias_default_ns(doc),
                               fun=xmlValue))
  Encoding(s) <- encoding

  # reading Study OIDs
  o <- xpathApply(parsed_xml, "//ns:Study",
                  namespaces=ox_alias_default_ns(doc),
                  fun=xmlAttrs)

  # Merging
  sites <- data.frame(study_oid = as.character(o),
                      site = s,
                      stringsAsFactors = FALSE)

  # return
  # dropping the 1st one, since its the (overall) study name
  sites[-1,]
}
