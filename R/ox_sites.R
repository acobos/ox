#' Sites in a dataframe
#'
#' Returns a dataframe with study sites from a parsed OpenClinica
#' odm1.3 .xml export file.
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
#' # Sites in a dataframe
#' sites <- ox_sites(doc)
#' View(sites)
#'
ox_sites <- function (parsed_xml){
  s <- as.character(xpathApply(parsed_xml, "//ns:StudyName",
                               namespaces=ox_alias_default_ns(doc),
                               fun=xmlValue))

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
