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
#' # Sites in a dataframe
#' sites <- ox_sites(doc)
#' View(sites)
#'
ox_sites <- function (parsed_xml){

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  s <- as.character(xpathApply(parsed_xml, "//ns:StudyName",
                               namespaces=.ns_alias(parsed_xml, "ns"),
                               fun=xmlValue))

  # reading Study OIDs
  o <- XML::xpathApply(parsed_xml, "//ns:Study",
                       namespaces = .ns_alias(parsed_xml, "ns"),
                       fun=XML::xmlAttrs)

  # Merging
  sites <- data.frame(study_oid = as.character(o),
                      site = s,
                      stringsAsFactors = FALSE)

  # return
  # dropping the 1st one, since its the (overall) study name
  sites[-1,]
}
