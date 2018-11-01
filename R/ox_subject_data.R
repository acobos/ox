#' Subject data in a dataframe
#'
#' Returns a dataframe with study subjects data from a parsed OpenClinica
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
#' # Subject data in a dataframe
#' subjects <- ox_subject_data(doc)
#' View(subjects)
ox_subject_data <- function (parsed_xml) {

  # get subject_data
  sd <- xpathApply(parsed_xml, "//ns:ClinicalData/ns:SubjectData",
                   namespaces = .ns_alias(parsed_xml, "ns"),
                   fun=xmlAttrs)
  # return
  as.data.frame(do.call(rbind, sd),
                stringsAsFactors = FALSE)
}

