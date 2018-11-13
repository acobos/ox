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
#' # Subject data in a dataframe
#' subjects <- ox_subject_data(doc)
#' head(subjects)
ox_subject_data <- function (parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  # get subject_data
  nodes <- XML::xpathApply(parsed_xml, "//ns:ClinicalData/ns:SubjectData",
                        namespaces = .ns_alias(parsed_xml, "ns"),
                        fun = XML::xmlAttrs)
  # loop
  res <- dplyr::bind_rows(
    lapply(nodes, function (x) data.frame(as.list(x), stringsAsFactors=FALSE))
  )

  # to numeric ----
  if (any("StudySubjectID" %in% names(res))) {
    # only if no NA's resulting from type cohercion
    if (sum(is.na(as.numeric(res$StudySubjectID))) == 0) {
      res$StudySubjectID <- as.numeric(res$StudySubjectID)
    }
  }


  # change CamelCase by snake_case
  names(res) <- snakecase::to_snake_case(names(res))

  # simplify some varnames
  names(res) <- gsub("study_subject", "subject", names(res), fixed=TRUE)

  # return
  res
}

