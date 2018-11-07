#' Creates an \code{ox} object from a parsed OpenClinica xml file
#'
#' Returns an object of class \code{ox}, containing data and metadata, from a
#' parsed OpenClinica odm1.3 .xml export file provided as argument.
#'
#' @param parsed_xml An object of class \code{XMLInternalDocument}, as returned
#' by \code{XML::xmlParse()}.
#'
#' @return An object of class \code{ox}, which is a list of two elements: data
#' and metadata. The data element is a dataframe containig all clinical data.
#' The metadata element is a list of elements, most of which are dataframes,
#' collectively describing the study data structure (events, forms, item groups,
#' items, codelists, measurement units, sites, and subjects).
#'
#' A summary S3 method is available. See \code{\link{summary.ox}} for details.
#'
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
#' # Create ox object
#' my_study <- ox(doc)
#' class(my_study)
#' names(my_study)
#'
#' # The data element
#' View(my_study$data)
#'
#' # Elements (names) in metadata
#' names(my_study$metadata)
#'
#' # Accessing metadata elements:
#' # Event definitions
#' View(my_study$metadata$event_def)
#'
#' # Codelist items
#' View(my_study$metadata$codelist_item)
#'
#' # etc.
ox <- function (parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  ox_obj <- list(data = ox_item_data(parsed_xml),
                 metadata = ox_metadata(parsed_xml))

  # assign class ox
  class(ox_obj) <- c("ox", "list")

  # return
  ox_obj
}

