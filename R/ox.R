#' Creates an \code{ox} object
#'
#' Returns an object of class \code{ox}, containing data and metadata, from a
#' parsed OpenClinica odm1.3 .xml export file provided as argument.
#'
#' @param parsed_xml An object of class \code{XMLInternalDocument}, as returned
#' by \code{XML::xmlParse()}.
#'
#' @return An object of class \code{ox}, which is a list of two elements:
#'
#' \itemize{
#'   \item \code{data}: a dataframe containig all clinical data, as returned by
#'   \code{ox_item_data()}.
#'   \item \code{metadata}: a list, as returned by \code{ox_metadata()}. See
#'   \code{\link{ox_metadata}} for more details.
#' }
#'
#' @details A superficial view of the contents of an \code{ox} object can be
#' obtained with \code{\link{ox_info}}.
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

