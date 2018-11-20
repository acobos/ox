#' Creates an \code{ox_all} object
#'
#' Returns an object of class \code{ox_all}, containing data and metadata, from a
#' parsed OpenClinica odm1.3 .xml export file provided as argument.
#'
#' @param parsed_xml An object of class \code{XMLInternalDocument}, as returned
#' by \code{XML::xmlParse()}.
#'
#' @return An object of class \code{ox_all}, which is a list of two elements:
#'
#' \itemize{
#'   \item \code{data}: a dataframe containig all clinical data, as returned by
#'   \code{ox_data()}.
#'   \item \code{metadata}: a list, as returned by \code{ox_metadata()}. See
#'   \code{\link{ox_metadata}} for more details.
#' }
#'
#' @details A superficial view of the contents of an \code{ox_all} object can be
#' obtained with \code{\link{ox_info}}.
#'
#' @export
#'
#' @examples
#' # The example odm1.3 xml file address
#' my_file <- system.file("extdata",
#'                        "odm1.3_full_example.xml",
#'                        package = "ox",
#'                        mustWork = TRUE)
#'
#' # Parsing the xml file
#' library(XML)
#' doc <- xmlParse(my_file)
#'
#' # Create ox_all object
#' d <- ox_all(doc)
#' class(d)
#' names(d)
#'
#' # The data element
#' head(d$data)
#'
#' # Elements (names) in metadata
#' names(d$metadata)
#'
#' # Accessing metadata elements:
#' # Event definitions
#' head(d$metadata$event_def)
#'
#' # Codelist items
#' head(d$metadata$codelist_item)
#'
#' # etc.
ox_all <- function (parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  res <- list(data = ox_data(parsed_xml),
                 metadata = ox_metadata(parsed_xml))

  # assign class ox
  class(res) <- c("ox_all", "list")

  # return
  res
}

