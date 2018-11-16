#' Event definitions in a dataframe
#'
#' Returns a dataframe with study event definitions from a parsed OpenClinica
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
#'                        "odm1.3_full_example.xml",
#'                        package = "ox",
#'                        mustWork = TRUE)
#'
#' # Parsing the xml file
#' library(XML)
#' doc <- xmlParse(my_file)
#'
#' # Event definitions in a dataframe
#' event_def <- ox_event_def(doc)
#' head(event_def)
ox_event_def <- function (parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  .attrs_node_and_ancestors(parsed_xml, "StudyEventDef") %>%
    dplyr::select(event_oid = OID.2,
                  event_name = Name.1,
                  event_repeating = Repeating,
                  event_type = Type)

}
