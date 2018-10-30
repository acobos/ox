#' Gets OpenClinica (OC) study event definitions
#'
#' Returns a dataframe with study event definitions from a parsed OC .xml
#' export file, provided as argument. Allows to define the character encoding
#' for event names.
#'
#' @param parsed_xml An object of class \code{XMLInternalDocument}, as returned
#' by \code{XML::xmlParse()}.
#' @param name_encoding A character string of length 1, either \code{"UTF-8"} (default)
#' or \code{"latin-1"}, to define the encoding of event names.
#'
#' @return A dataframe documenting the event definitions.
#' @export
#'
#' @examples
#'\dontrun{
#'
#' xmlFile <- "you_OC_export_file_address.xml"
#'
#' # Parsing the xml file
#' library(XML)
#' doc <- xmlParse(xmlFile, encoding="US_ASCII")
#'
#' # get event definitions
#'ox_event_def(doc)
#'
#'# same, defining encoding
#'ox_event_def(doc, "latin-1")
#'}
#'
ox_event_def <- function (parsed_xml, name_encoding="UTF-8") {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  if (class(name_encoding) != "character") {
    stop("name_encoding should be a character vector of length 1", call. = FALSE)
  }

  if (length(name_encoding) > 1) {
    stop("name_encoding should be a character vector of length 1", call. = FALSE)
  }

  if (!name_encoding %in% c("UTF-8", "latin-1")) {
    stop("name_encoding should be either `UTF-8` or `latin-1`", call. = FALSE)
  }

  ev_def <- bind_rows(lapply(xpathApply(parsed_xml,
                              "//ns:StudyEventDef",
                              namespaces = ox_alias_default_ns(parsed_xml),
                              fun=xmlAncestors,
                              xmlAttrs),
                   data.frame, stringsAsFactors=FALSE)) %>%
    select(study_oid = OID,
           version = OID.1,
           metadata_version = Name,
           event_oid = OID.2,
           event_name = Name.1,
           event_repeating = Repeating,
           event_type = Type)

  Encoding(ev_def$event_name) <- name_encoding

  ev_def

}
