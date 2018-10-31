#' Gets OpenClinica (OC) study event definitions
#'
#' Returns a dataframe with study event definitions from a parsed OC .xml
#' export file, provided as argument.
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
#' doc <- xmlParse(xmlFile)
#'
#' # get event definitions
#'ox_event_def(doc)
#'
#'}
#'
ox_event_def <- function (parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  # return
  bind_rows(lapply(xpathApply(parsed_xml,
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

}
