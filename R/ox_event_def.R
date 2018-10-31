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
#'
#' # Get the example file address
#' file_address <- system.file("extdata",
#'                             "odm1.3_clinical_ext_example.xml",
#'                             package = "ox",
#'                             mustWork = TRUE)
#'
#' # Parsing the xml file
#' library(XML)
#' doc <- xmlParse(file_address)
#'
#' # Event definitions in a dataframe
#' ox_event_def(doc)
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
