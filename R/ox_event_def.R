#' Title
#'
#' @param parsed_xml an object of class XMLInternalDocument
#' @param name_encoding
#'
#' @return dataframe
#' @export
#'
#' @examples
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
