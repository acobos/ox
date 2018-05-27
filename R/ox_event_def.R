#' Title
#'
#' @param parsed_xml an object of class XMLInternalDocument
#' @param name_encoding
#'
#' @return dataframe
#'
#' @examples
#'
ox_event_def <- function (parsed_xml, name_encoding="UTF-8") {

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
