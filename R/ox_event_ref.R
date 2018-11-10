#' Event references in a dataframe
#'
#' Returns a dataframe with study event references from a parsed OpenClinica
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
#' # Event references in a dataframe
#' event_ref <- ox_event_ref(doc)
#' head(event_ref)
ox_event_ref <- function (parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  .attrs_node_and_ancestors(parsed_xml, "StudyEventRef") %>%
    dplyr::select(study_oid = OID,
                  event_oid = StudyEventOID,
                  event_order = OrderNumber,
                  event_mandatory = Mandatory) %>%
    dplyr::mutate(event_order = as.numeric(event_order)) -> res

  # to see if all study_oid have same info, to simplify result if possible
  res %>% dplyr::select(-study_oid) %>% unique() -> uniques
  unique(res$study_oid) %>% length() -> studies

  if (nrow(uniques) * studies == nrow(res)) {
    uniques %>%
      dplyr::arrange(event_order)
  } else {res %>%
      dplyr::arrange(study_oid, event_order)}

}

