#' Event references in a dataframe
#'
#' Returns a dataframe with study event references from a parsed OpenClinica
#' odm1.3 .xml export file.
#'
#' @param parsed_xml An object of class \code{XMLInternalDocument}, as returned
#' by \code{XML::xmlParse()}.
#'
#' @param simplify A \code{logical} indicting whether or not to simplify output
#' whenever possible (see details). Defaults to \code{FALSE}.
#'
#' @details In OpenClinica, diferent sites (\code{study_oid}) may have diferent
#' events defined. For this reason, by default (\code{simplify = FALSE}), events
#' (\code{event_oid}) and event characteristics (\code{event_order},
#' \code{event_mandatory}) are returned for each site (\code{study_oid}). However, in
#' most studies, all sites have the same events and characteristics. If this is
#' the case, and \code{simplify = TRUE}, only unique combinations of
#' \code{event_oid}, \code{event_order} and \code{event_mandatory} are returned.
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
ox_event_ref <- function (parsed_xml, simplify = FALSE) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  if (!("logical" %in% class(simplify) & length(simplify) == 1)) {
    stop("simplify should be a logical value", call. = FALSE)
  }

  dplyr::bind_rows(
    lapply(
      XML::xpathApply(parsed_xml,
                      "//ns:StudyEventRef",
                      namespaces = .ns_alias(parsed_xml, "ns"),
                      fun = XML::xmlAncestors,
                      XML::xmlAttrs),
      data.frame, stringsAsFactors=FALSE)) %>%
    dplyr::select(study_oid = OID,
                  version = OID.1,
                  metadata_version = Name,
                  event_oid = StudyEventOID,
                  event_order = OrderNumber,
                  event_mandatory = Mandatory) %>%
    dplyr::mutate(event_order = as.numeric(event_order)) %>%
    dplyr::arrange(study_oid, version, event_order) -> e_r

  # Pending to decide if allow for simplification when all sites have same data in
  # event_oid, event_order and event_mandatory:
  # select(event_oid:event_mandatory) %>% unique()

  if (simplify) {

    # Verify that can be simplified. Assess conditions:

    # should be true
    with(e_r, table(table(study_oid, event_oid))) -> s_by_e
    a <- s_by_e["1"] == nrow(e_r)

    # should be true
    b <- length(unique(e_r$event_oid, e_r$event_order)) == length(unique(e_r$event_oid))

    # should be true
    c <- length(unique(e_r$event_oid, e_r$event_mandatory)) == length(unique(e_r$event_oid))

    if (a & b & c) {
      e_r %>%
        select(event_oid:event_mandatory) %>%
        unique() -> e_r
      }
  }

  # return
  e_r
  }

