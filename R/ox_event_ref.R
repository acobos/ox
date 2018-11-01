#' Title
#'
#' @param parsed_xml an object of class XMLInternalDocument
#' @param simplify logical
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_event_ref <- function (parsed_xml, simplify = FALSE) {

  bind_rows(lapply(xpathApply(doc,
                              "//ns:StudyEventRef",
                              namespaces = .ns_alias(parsed_xml, "ns"),
                              fun=xmlAncestors,
                              xmlAttrs),
                   data.frame, stringsAsFactors=FALSE)) %>%
    select(study_oid = OID,
           version = OID.1,
           metadata_version = Name,
           event_oid = StudyEventOID,
           event_order = OrderNumber,
           event_mandatory = Mandatory) %>%
    arrange(study_oid, version, event_order) -> e_r

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

