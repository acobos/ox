#' Item data in a dataframe
#'
#' Returns a dataframe with patient's data (all patients, all items, all groups,
#' all forms, all events), from a parsed OpenClinica odm1.3 .xml export file.
#' Be patient, the function is slow, even for small studies.
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
#' # Event definitions in a dataframe
#' d <- item_data(doc)
#' View(d)
ox_item_data <- function(parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  message("Getting ItemData nodes...")
  nodes <- XML::xpathApply(parsed_xml,
                           "//ns:ItemData",
                           namespaces = .ns_alias(parsed_xml, "ns"))


  message("Extracting data from ItemData nodes...")

  # loop over nodes with a progress bar,
  # extract attributes for node an ancestors,
  # bind_rows and renames
  res <- pbapply::pblapply(nodes,
                    FUN = function (x) data.frame(XML::xmlAncestors(x, XML::xmlAttrs),
                                                  stringsAsFactors = FALSE)) %>%
    dplyr::bind_rows() %>%
    dplyr::rename(study_oid = StudyOID,
                  metadata_version_oid = MetaDataVersionOID,
                  subject_key = SubjectKey,
                  # subject_id = StudySubjectID,
                  # subject_status = Status,
                  event_oid = StudyEventOID,
                  event_repeat_key = StudyEventRepeatKey,
                  form_oid = FormOID,
                  # form_version = Version,
                  # form_status = Status.1,
                  group_oid = ItemGroupOID,
                  group_repeat_key = ItemGroupRepeatKey,
                  trasaction_type = TransactionType,
                  item_oid = ItemOID,
                  value = Value) %>%
    dplyr::mutate(group_repeat_key = as.numeric(group_repeat_key),
                  event_repeat_key = as.numeric(event_repeat_key))

  # droping unneded vars
  res$FileOID <- NULL
  res$Description <- NULL
  res$CreationDateTime <- NULL
  res$FileType <- NULL
  res$ODMVersion <- NULL
  res$schemaLocation <- NULL

  # renaming additional vars in odm1.3_ext and _full,
  # (these are NOT present in odm1.3_clinical, so I CANNOT assume
  # they will be always present)
  if ("StudySubjectID" %in% names(res)) {
    names(res)[which(names(res) == "StudySubjectID")] <- "subject_id"
  }

  if ("Status" %in% names(res)) {
    names(res)[which(names(res) == "Status")] <- "subject_status"
  }

  if ("Version" %in% names(res)) {
    names(res)[which(names(res) == "Version")] <- "form_version"
  }

  if ("Status.1" %in% names(res)) {
    names(res)[which(names(res) == "Status.1")] <- "form_status"
  }

  message("Done")

  #return
  res
}

