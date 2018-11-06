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
  nodes <- XML::xpathApply(doc,
                           "//ns:ItemData",
                           namespaces = .ns_alias(doc, "ns"))

  # to store results
  res_list <- vector("list", length(nodes))

  message("Extracting data from ItemData nodes...")

  # start progress bar
  pb <- pbapply::startpb(0, length(nodes))

  # loop over nodes
  for (i in seq_along(1:length(nodes))) {

    # extract attributes from node and ancestors
    res_list[[i]] <- data.frame(xmlAncestors(nodes[[i]], xmlAttrs),
                           stringsAsFactors = FALSE)
    # update progress bar
    pbapply::setpb(pb, i)
  }

  # close and remove progress bar
  pbapply::closepb(pb)
  rm(pb)

  message("Extraction completed. Creating dataframe...")

  res_df <- dplyr::bind_rows(res_list) %>%
    dplyr::select(study_oid = StudyOID,
                  metadata_version_oid = MetaDataVersionOID,
                  subject_key = SubjectKey,
                  subject_id = StudySubjectID,
                  subject_status = Status,
                  event_oid = StudyEventOID,
                  event_repeat_key = StudyEventRepeatKey,
                  form_oid = FormOID,
                  form_version = Version,
                  form_status = Status.1,
                  group_oid = ItemGroupOID,
                  group_repeat_key = ItemGroupRepeatKey,
                  trasaction_type = TransactionType,
                  item_oid = ItemOID,
                  value = Value) %>%
    dplyr::mutate(group_repeat_key = as.numeric(group_repeat_key),
                  event_repeat_key = as.numeric(event_repeat_key))
  message("Done")

  #return
  res_df
}

