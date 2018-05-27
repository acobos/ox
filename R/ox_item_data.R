#' Title Clinical data
#'
#' @param parsed_xml an object of class XMLInternalDocument
#' @param value_encoding
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_item_data <- function(parsed_xml, value_encoding = "UTF-8") {

  item_data <- bind_rows(lapply(xpathApply(parsed_xml,
                                           "//ns:ItemData",
                                           namespaces = ox_alias_default_ns(parsed_xml),
                                           fun=xmlAncestors,
                                           xmlAttrs),
                                data.frame,
                                stringsAsFactors=FALSE)) %>%
    select(study_oid = StudyOID,
           metadata_version_oid = MetaDataVersionOID,
           subject_key = SubjectKey,
           subject_id = StudySubjectID,
           subject_status = Status,
           sex = Sex,
           event_oid = StudyEventOID,
           start_date = StartDate,
           form_oid = FormOID,
           form_version = Version,
           form_status = Status.1,
           group_oid = ItemGroupOID,
           group_repeat_key = ItemGroupRepeatKey,
           trasaction_type = TransactionType,
           item_oid = ItemOID,
           value = Value)

  Encoding(item_data$value) <- value_encoding

  # return
  item_data

}

