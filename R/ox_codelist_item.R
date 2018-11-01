#' Title
#'
#' @param parsed_xml an object of class XMLInternalDocument
#' @param label_encoding
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_codelist_item <- function (parsed_xml, label_encoding = "UTF-8") {

  cli <- bind_rows(lapply(xpathApply(parsed_xml,
                                     "//ns:CodeList/ns:CodeListItem/ns:Decode/ns:TranslatedText",
                                     namespaces = ox_alias_default_ns(parsed_xml),
                                     fun=xmlAncestors,
                                     xmlAttrs),
                          data.frame,
                          stringsAsFactors=FALSE)) %>%
    select(study_oid = OID,
           version = OID.1,
           metadata_version = Name,
           codelist_oid = OID.2,
           codelist_name = Name.1,
           codelist_data_type = DataType,
           sas_format_name = SASFormatName,
           coded_value = CodedValue)

  labels <- data.frame(sapply(xpathApply(parsed_xml,
                                         "//ns:CodeList/ns:CodeListItem/ns:Decode/ns:TranslatedText",
                                         namespaces = .ns_alias(parsed_xml, "ns"),
                                         fun=xmlValue),
                              unlist),
                       stringsAsFactors=FALSE) %>%
    select(code_label = 1)

  # ensure encoding
  Encoding(labels$code_label) <- label_encoding


  cli %>%
    cbind(labels)

}


