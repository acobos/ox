#' Title Item definitions
#'
#' @param parsed_xml an object of class XMLInternalDocument
#' @param comment_encoding
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_item_def <- function (parsed_xml, comment_encoding="UTF-8") {

  i_def <- bind_rows(lapply(xpathApply(parsed_xml,
                                       "//ns:ItemDef",
                                       namespaces = ox_alias_default_ns(parsed_xml),
                                       fun=xmlAncestors,
                                       xmlAttrs),
                            data.frame,
                            stringsAsFactors=FALSE))  %>%
    select(study_oid = OID,
           version = OID.1,
           metadata_version = Name,
           item_oid = OID.2,
           item_name = Name.1,
           item_data_type = DataType,
           item_length = Length,
           item_sas_field_name = SASFieldName,
           item_comment = Comment,
           form_oid = FormOIDs,
           item_significant_digits = SignificantDigits)

  Encoding(i_def$item_comment) <- comment_encoding

  i_def
}

