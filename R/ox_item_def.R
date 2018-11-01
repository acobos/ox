#' Item definitions in a dataframe
#'
#' Returns a dataframe with study item definitions from a parsed OpenClinica
#' odm1.3 .xml export file.
#'
#' @param parsed_xml An object of class \code{XMLInternalDocument}, as returned
#' by \code{XML::xmlParse()}.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # The example xml file address
#' file <- system.file("extdata",
#'                     "odm1.3_clinical_ext_example.xml",
#'                     package = "ox",
#'                     mustWork = TRUE)
#'
#' # Parsing the xml file
#' library(XML)
#' doc <- xmlParse(file_address)
#'
#' # Item definitions in a dataframe
#' item_def <- ox_item_def(doc)
#' View(item_def)
ox_item_def <- function (parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  # return
  bind_rows(lapply(xpathApply(parsed_xml,
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
           item_significant_digits = SignificantDigits,
           item_sas_field_name = SASFieldName,
           item_comment = Comment,
           form_oid = FormOIDs)
}

