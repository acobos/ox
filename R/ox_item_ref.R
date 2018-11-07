#' Item references in a dataframe
#'
#' Returns a dataframe with study item references from a parsed OpenClinica
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
#' # Form references in a dataframe
#' item_ref <- ox_item_ref(doc)
#' head(item_ref)
#'
ox_item_ref <- function(parsed_xml) {

  dplyr::bind_rows(
    lapply(
      XML::xpathApply(parsed_xml,
                      "//ns:ItemRef",
                      namespaces = .ns_alias(parsed_xml, "ns"),
                      fun = XML::xmlAncestors,
                      XML::xmlAttrs),
      data.frame,
      stringsAsFactors=FALSE)) %>%
    dplyr::select(study_oid = OID,
                  version = OID.1,
                  metadata_version = Name,
                  group_oid = OID.2,
                  group_name = Name.1,
                  group_repeating = Repeating,
                  sas_dataset_name = SASDatasetName,
                  item_oid = ItemOID,
                  item_order_number = OrderNumber,
                  item_mandatory = Mandatory)

}


