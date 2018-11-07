#' Group definitions in a dataframe
#'
#' Returns a dataframe with study item group definitions from a parsed OpenClinica
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
#' group_def <- ox_group_def(doc)
#' View(group_def)
ox_group_def <- function (parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  # return
  dplyr::bind_rows(
    lapply(
      XML::xpathApply(parsed_xml,
                      "//ns:ItemGroupDef",
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
                  sas_dataset_name = SASDatasetName)
}


