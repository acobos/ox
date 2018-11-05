#' Group repetitions in a dataframe
#'
#' Returns a dataframe with study item group repetitions from a parsed OpenClinica
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
#' # Group repetitions in a dataframe
#' group_repeats <- ox_group_repeat(doc)
#' View(group_repeats)
#'
ox_group_repeat <- function(parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  dplyr::bind_rows(
    lapply(
      XML::xpathApply(parsed_xml,
                      "//oc:ItemGroupRepeat",
                      namespaces = .ns_alias(parsed_xml, "oc"),
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
                  form_oid = FormOID,
                  show_group = ShowGroup,
                  repeat_number = RepeatNumber,
                  repeat_max = RepeatMax)
}

