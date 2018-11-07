#' Metadata in a list of dataframes
#'
#' Returns a list of dataframes containing metadata, from a parsed OpenClinica
#' odm1.3 .xml export file provided as argument.
#'
#' @param parsed_xml An object of class \code{XMLInternalDocument}, as returned
#' by \code{XML::xmlParse()}.
#'
#' @return A list of elements, most of which are dataframes, collectively
#' describing the study data structure (events, forms, item groups,
#' items, codelists, measurement units, sites, and subjects).
#'
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
#' # Get metadata
#' md <- ox_metadata(doc)
#' class(md)
#' names(md)
#'
#' # Accessing metadata elements:
#' # Event definitions
#' View(md$event_def)
#'
#' # Codelist items
#' View(md$codelist_item)
#'
#' # etc.
ox_metadata <- function (parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  message("Extracting metadata...")

  res <- list(global_vars = ox_global_vars(parsed_xml),
              event_def = ox_event_def(parsed_xml),
              event_ref = ox_event_ref(parsed_xml),
              form_def = ox_form_def(parsed_xml),
              form_ref = ox_form_ref(parsed_xml),
              group_def = ox_group_def(parsed_xml),
              group_ref = ox_group_ref(parsed_xml),
              item_def = ox_item_def(parsed_xml),
              item_ref = ox_item_ref(parsed_xml),
              codelist = ox_codelist(parsed_xml),
              codelist_item = ox_codelist_item(parsed_xml),
              codelist_ref = ox_codelist_ref(parsed_xml),
              units = ox_measurement_units(parsed_xml),
              sites = ox_sites(parsed_xml),
              subjects = ox_subject_data(parsed_xml))

  message("Done")

  #return
  res
}
