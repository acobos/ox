#' Creates an \code{ox} object from a parsed OpenClinica xml file
#'
#' Returns an object of class \code{ox}, containing data and metadata, from a
#' parsed OpenClinica odm1.3 .xml export file provided as argument.
#'
#' @param parsed_xml An object of class \code{XMLInternalDocument}, as returned
#' by \code{XML::xmlParse()}.
#'
#' @return An object of class \code{ox}, which is a list of two elements: data
#' and metadata. The data element is a dataframe containig all clinical data.
#' The metadata element is a list of elements, most of which are dataframes,
#' collectively describing the study data structure (events, forms, item groups,
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
#' # Create ox object
#' my_study <- ox(doc)
#' class(my_study)
#' names(my_study)
#'
#' # The data element
#' View(my_study$data)
#'
#' # Elements (names) in metadata
#' names(v$metadata)
#'
#' # Accessing metadata elements:
#' # Event definitions
#' View(my_study$metadata$event_def)
#'
#' # Codelist items
#' View(my_study$metadata$codelist_item)
#'
#' # etc.
ox <- function (parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  list(data = ox_item_data(parsed_xml),
       metadata = list(global_vars = ox_global_vars(parsed_xml),
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
                       subjects = ox_subject_data(parsed_xml))) -> ox_obj

  # assign class ox
  class(ox_obj) <- c("ox", "list")

  # return
  ox_obj
}

