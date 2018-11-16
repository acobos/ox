#' Metadata in a list of dataframes
#'
#' Returns a list of dataframes containing metadata, from a parsed OpenClinica
#' odm1.3 .xml export file provided as argument.
#'
#' @param parsed_xml An object of class \code{XMLInternalDocument}, as returned
#' by \code{XML::xmlParse()}.
#'
#' @return A list containing the following elements:
#'
#' \itemize{
#'   \item \code{file_info}: a list with .xml file information.
#'   \item \code{global_vars}: a list with global study variables.
#'   \item \code{event_def}: a dataframe of event definitions.
#'   \item \code{event_ref}: a dataframe of event references.
#'   \item \code{form_def}: a dataframe of form definitions.
#'   \item \code{form_ref}: a dataframe of form references.
#'   \item \code{group_def}: a dataframe of (item) group definitions.
#'   \item \code{group_ref}: a dataframe of (item) group references.

#'   \item \code{codelist}: a dataframe of codelists.
#'   \item \code{codelist_item}: a dataframe of codelist items.
#'   \item \code{codelist_ref}: a dataframe of codelist references.
#'
#'   \item \code{units}: a dataframe of unit definitions.
#'   \item \code{sites}: a dataframe of study sites.
#'   \item \code{subjects}: a dataframe of study subjects.
#' }
#'
#' @export
#'
#' @examples
#' # The example odm1.3 xml file address
#' my_file <- system.file("extdata",
#'                        "odm1.3_full_example.xml",
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
#' head(md$event_def)
#'
#' # Codelist items
#' head(md$codelist_item)
#'
#' # etc.
ox_metadata <- function (parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  message("Extracting metadata...")

  res <- list(file_info = ox_file_info(parsed_xml),
              global_vars = ox_global_vars(parsed_xml),
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

