#' Title
#'
#' @param parsed_xml
#'
#' @return a list of two elements: data and metadata. The data element is a
#' dataframe containig all clinical data. The metadata element is a list of
#' dataframes (and a vector with global variables), documenting events, forms,
#' groups, items, codelists and measurement units.
#'
#' @export
#'
#' @examples
#'
ox <- function (parsed_xml) {

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
                        subject_data = ox_subject_data(parsed_xml),
                        codelist = ox_codelist(parsed_xml),
                        codelist_item = ox_codelist_item(parsed_xml),
                        codelist_ref = ox_codelist_ref(parsed_xml),
                        units = ox_measurement_units(parsed_xml)))
}


