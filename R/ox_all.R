#' Title
#'
#' @param parsed_xml
#'
#' @return dataframes
#' @export
#'
#' @examples
#'
ox_all <- function (parsed_xml) {

stru <- list(global_vars = ox_global_vars(parsed_xml),
             event_def = ox_event_def(parsed_xml),
             event_ref = ox_event_ref(parsed_xml),
             form_def = ox_form_def(parsed_xml),
             form_ref = ox_form_ref(parsed_xml),
             group_def = ox_group_def(parsed_xml),
             group_ref = ox_group_ref(parsed_xml),
             item_def = ox_item_def(parsed_xml),
             item_ref = ox_item_ref(parsed_xml),
             subject_data = ox_subject_data(parsed_xml),
             # item_data = ox_item_data(parsed_xml),
             codelist = ox_codelist(parsed_xml),
             codelist_item = ox_codelist_item(parsed_xml),
             codelist_ref = ox_codelist_ref(parsed_xml),
             units = ox_measurement_units(parsed_xml)
             )

  list2env(stru,globalenv())

}


