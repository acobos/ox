#' @title Creates an ox object from a parsed OpenClinica xml file
#'
#' @description Returns and ox object from a parsed OpenClinica xml file
#' provided as argument
#'
#' @param parsed_xml
#'
#' @return An ox object, which is a list of two elements: data and metadata. The data element is a
#' dataframe containig all clinical data. The metadata element is a list of
#' dataframes (and a vector with global variables), documenting events, forms,
#' groups, items, codelists, measurement units and sites.
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
#'
#'
#'
#'
summary.ox <- function (ox_obj, form_info = TRUE) {
  ox_obj$data %>%
    select(form_oid, form_version, group_oid, event_oid) %>%
    arrange(form_oid, form_version, group_oid, event_oid) %>%
    unique() %>%
    mutate(`form_oid, form_version, group_oid` = paste(form_oid, form_version, group_oid, sep=", ")) -> k
  if (form_info == TRUE) {
    with(k, table(`form_oid, form_version, group_oid`, event_oid))
  } else {
    with(k, table(`form_oid, form_version, group_oid`, event_oid))
  }
}
