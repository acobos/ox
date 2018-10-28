#' Title Creates a dataframe with data from an ItemGroup
#'
#' @param GroupOID ItemGroupOID as character.
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_xtract_group <- function (ox_obj, GroupOID,
                             items_with_codelist_as_factors = FALSE,
                             var_names = "item_oid") {

  # IMPROVEMENTS:
  # var types
  # nicer varnames: item_name (from item_def)

  # to get the data
  ox_obj$data %>%
    filter(group_oid == GroupOID) %>%
    select(study_oid,
           subject_id,
           event_oid,
           form_oid,
           group_oid,
           item_oid,
           group_repeat_key,
           value) %>%
    tidyr::spread(item_oid , value) -> k

  # to define the var order of non-key vars
  ox_obj$metadata$item_ref %>%
    filter(group_oid == GroupOID) %>%
    arrange(item_order_number) %>%
    pull(item_oid) -> vars_in_order

  # to identify key vars
  key_vars <- names(k)[!(names(k) %in% vars_in_order)]

  # output
  k %>%
    select(one_of(key_vars), one_of(vars_in_order)) -> res

#  define_factors
  if (items_with_codelist_as_factors == TRUE) {
    res <- define_factors(res)
  }

# define vartypes
ox_obj$metadata$item_def %>%
  select(item_oid, item_name, item_data_type, item_significant_digits) %>%
  filter(item_oid %in% names(res)[7:length(res)]) -> item_info

item_info %>%
  filter(item_data_type == "date") %>%
  pull(item_oid) -> dates

# dates
if (length(dates) > 0) {
  for (i in 1:length(dates)) {
    res[[dates[i]]] <- as.Date(res[[dates[i]]])
  }
}

# numerics
item_info %>%
  filter(item_data_type %in% c("integer","float")) %>%
  pull(item_oid) -> numerics

lapply(res, class)[7:length(res)] -> resvar_class
resvar_class[!resvar_class %in% c("factor", "Date")] -> k
names(k) -> no_factor_or_date

intersect(numerics, no_factor_or_date) -> convert_to_numeric

if (length(convert_to_numeric) > 0) {
  for (i in 1:length(convert_to_numeric)) {
    res[[convert_to_numeric[i]]] <- as.numeric(res[[convert_to_numeric[i]]])
  }
}


# varnames:   PENDING



# return
res

}

# To define factors ----
define_factors <- function(df) {

  ox_obj$metadata$codelist_ref %>%
    left_join(ox_obj$metadata$codelist_item) %>%
    select(item_oid, codelist_oid, coded_value, code_label) -> dic

  # identify vars with codelist
  vars_with_cl <- names(df)[names(df) %in% unique(dic$item_oid)]

  for (i in vars_with_cl) {

    # subset codelist for var
    dic %>%
      filter(item_oid == i) -> var_dic

    # define factor
    df[[i]] <- factor(df[[i]],
                      levels = var_dic$coded_value,
                      labels = var_dic$code_label)
  }

  df
}

