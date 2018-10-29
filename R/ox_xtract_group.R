#' Title Creates a dataframe with data from an ItemGroup
#'
#' @param group ItemGroupOID as character.
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_xtract_group <- function (ox_obj, group,
                             define_factors = FALSE,
                             use_item_names = FALSE) {

  # to denormalize the group data ----
  ox_obj$data %>%
    filter(group_oid == group) %>%
    select(study_oid,
           subject_id,
           event_oid,
           form_oid,
           group_oid,
           item_oid,
           group_repeat_key,
           value) %>%
    mutate(group_repeat_key = as.numeric(group_repeat_key)) %>%
    tidyr::spread(item_oid , value) -> k

  # to define the var order of non-key vars
  ox_obj$metadata$item_ref %>%
    filter(group_oid == group) %>%
    arrange(item_order_number) %>%
    pull(item_oid) -> vars_in_order

  # to identify key vars
  key_vars <- names(k)[!(names(k) %in% vars_in_order)]

  # basic output ----
  k %>%
    select(one_of(key_vars), one_of(vars_in_order)) -> res


  # all item values are character; some codelist values may be character
  # the match would be problematic if we change the item value type before
  # defining factors. That's why factor definition comes first.

  # define_factors ----
  if (define_factors == TRUE) {
    # res <- define_factors(res)
    ox_obj$metadata$codelist_ref %>%
      left_join(ox_obj$metadata$codelist_item) %>%
      select(item_oid, codelist_oid, coded_value, code_label) -> dic

    # identify vars with codelist
    vars_with_cl <- names(res)[names(res) %in% unique(dic$item_oid)]

    for (i in vars_with_cl) {

      # subset codelist for var
      dic %>%
        filter(item_oid == i) -> var_dic

      # define factor
      res[[i]] <- factor(res[[i]],
                        levels = var_dic$coded_value,
                        labels = var_dic$code_label)

    }
  }

  # define vartypes ----
  ox_obj$metadata$item_def %>%
    select(item_oid, item_name, item_data_type, item_significant_digits) %>%
    filter(item_oid %in% names(res)[7:length(res)]) -> item_info

  # dates
  item_info %>%
    filter(item_data_type == "date") %>%
    pull(item_oid) -> dates

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

  # use item_names ----
  if (use_item_names == TRUE) {
    res_names <- names(res)[7:length(res)]

    for (i in 1:length(res_names)) {
      new_name <- item_info$item_name[item_info$item_oid == res_names[i]]
      res_names[i] <- ifelse(res_names[i] %in% item_info$item_oid,
                             item_info$item_name[item_info$item_oid == res_names[i]],
                             res_names[i])
    }
    names(res)[7:length(res)] <- res_names
  }

  # return ----
  res
}
