#' Title Creates a dataframe with data from an ItemGroup
#'
#' @param GroupOID ItemGroupOID as character.
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_group_to_df <- function (item_data, GroupOID) {

  # IMPROVEMENTS:
  # - nicer varnames: item_name (from item_def)
  # - factor definitions from codelists

  item_data %>%
    filter(group_oid == GroupOID) %>%
    select(study_oid,
           subject_id,
           event_oid,
           form_oid,
           group_oid,
           item_oid,
           group_repeat_key,
           value) %>%
    tidyr::spread(item_oid , value)
}

# # To define factors
#
# # identify vars with codelist
# ox_group_to_df(item_data, "IG_V1D01_UNGROUPED") -> g
#
#
# vars <- names(g)[names(g) %in% cl_r$item_oid]
#
# vars[1] -> var
#
# # get codelist for one var
# ox_codelist_ref(doc) %>%
#   filter(item_oid == var) %>% pull(codelist_oid) -> var_cl_oid
#
# ox_codelist_item(doc) %>%
#   filter(codelist_oid == var_cl_oid) %>%
#   select(coded_value, code_label) -> var_cl
#
# g[[var]] <- factor(g[[var]],
#                    levels = var_cl$coded_value,
#                    labels = var_cl$code_label)

