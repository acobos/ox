#' Title Creates a dataframe with data from an ItemGroup
#'
#' @param GroupOID ItemGroupOID as character.
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_group_to_df <- function (GroupOID, item_data, item_ref) {

  # IMPROVEMENTS:
  # - factor definitions from codelists
  # - nicer varnames: item_name (from item_def)

  # to get the data
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
    tidyr::spread(item_oid , value) -> k

  # to define the var order of non-key vars
  item_ref %>%
    filter(group_oid == GroupOID) %>%
    arrange(item_order_number) %>%
    pull(item_oid) -> vars_in_order

  # to identify key vars
  key_vars <- names(k)[!(names(k) %in% vars_in_order)]

  # output
  k %>%
    select(one_of(key_vars), one_of(vars_in_order))

}

# # verify: ----
# k <- ox_group_to_df("IG_CANCE_CANCERHISTOLOGYANDRECEPTOR",
#                     item_data,
#                     item_ref)
# dat <- item_data; # rm(item_data)
# items_reference <- item_ref; # rm(item_ref)
# kk <- ox_group_to_df("IG_CANCE_CANCERHISTOLOGYANDRECEPTOR",
#                      dat,
#                      items_reference)
# k <- ox_group_to_df("IG_IECRI_IECRITERIA",
#                     dat,
#                     items_reference)
# rm(dat, items_reference, k, kk)
# k <- ox_group_to_df("IG_CANCE_CANCERHISTOLOGYANDRECEPTOR", item_data, item_ref)


# # To define factors ----
#
# identify vars with codelist
# ox_group_to_df("IG_V1D01_UNGROUPED") -> g
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

