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


# To define factors ----
define_factors <- function(df) {

  ox_codelist_ref(doc) %>%
    left_join(ox_codelist_item(doc)) %>%
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

# define_factors(g) -> gg
