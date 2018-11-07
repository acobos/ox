#' Data for a group of items, as a tidy dataframe
#'
#' Returns a tidy dataframe from a \code{ox} object, containing data for all
#' items of the specified \code{group}. In the resulting dataframe, each item is
#' a variable, and each row is an observation; dataframe variables are
#' identified by their \code{item_oid} (or optionally, by their \code{item_name}),
#' and are one of the following classes: \code{Date}, \code{numeric}, or
#' \code{character}. Optionally, items with codelists can be defined as
#' \code{factor}.
#'
#' @param ox_obj An object of class \code{ox}, as returned by \code{ox::ox()}.
#'
#' @param group A group of items, as \code{character} value. Must be one
#' of the \code{group_oid} values in \code{ox_obj$metadata$group_def}.
#'
#' @param define_factors A \code{logical} value. When \code{TRUE}, items
#' with codelists are defined as factors using the codelist. Defaults to
#' \code{FALSE}.
#'
#' @param use_item_names A \code{logical} value. When \code{TRUE},
#' \code{item_name} in \code{ox_obj$metadata$item_def} are used as variable
#' names in the resulting dataframe. Othewise, \code{item_oid} are used.
#' Defaults to \code{FALSE}.
#'
#' @return A dataframe with study, subject, event, form, group and group repeat keys,
#' plus all items in \code{group} as variables.
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
#'
#' # Item groups
#' unique(my_study$metadata$group_def$group_oid)
#'
#' # Extract data for a group
#' demo <- ox_xtract_group(my_study,
#'                         group = "IG_DEMO_DEMOGRAPHICDATA")
#'
#' # Same, using item names to identify vars, and
#' # defining factors for items with codelist
#' demo <- ox_xtract_group(my_study,
#'                         group = "IG_DEMO_DEMOGRAPHICDATA",
#'                         define_factors = TRUE,
#'                         use_item_names = TRUE)
#'
ox_xtract_group <- function (ox_obj, group,
                             define_factors = FALSE,
                             use_item_names = FALSE) {

  if ( class(ox_obj)[1] != "ox") {
    stop("ox_obj should be an object of class ox", call. = FALSE)
  }

  if ( class(group) != "character" | length(group) != 1 |
       !group %in% ox_obj$metadata$group_def$group_oid ) {
    stop("ox_obj should be an object of class ox", call. = FALSE)
  }

  if ( class(define_factors) != "logical" | length(define_factors) > 1) {
    stop("ox_obj should be an object of class ox", call. = FALSE)
  }

  if ( class(use_item_names) != "logical" | length(use_item_names) > 1) {
    stop("ox_obj should be an object of class ox", call. = FALSE)
  }

  # to denormalize the group data ----
  ox_obj$data %>%
    dplyr::filter(group_oid == group) %>%
    dplyr::select(study_oid,
                  subject_key,
                  # subject_id,
                  event_oid,
                  event_repeat_key,
                  form_oid,
                  group_oid,
                  item_oid,
                  group_repeat_key,
                  value) %>%
    tidyr::spread(item_oid , value) -> k

  # to define the var order of non-key vars
  ox_obj$metadata$item_ref %>%
    dplyr::filter(group_oid == group) %>%
    dplyr::arrange(item_order_number) %>%
    dplyr::pull(item_oid) -> vars_in_order

  # to identify key vars
  key_vars <- names(k)[!(names(k) %in% vars_in_order)]

  # basic output ----
  k %>%
    dplyr::select(dplyr::one_of(key_vars),
                  dplyr::one_of(vars_in_order)) -> res


  # all item values are character; some codelist values may be character
  # the match would be problematic if we change the item value type before
  # defining factors. That's why factor definition comes first.

  # define_factors ----
  if (define_factors == TRUE) {
    # res <- define_factors(res)
    ox_obj$metadata$codelist_ref %>%
      dplyr::left_join(ox_obj$metadata$codelist_item) %>%
      dplyr::select(item_oid, codelist_oid, coded_value, code_label) -> dic

    # identify vars with codelist
    vars_with_cl <- names(res)[names(res) %in% unique(dic$item_oid)]

    for (i in vars_with_cl) {

      # subset codelist for var
      dic %>%
        dplyr::filter(item_oid == i) -> var_dic

      # define factor
      res[[i]] <- factor(res[[i]],
                         levels = var_dic$coded_value,
                         labels = var_dic$code_label)

    }
  }

  # define vartypes ----
  ox_obj$metadata$item_def %>%
    dplyr::select(item_oid, item_name, item_data_type, item_significant_digits) %>%
    dplyr::filter(item_oid %in% names(res)[7:length(res)]) -> item_info

  # dates
  item_info %>%
    dplyr::filter(item_data_type == "date") %>%
    dplyr::pull(item_oid) -> dates

  if (length(dates) > 0) {
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- as.Date(res[[dates[i]]])
    }
  }

  # numerics
  item_info %>%
    dplyr::filter(item_data_type %in% c("integer","float")) %>%
    dplyr::pull(item_oid) -> numerics

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
