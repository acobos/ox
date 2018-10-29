#' Summary method for ox objects
#'
#' @param ox object
#' @param form_info Logical. Indicates if summary should include
#'
#' @return A crosstabulation of groups of items (group_oid) and events
#'  (envent_oid). If form_info = TRUE, the rows of the table are combinations of
#'  form (form_oid), form version and group (group_oid).
#'
#' @export
#'
#' @examples
#'
summary.ox <- function (ox_obj, form_info = TRUE) {
  ox_obj$data %>%
    select(form_oid, form_version, group_oid, event_oid) %>%
    arrange(form_oid, form_version, group_oid, event_oid) %>%
    unique() %>%
    mutate(`form_oid, form_version, group_oid` = paste(form_oid, form_version, group_oid, sep=", ")) -> k

  if (form_info == TRUE) {
    str <- with(k, table(`form_oid, form_version, group_oid`, event_oid))
  } else {
    str <- with(k, table(`form_oid, form_version, group_oid`, event_oid))
  }

  list(data = c(datapoints = nrow(ox_obj$data),
                subjects = length(unique(ox_obj$data$subject_id)),
                sites = length(unique(ox_obj$data$study_oid))
                ),
       structure = str,
       item_info = "Pending"
       )
}

