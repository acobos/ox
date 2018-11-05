# S3 method for ox objects

summary.ox <- function (ox_obj, form_info = TRUE) {
  ox_obj$data %>%
    dplyr::select(form_oid, form_version, group_oid, event_oid) %>%
    dplyr::arrange(form_oid, form_version, group_oid, event_oid) %>%
    unique() %>%
    dplyr::mutate(`form_oid, form_version, group_oid` = paste(form_oid, form_version, group_oid, sep=", ")) -> k
  if (form_info == TRUE) {
    with(k, table(`form_oid, form_version, group_oid`, event_oid))
  } else {
    with(k, table(`group_oid`, event_oid))
  }
}
