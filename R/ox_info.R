#' Info on \code{ox_all} objects
#'
#' Provides asuperficial view of an \code{ox_all} object, including minimal
#' information on datapoints, subjects, items, (item) groups, forms and events.
#'
#' @param ox_obj An object of class \code{ox_all}, as returned by
#' \code{ox_all()}.
#'
#' @param assessments \code{logical} indicating if assessments (groups by
#' events) should be included in output. Default is \code{FALSE}.
#'
#' @return A list with the following components:
#'
#' \itemize{
#'   \item \code{numbers}: numbers of datapoints, subjects, sites, events, forms,
#' groups, and items, in  \code{ox_obj$data}.
#'   \item \code{events}: unique values of \code{event_oid} in \code{ox_obj$data}.
#'   \item \code{forms}: unique values of \code{form_oid} in \code{ox_obj$data}.
#'   \item \code{groups}: unique values of \code{group_oid} in \code{ox_obj$data}.
#'   \item \code{assessments}: when \code{assessments = TRUE}. A dataframe
#'   showing the number of datapoints (rows in \code{ox_obj$data}) per event_oid
#'   and group_oid.
#' }
#'
#' @export
#'
#' @examples
#' # The example odm1.3 xml file address
#' my_file <- system.file("extdata",
#'                        "odm1.3_full_example.xml",
#'                        package = "ox",
#'                        mustWork = TRUE)
#'
#' # Parsing the xml file
#' library(XML)
#' doc <- xmlParse(my_file)
#'
#' # Creating ox object
#' my_ox_obj <- ox_all(doc)
#'
#' # Getting summary info (default)
#' ox_info(my_ox_obj)
#'
#' # Same, including assessments table
#' ox_info(my_ox_obj, assessments = TRUE)
#'
ox_info <- function (ox_obj, assessments = FALSE) {

  res <- list(numbers = c(datapoints = nrow(ox_obj$data),
                          subjects = length(unique(ox_obj$data$subject_key)),
                          sites = length(unique(ox_obj$data$study_oid)),
                          events = length(unique(ox_obj$data$event_oid)),
                          forms = length(unique(ox_obj$data$form_oid)),
                          groups = length(unique(ox_obj$data$group_oid)),
                          items = length(unique(ox_obj$data$item_oid))),
              events = unique(ox_obj$data$event_oid),
              forms = unique(ox_obj$data$form_oid),
              groups = unique(ox_obj$data$group_oid))

  if (assessments) {
    ueg<- ox_obj$data %>%
      dplyr::select(event_oid, group_oid) %>%
      dplyr::mutate(event_oid = factor(event_oid,
                                levels = ox_obj$metadata$event_ref$event_oid))

    with(ueg, table(event_oid, group_oid)) %>%
      as.data.frame() %>%
      tidyr::spread(group_oid, Freq) -> res$assessments
  }

  # return
  res

}


