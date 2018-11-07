#' Summary for \code{ox} objects
#'
#' Produces a summary of an \code{ox} object, including minimal information on
#' datapoints, subjects, items, groups, forms and events.
#'
#' @param ox_obj An object of class \code{ox}, as returned by \code{ox()}.
#'
#' @return A list with the following components:
#'
#' \itemize{
#'   \item \code{numbers}: numbers of datapoints, subjects, sites, events, forms,
#' groups, and items, in  \code{ox_obj$data}.
#'   \item \code{events}: unique values of \code{event_oid} in \code{ox_obj$data}.
#'   \item \code{forms}: unique values of \code{form_oid} in \code{ox_obj$data}.
#'   \item \code{groups}: unique values of \code{group_oid} in \code{ox_obj$data}.
#'   \item \code{assessments}: table of \code{event_oid} by \code{group_oid}, in
#'   \code{ox_obj$data}, Shows the number of datapoints (rows in
#' \code{ox_obj$data}) per event-group combination.
#' }
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
#' # Creating ox object and getting summary info
#' my_ox_obj <- ox(doc)
#' summary(my_ox_obj)
#'
summary.ox <- function (ox_obj) {

  list(numbers = c(datapoints = nrow(d$data),
                   subjects = length(unique(d$data$subject_key)),
                   sites = length(unique(d$data$study_oid)),
                   events = length(unique(d$data$event_oid)),
                   forms = length(unique(d$data$form_oid)),
                   groups = length(unique(d$data$group_oid)),
                   items = length(unique(d$data$item_oid))),

       events = unique(d$data$event_oid),
       forms = unique(d$data$form_oid),
       groups = unique(d$data$group_oid),
       assessments = with(d$data, table(group_oid, event_oid))
  )
}


