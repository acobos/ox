#' Group repetitions in a dataframe
#'
#' Returns a dataframe with study item group repetitions from a parsed OpenClinica
#' odm1.3 .xml export file.
#'
#' @param parsed_xml An object of class \code{XMLInternalDocument}, as returned
#' by \code{XML::xmlParse()}.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # The example xml file address
#' my_file <- system.file("extdata",
#'                        "odm1.3_full_example.xml",
#'                        package = "ox",
#'                        mustWork = TRUE)
#'
#' # Parsing the xml file
#' library(XML)
#' doc <- xmlParse(my_file)
#'
#' # Group repetitions in a dataframe
#' group_repeats <- ox_group_repeat(doc)
#' head(group_repeats)
#'
ox_group_repeat <- function(parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  .attrs_node_and_ancestors(parsed_xml, "ItemGroupRepeat", "oc") %>%
    dplyr::select(group_oid = OID.2,
                  show_group = ShowGroup,
                  repeat_number = RepeatNumber,
                  repeat_max = RepeatMax)
}

