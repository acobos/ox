#' Measurement units in a dataframe
#'
#' Returns a dataframe with measurement units from a parsed OpenClinica
#' odm1.3 .xml export file.
#'
#' @param parsed_xml An object of class \code{XMLInternalDocument}, as returned
#' by \code{XML::xmlParse()}.
#'
#' @return A dataframe.
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
#' # Measurement units in a dataframe
#' measurement_units <- ox_measurement_units(doc)
#' head(measurement_units)
ox_measurement_units <- function (parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  .attrs_node_and_ancestors(parsed_xml, "MeasurementUnit") %>%
    data.frame(stringsAsFactors = FALSE) -> res

  # Dropping unneded vars
  # NOT with dplyr::select, because they are NOT all  always present !
  res$FileOID <- NULL
  res$Description <- NULL
  res$CreationDateTime <- NULL
  res$FileType <- NULL
  res$ODMVersion <- NULL
  res$schemaLocation <- NULL
  res$MetaDataVersionOID <- NULL


  if ("OID" %in% names(res)) {
    names(res)[which(names(res) == "OID")] <- "study_oid"
  }


  if ("OID.1" %in% names(res)) {
    names(res)[which(names(res) == "OID.1")] <- "unit_oid"
  }

  if ("Name" %in% names(res)) {
    names(res)[which(names(res) == "Name")] <- "unit_name"
  }

  # change CamelCase by snake_case
  names(res) <- snakecase::to_snake_case(names(res))

  # return
  res
}
