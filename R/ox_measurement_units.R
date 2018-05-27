#' Title Measurement units
#'
#' @param parsed_xml an object of class XMLInternalDocument
#' @param name_encoding
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_measurement_units <- function (parsed_xml, name_encoding="UTF-8") {
  mu <- bind_rows(lapply(xpathApply(parsed_xml,
                                    "//ns:MeasurementUnit",
                                    namespaces = ox_alias_default_ns(parsed_xml),
                                    fun=xmlAncestors,
                                    xmlAttrs),
                         data.frame,
                         stringsAsFactors=FALSE))

  # %>%
  #   select(StudyOID = OID, OID = OID.1, Name)

  # Encoding(mu$Name) <- name_encoding

  mu
}
