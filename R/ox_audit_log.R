#' Audit log in a dataframe
#'
#' Returns a dataframe with study audit log from a parsed OpenClinica
#' odm1.3_full .xml export file.
#'
#' @param parsed_xml An object of class \code{XMLInternalDocument}, as returned
#' by \code{XML::xmlParse()}.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # The example odm1.3_full xml file address
#' my_file <- system.file("extdata",
#'                        "odm1.3_clinical_ext_example.xml",
#'                        package = "ox",
#'                        mustWork = TRUE)
#'
#' # Parsing the xml file
#' library(XML)
#' doc <- xmlParse(my_file)
#'
#' # Audit log in a dataframe
#' audit_log <- ox_audit_log(doc)
#' View(audit_log)
ox_audit_log <- function (parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  bind_rows(lapply(xpathApply(parsed_xml, "//OpenClinica:AuditLog",
                    namespaces = .ns_alias(parsed_xml, "OpenClinica"),
                    fun=xmlAncestors,xmlAttrs),
         data.frame,
         stringsAsFactors=FALSE))
}

