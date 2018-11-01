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
#' # The example xml file address
#' file <- system.file("extdata",
#'                     "odm1.3_clinical_ext_example.xml",
#'                     package = "ox",
#'                     mustWork = TRUE)
#'
#' # Parsing the xml file
#' library(XML)
#' doc <- xmlParse(file_address)
#'
#' # Audit log in a dataframe
#' audit_log <- ox_audit_log(doc)
#' View(audit_log)
ox_audit_log <- function (ox_obj) {

  OpenClinica.ns <- c(OpenClinica=xmlNamespaces(doc)$OpenClinica$uri)

  bind_rows(lapply(xpathApply(doc, "//OpenClinica:AuditLog",
                    namespaces=OpenClinica.ns,
                    fun=xmlAncestors,xmlAttrs),
         data.frame,
         stringsAsFactors=FALSE))
}

