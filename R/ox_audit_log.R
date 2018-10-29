#' Extracts the Audit log from an ox object
#'
#' @param ox_obj An object created with ox()
#'
#' @return dataframe containing Audit log entries
#' @export
#'
#' @examples
#'
ox_audit_log <- function (ox_obj) {

  OpenClinica.ns <- c(OpenClinica=xmlNamespaces(doc)$OpenClinica$uri)

  bind_rows(lapply(xpathApply(doc, "//OpenClinica:AuditLog",
                    namespaces=OpenClinica.ns,
                    fun=xmlAncestors,xmlAttrs),
         data.frame,
         stringsAsFactors=FALSE))
}

