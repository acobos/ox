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
#'                        "odm1.3_full_example_Optimal.xml",
#'                        package = "ox",
#'                        mustWork = TRUE)
#'
#' # Parsing the xml file
#' library(XML)
#' doc <- xmlParse(my_file)
#'
#' # Audit log in a dataframe
#' audit_log <- ox_audit_log(doc)
#' head(audit_log)
#'
ox_audit_log <- function (parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  nodes <- XML::xpathApply(parsed_xml,
                           "//OpenClinica:AuditLog",
                           namespaces = .ns_alias(parsed_xml, "OpenClinica"))

  if (!length(nodes) > 0) {
    stop("Sorry, audit_log data was not found.", call. = FALSE)
  }

  message("Extracting data from AuditLogs nodes...")

  # loop over nodes with a progress bar,
  # extract attributes for node an ancestors,
  # and bind_rows
  res <- pbapply::pblapply(nodes,
                           FUN = function (x) data.frame(XML::xmlAncestors(x, XML::xmlAttrs),
                                                         stringsAsFactors = FALSE)) %>%
    dplyr::bind_rows()


  # Dropping unneded vars
  # NOT with dplyr::select, just in case some of them is not present in future
  # odm1.3 exports
  res$FileOID <- NULL
  res$Description <- NULL
  res$CreationDateTime <- NULL
  res$FileType <- NULL
  res$ODMVersion <- NULL
  res$schemaLocation <- NULL
  res$MetaDataVersionOID <- NULL

  # change CamelCase by snake_case
  names(res) <- snakecase::to_snake_case(names(res))

  # simplify some varnames
  names(res) <- gsub("study_event", "event", names(res), fixed=TRUE)
  names(res) <- gsub("item_group", "group", names(res), fixed=TRUE)
  names(res) <- gsub("study_subject", "subject", names(res), fixed=TRUE)

  message("Done")

  #return
  res
}

