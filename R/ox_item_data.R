#' Item data in a dataframe
#'
#' Returns a dataframe with patient's data (all patients, all items, all groups,
#' all forms, all events), from a parsed OpenClinica odm1.3 .xml export file.
#' Be patient, the function is slow, even for small studies.
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
#' # Item data in a dataframe
#' d <- ox_item_data(doc)
#' head(d)
ox_item_data <- function(parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  message("Getting ItemData nodes...")
  nodes <- XML::xpathApply(parsed_xml,
                           "//ns:ItemData",
                           namespaces = .ns_alias(parsed_xml, "ns"))


  message("Extracting data from ItemData nodes...")

  # loop over nodes with a progress bar,
  # extract attributes for node an ancestors,
  # and bind_rows
  res <- pbapply::pblapply(nodes,
                    FUN = function (x) data.frame(XML::xmlAncestors(x, XML::xmlAttrs),
                                                  stringsAsFactors = FALSE)) %>%
    dplyr::bind_rows()

  # Dropping unneded vars
  # NOT with dplyr::select, because they are NOT all  always present !
  res$FileOID <- NULL
  res$Description <- NULL
  res$CreationDateTime <- NULL
  res$FileType <- NULL
  res$ODMVersion <- NULL
  res$schemaLocation <- NULL
  res$MetaDataVersionOID <- NULL

  # to numeric ----
  if (any("StudyEventRepeatKey" %in% names(res))) {
    res$StudyEventRepeatKey <- as.numeric(res$StudyEventRepeatKey)
  }

  if (any("ItemGroupRepeatKey" %in% names(res))) {
    res$ItemGroupRepeatKey <- as.numeric(res$ItemGroupRepeatKey)
  }

  if (any("StudySubjectID" %in% names(res))) {
    # only if no NA's resulting from type cohercion
    if (sum(is.na(as.numeric(res$StudySubjectID))) == 0) {
      res$StudySubjectID <- as.numeric(res$StudySubjectID)
    }
  }

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

