#' Item references in a dataframe
#'
#' Returns a dataframe with study item references from a parsed OpenClinica
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
#'                        "odm1.3_full_example.xml",
#'                        package = "ox",
#'                        mustWork = TRUE)
#'
#' # Parsing the xml file
#' library(XML)
#' doc <- xmlParse(my_file)
#'
#' # Form references in a dataframe
#' item_ref <- ox_item_ref(doc)
#' head(item_ref)
#'
ox_item_ref <- function(parsed_xml) {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  .attrs_node_and_ancestors(parsed_xml, "ItemRef") %>%
    dplyr::select(group_oid = OID.2,
                  item_oid = ItemOID,
                  item_order_number = OrderNumber,
                  item_mandatory = Mandatory) %>%
    dplyr::mutate(item_order_number = as.numeric(item_order_number)) %>%
    dplyr::arrange(group_oid, item_order_number)

}


