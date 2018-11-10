# Helper functions

# namespace alias
.ns_alias <- function (parsed_xml, alias) {

  x <- c(ns = XML::xmlNamespaces(parsed_xml)[[1]]$uri,
         oc = XML::xmlNamespaces(parsed_xml)$OpenClinica$uri,
         OpenClinica = XML::xmlNamespaces(parsed_xml)$OpenClinica$uri)

  # return
  x[alias]
}

# attributes of node and ancestors
.attrs_node_and_ancestors <- function (parsed_xml, node_name, ns_alias = "ns") {

  if (! "XMLInternalDocument" %in% class(parsed_xml)) {
    stop("parsed_xml should be an object of class XMLInternalDocument", call. = FALSE)
  }

  # validation of no_name PENDING !!

  # return
  dplyr::bind_rows(
    lapply(
      XML::xpathApply(parsed_xml,
                      paste0("//", ns_alias, ":", node_name), # "//ns:StudyEventDef",
                      namespaces = .ns_alias(parsed_xml = parsed_xml,
                                             alias = ns_alias),
                      fun = XML::xmlAncestors,
                      XML::xmlAttrs),
      data.frame, stringsAsFactors=FALSE))

}
