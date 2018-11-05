# Helper functions

# namespace alias
.ns_alias <- function (parsed_xml, alias) {

  x <- c(ns = XML::xmlNamespaces(parsed_xml)[[1]]$uri,
         oc = XML::xmlNamespaces(parsed_xml)$OpenClinica$uri,
         OpenClinica = XML::xmlNamespaces(parsed_xml)$OpenClinica$uri)

  # return
  x[alias]
}

