# Helper function to get namespace alias

.ns_alias <- function (parsed_xml, alias) {

  x <- c(ns = xmlNamespaces(parsed_xml)[[1]]$uri,
         oc = xmlNamespaces(parsed_xml)$OpenClinica$uri,
         OpenClinica = xmlNamespaces(parsed_xml)$OpenClinica$uri)

  # return
  x[alias]
}

