#' Title Subject data
#'
#' @param parsed_xml an object of class XMLInternalDocument
#'
#' @return dataframe
#' @export
#'
#' @examples
#'
ox_subject_data <- function (parsed_xml) {

  sd <- xpathSApply(doc, "//ns:ClinicalData/ns:SubjectData",
                    namespaces=ox_alias_default_ns(parsed_xml),
                    fun=xmlAttrs)

  # function to get a df for each element in sd
  to_df <- function (x) {
    attr(x, "namespaces") <- NULL   # delete namspace attribute
    t(as.data.frame(x)) -> x_df     # as df
    names(x_df) <- attr(x, "names") # because names are lost
    row.names(x_df) <- NULL         # avoid rownames
    as.data.frame(x_df, stringsAsFactors=FALSE) # again, as df
  }

  bind_rows(lapply(sd, to_df))   # apply function to all list elements
}

