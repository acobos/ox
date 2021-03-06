% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ox_info.R
\name{ox_info}
\alias{ox_info}
\title{Info on \code{ox_all} objects}
\usage{
ox_info(ox_obj, assessments = FALSE)
}
\arguments{
\item{ox_obj}{An object of class \code{ox_all}, as returned by
\code{ox_all()}.}

\item{assessments}{\code{logical} indicating if assessments (groups by
events) should be included in output. Default is \code{FALSE}.}
}
\value{
A list with the following components:

\itemize{
  \item \code{numbers}: numbers of datapoints, subjects, sites, events, forms,
groups, and items, in  \code{ox_obj$data}.
  \item \code{events}: unique values of \code{event_oid} in \code{ox_obj$data}.
  \item \code{forms}: unique values of \code{form_oid} in \code{ox_obj$data}.
  \item \code{groups}: unique values of \code{group_oid} in \code{ox_obj$data}.
  \item \code{assessments}: when \code{assessments = TRUE}. A dataframe
  showing the number of datapoints (rows in \code{ox_obj$data}) per event_oid
  and group_oid.
}
}
\description{
Provides asuperficial view of an \code{ox_all} object, including minimal
information on datapoints, subjects, items, (item) groups, forms and events.
}
\examples{
# The example odm1.3 xml file address
my_file <- system.file("extdata",
                       "odm1.3_full_example.xml",
                       package = "ox",
                       mustWork = TRUE)

# Parsing the xml file
library(XML)
doc <- xmlParse(my_file)

# Creating ox object
my_ox_obj <- ox_all(doc)

# Getting summary info (default)
ox_info(my_ox_obj)

# Same, including assessments table
ox_info(my_ox_obj, assessments = TRUE)

}
