% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_db.R
\name{update_db}
\alias{update_db}
\title{Creates a SQLite database on the M drive}
\usage{
update_db()
}
\description{
Finds the fish Access database on the M drive and creates a new
  SQLite database, if the SQLite needs updating. The function will not work
  unless you are connected to the M drive. Most of the time the user
  shouldn't have to use this as a stand alone function, but use the argument
  in \code{\link{connect_fish_db}}
}
\examples{
Need to add...
}
\author{
Michael J. Dodrill, \email{mdodrill@usgs.gov}
}
