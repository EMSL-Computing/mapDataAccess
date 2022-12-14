% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/put_data.R
\name{put_data}
\alias{put_data}
\title{Send data object to a map_data_connection object and get back a UUID}
\usage{
put_data(con, data, ...)
}
\arguments{
\item{con}{\code{\link{map_data_connection}} object}

\item{data}{any R data object}
}
\value{
UUID that can be used for retrieving the data (see \code{\link{get_data}})
}
\description{
Send data object to a map_data_connection object and get back a UUID
}
\seealso{
\code{\link{get_data}}, \code{\link{map_data_connection}}
}
