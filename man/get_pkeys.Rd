% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identify_data_relationships.R
\name{get_pkeys}
\alias{get_pkeys}
\title{Identify the primary key(s) in a table}
\usage{
get_pkeys(df, alternates = FALSE)
}
\arguments{
\item{df}{a data frame}

\item{alternates}{logical; whether to include alternate primary keys in case when multiple column uniquely identfy the rows}
}
\value{
a character vectir containing the primary key(s) column name(s)
}
\description{
Identify the primary key(s) in a table
}
