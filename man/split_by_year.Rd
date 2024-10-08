% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_dssat.R
\name{split_by_year}
\alias{split_by_year}
\title{Split a multiyear crop experiment dataset by year}
\usage{
split_by_year(ls)
}
\arguments{
\item{ls}{a list of data frames containing ICASA-structured crop experiment data}
}
\value{
a list containing n lists of crop experiment data tables, where n is the number of years in the experiment
}
\description{
Handles all artefacts created by spitting the original data (e.g., empty data frames, columns with only NAs)
}
