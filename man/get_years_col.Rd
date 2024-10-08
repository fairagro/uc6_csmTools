% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identify_exp_design.R
\name{get_years_col}
\alias{get_years_col}
\title{Find years column in a design data frame based on temporal coverage metadata}
\usage{
get_years_col(df, start, end)
}
\arguments{
\item{df}{a data frame}

\item{start}{a character or date depicting the first day of first year of the experiment}

\item{end}{a character or date depicting the last day of last year of the experiment}
}
\value{
the name of the year column
is_date is from etl_utils.R
}
\description{
Find years column in a design data frame based on temporal coverage metadata
}
