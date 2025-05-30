% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rf_cv.R
\name{rf_cv}
\alias{rf_cv}
\title{Run Random Forest with Cross-Validation and Evaluation Metrics}
\usage{
rf_cv(formula, data, k = 5, seed = 42)
}
\arguments{
\item{formula}{A formula specifying the model structure}

\item{data}{A data frame containing the variables used in the formula}
}
\value{
A list containing the cross-validation results and evaluation metrics
}
\description{
Run Random Forest with Cross-Validation and Evaluation Metrics
}
