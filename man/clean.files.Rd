% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00_Pre-processing_functions.R
\name{clean.files}
\alias{clean.files}
\title{Create a clean dataframe from files}
\usage{
clean.files(filepath)
}
\arguments{
\item{filepath}{that contains all the data files}
}
\value{
a cleaned data frame consisting information about subjects
}
\description{
imports,processes, and cleans the files in the data folder to create an initial dataframe with experimental information about each subject
}
\examples{
 clean.files('C:\\\\Users\\\\meher\\\\Desktop\\\\EDneurolabwork\\\\data')
}
