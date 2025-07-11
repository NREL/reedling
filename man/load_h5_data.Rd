% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load.R
\name{load_h5_data}
\alias{load_h5_data}
\title{Load data from outputs.h5 from ReEDS runs}
\usage{
load_h5_data(runs, resultname, folder = "outputs")
}
\arguments{
\item{runs}{datatable of runs to load outputs, generated from run_summary()}

\item{resultname}{data key to load from h5 file}

\item{folder}{optional parameter to specify folder where the h5 file resides (default: outputs)}
}
\value{
data table with results by run name
}
\description{
Collect data from outputs.h5 files for a set of ReEDS runs.
}
