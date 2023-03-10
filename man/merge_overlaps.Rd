% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_overlaps.R
\name{merge_overlaps}
\alias{merge_overlaps}
\title{Merge overlapping selections}
\usage{
merge_overlaps(X, pb = TRUE, cores = 1)
}
\arguments{
\item{X}{Data frame or 'selection.table' (following the warbleR package format) with selections (start and end of the soudn events). Must contained at least the following columns: "sound.files", "selec", "start" and "end".}

\item{pb}{Logical argument to control progress bar. Default is \code{TRUE}.}

\item{cores}{Numeric. Controls whether parallel computing is applied.
It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).}
}
\value{
If any time-overlapping selection is found it returns a data frame in which overlapping selections are collapse into a single selection.
}
\description{
\code{merge_overlaps} merges several overlapping selections a single selection
}
\details{
The function finds time-overlapping selection in reference tables and collapses them into a single selection. It can be useful to prepare reference tables to be used in an energy detection routine. In such cases overlapping selections are expected to be detected as a single sound. Therefore, merging them can be useful to prepare references in a format representing a more realistic expectation of how a pefect energy detection routine would look like.
}
\examples{
{
# load data
data("lbh_reference")

# nothing to merge
merge_overlaps(lbh_reference)

# create artificial overlapping selections
lbh_ref2 <- rbind(as.data.frame(lbh_reference[c(3, 10), ]), lbh_reference[c(3, 10), ])

lbh_ref2$selec <- 1:nrow(lbh_ref2)

merge_overlaps(lbh_ref2)
}
}
\references{
{
Araya-Salas, M. (2021), ohun: diagnosing and optimizing automated sound event detection. R package version 0.1.0.
}
}
\seealso{
\code{\link{summarize_diagnostic}}, \code{\link{label_detection}}
}
\author{
Marcelo Araya-Salas \email{marcelo.araya@ucr.ac.cr})
}
