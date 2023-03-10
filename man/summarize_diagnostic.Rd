% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_diagnostic.R
\name{summarize_diagnostic}
\alias{summarize_diagnostic}
\title{Summarize detection diagnostics}
\usage{
summarize_diagnostic(diagnostic, time.diagnostics = FALSE)
}
\arguments{
\item{diagnostic}{A data frame with the reference selections (start and end of the sound events) that will be used to evaluate the performance of the detection, represented by those selections in 'detection'. Must contained at least the following columns: "sound.files", "selec", "start" and "end".}

\item{time.diagnostics}{Logical argument to control if diagnostics related to the duration of the sound events ("mean.duration.true.positives", "mean.duration.false.positives", "mean.duration.false.negatives" and "proportional.duration.true.positives") are returned (if \code{TRUE}). Default is \code{FALSE}.}
}
\value{
A data frame, typically the output of a detection optimization function (\code{\link{diagnose_detection}}, \code{\link{optimize_energy_detector}}, \code{\link{optimize_template_detector}}) including the following detection performance diagnostics:
\itemize{
 \item \code{total.detections}: total number of detections
 \item \code{true.positives}: number of sound events in 'reference' that correspond to any detection. Matching is defined as some degree of overlap in time. In a perfect detection routine it should be equal to the number of rows in 'reference'.
 \item \code{false.positives}: number of detections that don't match (i.e. don't overlap with) any of the sound events in 'reference'. In a perfect detection routine it should be 0.
 \item \code{false.negatives}: number of sound events in 'reference' that were not detected (not found in 'detection'. In a perfect detection routine it should be 0.
 \item \code{split.positives}: number of sound events in 'reference' that were overlapped by more than 1 detection (i.e. detections that were split). In a perfect detection routine it should be 0.
 \item \code{merged.positives}: number of sound events in 'reference' that were overlapped by a detection that also overlaps with other sound events in 'reference' (i.e. sound events that were merged into a single detection). In a perfect detection routine it should be 0.
 \item \code{mean.duration.true.positives}: mean duration of true positives (in s). Only included when \code{time.diagnostics = TRUE}.
 \item \code{mean.duration.false.positives}: mean duration of false positives (in ms). Only included when \code{time.diagnostics = TRUE}.
 \item \code{mean.duration.false.negatives}: mean duration of false negatives (in ms). Only included when \code{time.diagnostics = TRUE}.
 \item \code{overlap.to.true.positives}: ratio of the time overlap of true positives in 'detection' with its corresponding reference sound event to the duration of the reference sound event.
 \item \code{proportional.duration.true.positives}: ratio of duration of true positives to the duration of sound events in 'reference'. In a perfect detection routine it should be 1. Based only on true positives that were not split or merged.
 \item \code{duty.cycle}: proportion of a sound file in which sounds were detected. Only included when \code{time.diagnostics = TRUE} and \code{path} is supplied. Useful when conducting energy-based detection as a perfect detection can be obtained with a very low amplitude threshold, which will detect everything, but will produce a duty cycle close to 1.
 \item \code{recall}: Proportion of sound events in 'reference' that were detected. In a perfect detection routine it should be 1.
 \item \code{precision}: Proportion of detections that correspond to sound events in 'reference'. In a perfect detection routine it should be 1.
 \item \code{f1.score}: Combines recall and precision as the harmonic mean of these two. Provides a single value for evaluating performance. In a perfect detection routine it should be 1.
 }
}
\description{
\code{summarize_diagnostic} summarizes detection diagnostics
}
\details{
The function summarizes a detection diagnostic data frame in which diagnostic parameters are shown split by (typically) a categorical column, usually sound files. This function is used internally by \code{\link{diagnose_detection}}.
}
\examples{
{
# load example selection tables

data("lbh_reference")

# run diagnose_detection() by sound file
diag <- diagnose_detection(reference = lbh_reference,
detection = lbh_reference[-1, ], by.sound.file = TRUE)

# summarize
summarize_diagnostic(diagnostic = diag)

# should be the same as this:
diagnose_detection(reference = lbh_reference,
detection = lbh_reference[-1, ], by.sound.file = FALSE)
}
}
\references{
{
Araya-Salas, M. (2021), ohun: diagnosing and optimizing automated sound event detection. R package version 0.1.0.
}
}
\seealso{
\code{\link{diagnose_detection}}
}
\author{
Marcelo Araya-Salas \email{marcelo.araya@ucr.ac.cr})
}
