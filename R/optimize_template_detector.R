#' @title Optimize acoustic template detection
#'
#' @description \code{\link{optimize_template_detector}} optimizes acoustic template detection
#' @usage optimize_template_detector(template.correlations, reference, threshold,
#' cores = 1, pb = TRUE, by.sound.file = FALSE, previous.output = NULL)
#' @param template.correlations An object of class 'template_correlations' (generated by \code{\link{template_correlator}}) in which to optimize detections. Must contain data for all sound files as in 'reference'. It can also contain data for additional sound files. In this case the routine assumes that no sound events are found in those files, so detection from those files are all false positives.
#' @param reference Data frame or 'selection.table' (following the warbleR package format) with the reference selections (start and end of the sound events) that will be used to evaluate the performance of the detection, represented by those selections in 'detection'. Must contained at least the following columns: "sound.files", "selec", "start" and "end". \strong{It must contain the reference selections that will be used for detection optimization}.
#' @param threshold Numeric vector of length > 1 with values between 0 and 1 specifying the correlation threshold for detecting sound event occurrences (i.e. correlation peaks). Must be supplied. \strong{Several values should be supplied for optimization}.
#' @param cores Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar and messages. Default is \code{TRUE}.
#' @param by.sound.file Logical to control if diagnostics are calculated for each sound file independently (\code{TRUE}) or for all sound files combined (\code{FALSE}, default).
#' @param previous.output Data frame with the output of a previous run of this function. This will be used to include previous results in the new output and avoid recalculating detection performance for parameter combinations previously evaluated.
#' @return A data frame in which each row shows the result of a detection job for each cutoff value, including the following diagnostic metrics:
#' \itemize{
#'  \item \code{true.positives}: number of sound events in 'reference' that correspond to any detection. Matching is defined as some degree of overlap in time. In a perfect detection routine it should be equal to the number of rows in 'reference'.
#'  \item \code{false.positives}: number of detections that don't match any of the sound events in 'reference'. In a perfect detection routine it should be 0.
#'  \item \code{false.negatives}: number of sound events in 'reference' that were not detected (not found in 'detection'. In a perfect detection routine it should be 0.
#'  \item \code{split.positives}: number of sound events in 'reference' that were overlapped by more than 1 detection (i.e. detections that were split). In a perfect detection routine it should be 0.
#'  \item \code{merged.positives}: number of sound events in 'detection' that were overlapped by more than 1 detection (i.e. sound events that were merged). In a perfect detection routine it should be 0.
#'  \item \code{recall}: Proportion of sound events in 'reference' that were detected. In a perfect detection routine it should be 1.
#'  \item \code{precision}: Proportion of detections that correspond to sound events in 'reference' that were detected. In a perfect detection routine it should be 1.
#'  }
##' @export
#' @name optimize_template_detector
#' @details This function takes a a reference data frame or 'selection_table' ('X') and the output of \code{\link{template_correlator}} and estimates the detection performance for different detection parameter combinations. This is done by comparing the position in time of the detection to those of the reference selections. The function returns several diagnostic metrics to allow user to determine which parameter values provide a detection that more closely matches the selections in 'reference'. Those parameters can be later used for performing a more efficient detection using \code{\link{optimize_template_detector}}.
#'
#' @examples{
#' # Save sound files to temporary working directory
#' data("lbh1", "lbh2", "lbh_reference")
#' writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
#' writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))
#'
#' # template for the second sound file in 'lbh_reference'
#' templ <- lbh_reference[11, ]
#'
#' # generate template correlations
#' tc <- template_correlator(templates = templ, path = tempdir(),
#' files = "lbh2.wav")
#'
#' # using 2 threshold
#' optimize_template_detector(template.correlations = tc, reference =
#' lbh_reference[lbh_reference$sound.files == "lbh2.wav", ],
#' threshold = c(0.2, 0.5))
#'
#' # using several thresholds
#' optimize_template_detector(template.correlations = tc,
#' reference = lbh_reference[lbh_reference$sound.files == "lbh2.wav", ],
#'  threshold = seq(0.5, 0.9, by = 0.05))
#'
#'  # template for the first and second sound file in 'lbh_reference'
#'  templ <- lbh_reference[c(1, 11), ]
#'
#'  # generate template correlations
#'  tc <- template_correlator(templates = templ, path = tempdir(),
#'  files = c("lbh1.wav", "lbh2.wav"))
#'
#' optimize_template_detector(template.correlations = tc, reference =
#'   lbh_reference, threshold = seq(0.5, 0.7, by = 0.1))
#'
#'  # showing diagnostics by sound file
#'  optimize_template_detector(template.correlations = tc, reference =
#'  lbh_reference,
#'  threshold = seq(0.5, 0.7, by = 0.1), by.sound.file = TRUE)
#' }
#'
#' @references {
#' Araya-Salas, M. (2021), ohun: diagnosing and optimizing automated sound event detection. R package version 0.1.0.
#' }
#' @seealso \code{\link{optimize_energy_detector}}, \code{\link{template_correlator}}, \code{\link{template_detector}}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}).
#last modification on dec-21-2021 (MAS)
optimize_template_detector <-
  function(template.correlations,
           reference,
           threshold,
           cores = 1,
           pb = TRUE,
           by.sound.file = FALSE,
           previous.output = NULL) {
    if (!is(template.correlations, "template_correlations"))
      stop2(
        "'template.correlations' must be an object of class 'template_correlations' generated by template_detector()"
      )

    if (is_extended_selection_table(reference))
      stop2("This function cannot take extended selection tables ('reference' argument)")

    #if reference is not a data frame
    if (!any(is.data.frame(reference), is_selection_table(reference)))
      stop2("reference is not of a class 'data.frame' or 'selection_table'")

    #check if all columns are found
    if (any(!(
      c("sound.files", "selec", "start", "end") %in% colnames(reference)
    )))
      stop2(paste(paste(
        c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec",
                                                      "start", "end") %in% colnames(reference))], collapse =
          ", "
      ), "column(s) not found in 'reference'"))

    #if there are NAs in start or end stop
    if (any(is.na(c(reference$end, reference$start))))
      stop2("NAs found in start and/or end columns")

    #if end or start are not numeric stop
    if (any(!is(reference$end, "numeric"),
            !is(reference$start, "numeric")))
      stop2("'start' and 'end' must be numeric")

    #if any start higher than end stop
    if (any(reference$end - reference$start <= 0))
      stop2(paste(
        "Start is higher than or equal to end in",
        length(which(reference$end - reference$start <= 0)),
        "case(s)"
      ))

    # check that all sound files in reference have and correlation vector
    if (!all(reference$sound.files %in% sapply(strsplit(names(
      template.correlations
    )[-length(template.correlations)], "/"), `[`, 2)))
      stop2("Not all sound files in 'reference' are found in 'template.correlations'")

    # if previous output included
    if (!is.null(previous.output))
      threshold <-
        threshold[!threshold %in% previous.output$threshold]

    if (length(threshold) == 0) {
      cat(
        "all combinations were already evaluated on previous call to this function (based on 'pevious.output')"
      )

      diagnostics <- previous.output
    } else {
      cat(paste(length(threshold), "thresholds will be evaluated:"))
      cat("\n")

      # set pb options
      diagnostics_list <-
        warbleR:::pblapply_wrblr_int(
          pbar = pb,
          cl = 1,
          X = threshold,
          FUN = function(x) {
            detection <-
              as.data.frame(
                template_detector(
                  template.correlations = template.correlations,
                  threshold = x,
                  cores =  cores,
                  pb = FALSE,
                  verbose = FALSE
                )
              )

            # run diagnostic by template
            templates <-
              sapply(strsplit(names(template.correlations)[-length(template.correlations)], "/"), `[`, 1)

            sub_diagnostics_list <-
              lapply(X = unique(templates), function(r) {
                Q <-
                  diagnose_detection(
                    reference = reference,
                    detection = detection[detection$template == r,],
                    by.sound.file = by.sound.file,
                    cores = cores,
                    pb = FALSE
                  )

                # get column names
                col_names <- names(Q)[if (by.sound.file)
                  - 1 else
                    1:ncol(Q)]

                # add template
                Q$templates <- r

                # sort columns
                Q <-
                  Q[, c(if (by.sound.file)
                    "sound.files" else
                      NULL, "templates", col_names)]

                return(Q)
              })

            diagnostic <- do.call(rbind, sub_diagnostics_list)

            return(diagnostic)
          }
        )

      # put all in a single data frame
      diagnostics <- do.call(rbind, diagnostics_list)

      # summarize sound files
      threshold <-
        rep(threshold, each = nrow(diagnostics_list[[1]]))

      # add thresholds to output
      diagnostics <- data.frame(threshold = threshold, diagnostics)

      # add previous output
      if (!is.null(previous.output))
        diagnostics <- rbind(previous.output, diagnostics)

    }
    return(diagnostics)
  }
