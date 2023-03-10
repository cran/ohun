#' @title Detects the start and end of sound events
#'
#' @description \code{energy_detector} detects the start and end of sound events based on energy and time attributes
#' @usage energy_detector(files = NULL, envelopes = NULL, path = ".", hop.size = 11.6, wl = NULL,
#' thinning = 1, bp = NULL, smooth = 5, threshold = 5, peak.amplitude = 0,
#' hold.time = 0, min.duration = 0, max.duration = Inf, cores = 1, pb = TRUE)
#' @param files Character vector indicating the sound files that will be analyzed. Optional. If 'files' and 'envelopes' are not supplied then the function will work on all supported format sound files in the working directory.
#' @param envelopes An object of class 'envelopes' (generated by \code{\link{get_envelopes}}) containing the amplitude envelopes of the sound files to be analyzed. If 'files' and 'envelopes' are not supplied then the function will work on all supported format sound files in the working directory.
#' @param path Character string containing the directory path where the sound files are located.
#'The current working directory is used as default.
#' @param hop.size A numeric vector of length 1 specifying the time window duration (in ms). Default is 11.6 ms, which is equivalent to 512 wl for a 44.1 kHz sampling rate. Ignored if 'wl' is supplied.
#' @param wl A numeric vector of length 1 specifying the window length of the spectrogram. Default is \code{NULL}. If supplied, 'hop.size' is ignored. Used internally for bandpass filtering (so only applied when 'bp' is supplied).
#' @param thinning Numeric vector of length 1 in the range 0~1 indicating the proportional reduction of the number of
#' samples used to represent amplitude envelopes (i.e. the thinning of the envelopes). Usually amplitude envelopes have many more samples
#' than those needed to accurately represent amplitude variation in time, which affects the size of the
#' output (usually very large R objects / files). Default is \code{1} (no thinning). Higher sampling rates can afford higher size reduction (e.g. lower thinning values). Reduction is conducted by interpolation using \code{\link[stats]{approx}}. Note that thinning may decrease time precision, and the higher the thinning the less precise the time detection. This argument is used internally by \code{\link{get_envelopes}}. Not used if 'envelopes' are supplied.
#' @param bp Numeric vector of length 2 giving the lower and upper limits of a
#'   frequency bandpass filter (in kHz). Default is \code{NULL}. This argument is used internally by \code{\link{get_envelopes}}. Not used if 'envelopes' are supplied.
#' @param smooth A numeric vector of length 1 to smooth the amplitude envelope
#'   with a sum smooth function. It controls the time 'neighborhood' (in ms) in which amplitude samples are smoothed (i.e. averaged with neighboring samples). Default is 5. 0 means no smoothing is applied. Note that smoothing is applied before thinning (see 'thinning' argument). This argument is used internally by \code{\link{get_envelopes}}. Not used if 'envelopes' are supplied.
#' @param threshold Numeric vector of length 1 with a value between 0 and 100 specifying the amplitude threshold for detecting sound event occurrences. Amplitude is represented as a percentage so 0 and 100 represent the lowest amplitude and highest amplitude respectively. Default is 5.
#' @param peak.amplitude Numeric vector of length 1 with the minimum peak amplitude value. Detections below that value are excluded. Peak amplitude is the maximum sound pressure level (in decibels) across the sound event (see \code{\link[warbleR]{sound_pressure_level}}). This can be useful when expecting higher peak amplitude in the target sound events compared to non-target sound events or when keeping only the best examples of the target sound events. Default is 0.
#' @param hold.time Numeric vector of length 1. Specifies the time range (in ms) at which selections will be merged (i.e. if 2 selections are separated by less than the specified 'hold.time' they will be merged in to a single selection). Default is \code{0} (no hold time applied).
#' @param min.duration Numeric vector of length 1 giving the shortest duration (in
#'   ms) of the sound events to be detected. It removes sound events below that
#'   threshold. If 'hold.time' is supplied sound events are first merged and then filtered by duration. Default is 0 (i.e. no filtering based on minimum duration).
#' @param max.duration Numeric vector of length 1 giving the longest duration (in
#'   ms) of the sound events to be detected. It removes sound events above that
#'   threshold. If 'hold.time' is supplied sound events are first merged and then filtered by duration.  Default is \code{Inf} (i.e. no filtering based on maximum duration).
#' @param cores Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return The function returns a 'selection_table' (warbleR package's formats, see \code{\link[warbleR]{selection_table}}) or data frame (if sound files can't be found) containing the start and end of each sound event by
#'   sound file. If no sound event was detected for a sound file it is not included in the output data frame.
#' @export
#' @name energy_detector
#' @details This function detects the time position of target sound events based on energy and time thresholds. It first detect all sound above a given energy threshold (argument 'energy'). If 'hold.time' is supplied then detected sounds are merged if necessary. Then the sounds detected are filtered based on duration attributes ('min.duration' and 'max.duration'). If 'peak.amplitude' is higher than 0 then only those sound events with higher peak amplitude are kept. Band pass filtering ('bp'), thinning ('thinning') and envelope smoothing ('smooth') are applied (if supplied) before threshold detection.
#'
#' @examples \donttest{
#' # Save example files into temporary working directory
#' data("lbh1", "lbh2", "lbh_reference")
#' writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))
#' writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))
#'
#' # using smoothing and minimum duration
#' detec <- energy_detector(files = c("lbh1.wav", "lbh2.wav"),
#' path = tempdir(), threshold = 60, smooth = 6.8,
#' bp = c(2, 9), hop.size = 6.8, min.duration = 0.09)
#'
#' # diagnose detection
#' diagnose_detection(reference = lbh_reference,
#' detection = detec)
#'
#' # without declaring 'files'
#' detec <- energy_detector(path = tempdir(), threshold = 60, smooth = 6.8,
#' bp = c(2, 9), hop.size = 6.8, min.duration = 90)
#'
#' # diagnose detection
#' diagnose_detection(reference = lbh_reference,
#' detection = detec)
#'
#' # using hold time
#' detec <- energy_detector(threshold = 10, hold.time = 150,
#' bp = c(2, 9), hop.size = 6.8, path = tempdir())
#'
#' # diagnose detection
#' diagnose_detection(reference = lbh_reference, detection = detec)
#'
#' # calculate envelopes first
#' envs <- get_envelopes(bp = c(2, 9), hop.size = 6.8, path = tempdir())
#'
#' # then run detection providing 'envelopes' (but no 'files')
#' detec <- energy_detector(envelopes = envs, threshold = 10, hold.time = 150, min.duration = 50)
#'
#' # diagnose detection
#' diagnose_detection(reference = lbh_reference, detection = detec, time.diagnostics = TRUE)
#'
#' \dontrun{
#' # USIN OTHER SOUND FILE FORMAT (flac program must be installed)
#'  # fisrt convert files to flac
#'  warbleR::wav_2_flac(path = tempdir())
#'
#'  # change sound file extension to flac
#'  flac_reference <- lbh_reference
#'  flac_reference$sound.files <- gsub(".wav", ".flac", flac_reference$sound.files)
#'
#'  # run detection
#'  detec <- energy_detector(files = c("lbh1.flac", "lbh2.flac"), path = tempdir(), threshold = 60,
#'  smooth = 6.8, bp = c(2, 9), hop.size = 6.8, min.duration = 90)

#'
#'  # diagnose detection
#'  diagnose_detection(reference = flac_reference, detection = detec)
#'  }
#' }
#'
#' @references {
#' Araya-Salas, M. (2021), ohun: diagnosing and optimizing automated sound event detection. R package version 0.1.0.
#' }
#' @seealso \code{\link{optimize_energy_detector}}
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr}). Implements a
#' modified version of the timer function from seewave.
#last modification on feb-2022 (MAS)

energy_detector <-
  function(files = NULL,
           envelopes = NULL,
           path = ".",
           hop.size = 11.6,
           wl = NULL,
           thinning = 1,
           bp = NULL,
           smooth = 5,
           threshold = 5,
           peak.amplitude = 0,
           hold.time = 0,
           min.duration = 0,
           max.duration = Inf,
           cores = 1,
           pb = TRUE
           ) {

    # save start time
    start_time <- proc.time()

    #check path if not provided set to working directory
    if (is.null(path))
      path <- getwd() else
      if (!dir.exists(path))
        stop2("'path' supplied does not exist") else
      path <- normalizePath(path)

        # hopsize
        if (!is.numeric(hop.size) | hop.size < 0) stop2("'hop.size' must be a positive number")

    #if bp is not vector or length!=2 stop
    if (!is.null(bp))
    {
      if (!is.vector(bp))
        stop2("'bp' must be a numeric vector of length 2")  else {
        if (!length(bp) == 2)
          stop2("'bp' must be a numeric vector of length 2")
      }
    }

    #if smooth is not vector or length!=1 stop
      if (!is.vector(smooth))
        stop2("'smooth' must be a numeric vector of length 1") else {
        if (!length(smooth) == 1)
          stop2("'smooth' must be a numeric vector of length 1")
      }

    #if thinning is not vector or length!=1 between 1 and 0
    if (!is.vector(thinning) | !is.numeric(thinning))
      stop2("'thinning' must be a numeric vector of length 1")
        if (thinning[1] > 1 | thinning[1] <= 0)
          stop2("'thinning' must be greater than 0 and lower than or equal to 1")

    #if wl is not vector or length!=1 stop
    if (!is.null(wl)){
      if (!is.vector(wl))
        stop2("'wl' must be a numeric vector of length 1") else {
        if (!length(wl) == 1)
          stop2("'wl' must be a numeric vector of length 1")
      }
    }

    #if threshold is not vector or length!=1 stop
      if (!is.numeric(threshold))
      stop2("'threshold' must be a numeric vector of length 1")
      if (!is.vector(threshold))
        stop2("'threshold' must be a numeric vector of length 1")
        if (!length(threshold) == 1)
          stop2("'threshold' must be a numeric vector of length 1")
        if(threshold >= 100 | threshold <= 0)
          stop2("'threshold' must be a number > 0 and < 100")

    #if files is not character vector
    if (!is.null(files) &
        any(!is.character(files),!is.vector(files)))
      stop2("'files' must be a character vector")

    #if cores is not numeric
    if (!is.numeric(cores))
      stop2("'cores' must be a numeric vector of length 1")
    if (any(!(cores %% 1 == 0), cores < 1))
      stop2("'cores' should be a positive integer")

    # check hold time
      if (!is.numeric(hold.time))
        stop2("'hold.time' must be a numeric vector of length 1")

    # if files and envelopes are not supplied
      if (is.null(files) & is.null(envelopes)){
          files <- list.files(path = path, pattern = "\\.wav$|\\.wac$|\\.mp3$|\\.flac$", ignore.case = TRUE)

          if (length(files) == 0)
          stop2("No files found in the working directory or 'path' supplied")
}

        # either files or envelopes must be supplied
        if (!is.null(files) & !is.null(envelopes))
          message("'files' will be ignored as 'envelopes' has been supplied")

        # get file names from envelopes
        if (is.null(files))
          files <- names(envelopes)[-length(envelopes)]

   # function for detecting sound events (i is the file name)
    detect_FUN <-
      function(file,
               wl,
               thres,
               peak.amplitude,
               min.duration,
               max.duration,
               path,
               bp,
               thinning,
               smooth,
               envlp
               )
      {

        # get envelope if not supplied
        if (is.null(envlp))
        envlp <- env_ohun_int(i = file,
                            path,
                            bp,
                            hop.size,
                            wl,
                            cores,
                            thinning,
                            pb,
                            smooth,
                            normalize = TRUE
        ) else envlp <- envelopes[[file]]

        # normalize to range
        if (max(envlp$envelope) > 1) {
          envlp$envelope <- envlp$envelope - min(envlp$envelope)
          envlp$envelope <- envlp$envelope / max(envlp$envelope)
        }

        # time interval between consecutive samples
        hop.samples <- envlp$duration / (length(envlp$envelope) - 1)

          # get times at which threshold is crossed
          cross_thresh <- unlist(lapply(2:length(envlp$envelope), function(x) {

            #positive means going up
            if (envlp$envelope[x] > thres & envlp$envelope[x - 1] <= thres) out <- hop.samples * (x - 1)
            # negative means going down
            if (envlp$envelope[x] <= thres & envlp$envelope[x - 1] > thres) out <- hop.samples * (x - 1) * -1
          # anything else should be null to save memory
            if (envlp$envelope[x] <= thres & envlp$envelope[x - 1] <= thres | envlp$envelope[x] > thres & envlp$envelope[x - 1] > thres) out <- NULL

              return(out)
          }))

          ## FIX IF START OR END OF sound eventS IS NOT INCLUDED IN SOUND FILE
          # get start and end of detections
          # starts are the positive ones
          starts <- cross_thresh[cross_thresh > 0]
          # and ends the negative ones (should be converted to positive)
          ends <- abs(cross_thresh[cross_thresh < 0])

          # if there is no end
          if (length(starts) > 0 & length(ends) == 0) ends <- envlp$duration

          # if there is no start
          if (length(ends) > 0 & length(starts) == 0) starts <- 0

          # if there are both starts and ends detected
          if (length(starts) > 0 & length(ends) > 0){

            # if start is not lower in the first detection
                if (starts[1] > ends[1]) starts <- c(0, starts)

                # if end is not higher in the last
          if (starts[length(starts)] > ends[length(ends)]) ends <- c(ends, envlp$duration)

          #put time of detection in data frame
          detections_df <-
            data.frame(
              sound.files = file,
              duration =  ends - starts,
              selec = NA,
              start = starts,
              end = ends,
              stringsAsFactors = FALSE
            )

          # add row names
          if (nrow(detections_df) > 0)
            detections_df$selec <- 1:nrow(detections_df)

          } else # return NAs
            detections_df <-
            data.frame(
              sound.files = file,
              duration =  NA,
              selec = NA,
              start = NA,
              end = NA,
              stringsAsFactors = FALSE
            )

          # TIME FILTERS
          # if something was detected applied time filters
           if (nrow(detections_df) > 0) {

            ## HOLD TIME MERGING
            # merge selections based on hold time
            if (hold.time > 0 & nrow(detections_df) > 1) {

              # empty column to tag rows to be merged
              detections_df$ovlp.sels <- NA

              # calculate overlapping selection after adding hope time
              for(e in 1:(nrow(detections_df) - 1)) {
                # if overlap
                  if (detections_df$end[e] + hold.time / 1000 >= detections_df$start[e + 1]){

                    # return 1 if is the first merging
                    if (all(is.na(detections_df$ovlp.sels))) detections_df$ovlp.sels[c(e, e + 1)] <- 1

                    # if current (e) overlapping with previous one
                    if (is.na(detections_df$ovlp.sels[e]))
                      detections_df$ovlp.sels[c(e, e + 1)] <- max(detections_df$ovlp.sels, na.rm = TRUE) + 1

                    # if overlapping with previous one use same tag
                    detections_df$ovlp.sels[e + 1] <- detections_df$ovlp.sels[e]
                }
              }

              # subset non-overlapping and overlapping
              no_ovlp <- detections_df[is.na(detections_df$ovlp.sels), ]
              ovlp <- detections_df[!is.na(detections_df$ovlp.sels), ]

              # if some overlaps detected
              if (nrow(ovlp) > 0)  {

                # loop to merge selections
                out <- lapply(X = unique(ovlp$ovlp.sels), FUN = function(x) {
                    # subset for one level
                    Y <- ovlp[ovlp$ovlp.sels == x, ]

                    # keep only one per overlapping group label
                    Z <- Y[1, , drop = FALSE]

                    # start is the minimum of all starts
                    Z$start <- min(Y$start)

                    # end is the maximum of all ends
                    Z$end <- max(Y$end)

                    return(Z)
                  })

                # put list together in a data frame
                ovlp <- do.call(rbind, out)

                # add non-overlapping selections
                detections_df <- rbind(ovlp, no_ovlp)

                # order selections by sound file and time
                detections_df <- detections_df[order(detections_df$start), ]

                # relabel selec column
                detections_df$selec <- 1:nrow(detections_df)

                # recalculate duration (gets messed up when using hold time)
                detections_df$duration[!is.na(detections_df$start)] <- round(detections_df$end[!is.na(detections_df$start)] - detections_df$start[!is.na(detections_df$start)], 7)

              } else detections_df <- no_ovlp # if not return non-overlapping
            }

            #remove sound events based on duration
            if (min.duration > 0)
              detections_df <- detections_df[detections_df$duration > min.duration / 1000, ]
            if (max.duration < Inf)
              detections_df <- detections_df[detections_df$duration < max.duration / 1000, ]
          }

          # remove extra column
          detections_df$ovlp.sels <- NULL

          # measure peak.amplitude
          if (peak.amplitude > 0){
            detections_df <- warbleR::sound_pressure_level(detections_df, parallel = 1, path = path, pb =  FALSE, type = "peak")

            detections_df <- detections_df[detections_df$SPL > peak.amplitude, ]

            # remove extra column
            detections_df$SPL <- NULL
            }
          return(detections_df)
          }

    #Apply over each sound file
    # set clusters for windows OS
    if (Sys.info()[1] == "Windows" & cores > 1)
      cl <- parallel::makeCluster(cores) else
      cl <- cores

    # run function over sound files or selections in loop
    detections_l <- warbleR:::pblapply_wrblr_int(pbar = pb,
      X = files,
      cl = cl,
      FUN = function(file)
      {
        out <- detect_FUN(file,
              wl,
              thres = threshold / 100,
              peak.amplitude,
              min.duration,
              max.duration,
              path,
              bp,
              thinning,
              smooth,
             envlp = envelopes
              )
        return(out)

      }
    )

    # put together in a single data frame
    detections <- do.call(rbind, detections_l)

    # remove NAs in detections
    detections <- detections[!is.na(detections$sound.files), ]

     #rename rows
    if (nrow(detections) > 0)
    rownames(detections) <- 1:nrow(detections)


      if (all(detections$sound.files %in% list.files(path = path)) & nrow(detections > 0)){

        detections <- selection_table(X = detections[!is.na(detections$start), ], path = path, parallel = cores, pb = FALSE, verbose = FALSE, fix.selec = TRUE)

      attributes(detections)$call <- base::match.call()

      attributes(detections)$elapsed.time.s <- as.vector((proc.time() - start_time)[3])
      }

    return(detections)
}
