params <-
list(EVAL = FALSE)

## ---- eval = FALSE------------------------------------------------------------
#  
#  # install pacakge
#  remotes::install_github("maRce10/ohun")
#  
#  #load package
#  library(ohun)
#  

## ----global options, echo = FALSE, message=FALSE, warning=FALSE-------------------------

#load package
library(ohun)

load("../data/lbh2.rda")
load("../data/lbh1.rda")
load("../data/lbh_reference.rda")

# for spectrograms
par(mar = c(5, 4, 2, 2) + 0.1)

stopifnot(require(knitr))
options(width = 90)
opts_chunk$set(
  comment = NA,
  message = FALSE,
  warning = FALSE,
  # eval = if (isTRUE(exists("params"))) params$EVAL else FALSE,
  dev = "jpeg",
  dpi = 100,
  fig.asp = 0.4,
  fig.width = 6.5,
  out.width = "100%",
  fig.align = "center"
)


## ---- eval = TRUE-----------------------------------------------------------------------
# load example data
data("lbh1", "lbh2", "lbh_reference")

lbh_reference

## ---------------------------------------------------------------------------------------
# convert to data frame
as.data.frame(lbh_reference)

## ---- eval = TRUE-----------------------------------------------------------------------
# save sound file
writeWave(lbh1, file.path(tempdir(), "lbh1.wav"))

# save sound file
writeWave(lbh2, file.path(tempdir(), "lbh2.wav"))

# print spectrogram
label_spectro(wave = lbh1, reference = lbh_reference[lbh_reference$sound.files == "lbh1.wav", ], hop.size = 10, ovlp = 50, flim = c(1, 10))

# print spectrogram
label_spectro(wave = lbh2, reference = lbh_reference[lbh_reference$sound.files == "lbh2.wav", ], hop.size = 10, ovlp = 50, flim = c(1, 10))

## ---------------------------------------------------------------------------------------

lbh1_reference <-
  lbh_reference[lbh_reference$sound.files == "lbh1.wav",]

# diagnose
diagnose_detection(reference = lbh1_reference, detection = lbh1_reference)[, c(1:3, 7:9)]


## ---------------------------------------------------------------------------------------


# create new table
lbh1_detection <- lbh1_reference[3:9,]

# print spectrogram
label_spectro(
  wave = lbh1,
  reference = lbh1_reference,
  detection = lbh1_detection,
  hop.size = 10,
  ovlp = 50,
  flim = c(1, 10)
)

# diagnose
diagnose_detection(reference = lbh1_reference, detection = lbh1_detection)[, c(1:3, 7:9)]


## ---------------------------------------------------------------------------------------


# print spectrogram
label_spectro(
  wave = lbh1,
  detection = lbh1_reference,
  reference = lbh1_detection,
  hop.size = 10,
  ovlp = 50,
  flim = c(1, 10)
)

# diagnose
diagnose_detection(reference = lbh1_detection, detection = lbh1_reference)[, c(1:3, 7:9)]


## ---------------------------------------------------------------------------------------

# create new table
lbh1_detection <- lbh1_reference

# add 'noise' to start
set.seed(18)
lbh1_detection$start <-
  lbh1_detection$start + rnorm(nrow(lbh1_detection), mean = 0, sd = 0.1)

## print spectrogram
label_spectro(
  wave = lbh1,
  reference = lbh1_reference,
  detection = lbh1_detection,
  hop.size = 10,
  ovlp = 50,
  flim = c(1, 10)
)

# diagnose
diagnose_detection(reference = lbh1_reference, detection = lbh1_detection)


## ---------------------------------------------------------------------------------------

# diagnose with time diagnostics
diagnose_detection(reference = lbh1_reference[-1, ], detection = lbh1_detection[-10, ], time.diagnostics = TRUE)


## ---------------------------------------------------------------------------------------
# diagnose by sound file
diagnostic <-
  diagnose_detection(reference = lbh1_reference,
                     detection = lbh1_detection,
                     by.sound.file = TRUE)

diagnostic

## ---------------------------------------------------------------------------------------

# summarize
summarize_diagnostic(diagnostic)


## ---------------------------------------------------------------------------------------

# plot spectrogram and envelope
label_spectro(
  wave = cutw(
    lbh1,
    from = 0,
    to = 1.5,
    output = "Wave"
  ),
  ovlp = 90,
  hop.size = 10,
  flim = c(0, 10),
  envelope = TRUE
)


## ---------------------------------------------------------------------------------------

# install this package first if not installed
# install.packages("Sim.DiffProc")

#Creating vector for duration 
durs <- rep(c(0.3, 1), 5)

#Creating simulated song
set.seed(12)
simulated_1 <-
  warbleR::simulate_songs(
    n = 10,
    durs = durs,
    freqs = 5,
    sig2 = 0.01,
    gaps = 0.5,
    harms = 1,
    bgn = 0.1,
    path = tempdir(),
    file.name = "simulated_1",
    selec.table = TRUE,
    shape = "cos",
    fin = 0.3,
    fout = 0.35,
    samp.rate = 18
  )$wave


## ---- fig.height=4, fig.width=10--------------------------------------------------------

# plot spectrogram and envelope
label_spectro(wave = simulated_1,
              env = TRUE,
              fastdisp = TRUE)


## ---- fig.height=4, fig.width=10--------------------------------------------------------


# run detection
detection <-
  energy_detector(
    files = "simulated_1.wav",
    bp = c(2, 8),
    threshold = 50,
    smooth = 150,
    path = tempdir()
  )

# plot spectrogram and envelope
label_spectro(
  wave = simulated_1,
  envelope = TRUE,
  detection = detection,
  threshold = 50
)


## ---------------------------------------------------------------------------------------

detection


## ----eval = TRUE, echo = TRUE, fig.height=4, fig.width=10-------------------------------


# run detection
detection <-
  energy_detector(
    files = "simulated_1.wav",
    bp = c(1, 8),
    threshold = 50,
    min.duration = 500,
    smooth = 150,
    path = tempdir()
  )

# plot spectrogram
label_spectro(wave = simulated_1, detection = detection)


## ----eval = TRUE, echo = TRUE, fig.height=4, fig.width=10-------------------------------

# run detection
detection <- energy_detector(files = "simulated_1.wav", bp = c(1, 8),  threshold = 50, smooth = 150, max.duration = 500, path = tempdir())

# plot spectrogram
label_spectro(wave = simulated_1,  detection = detection)


## ---- fig.height=4, fig.width=10, eval = TRUE, echo = TRUE------------------------------

# Detecting 
detection <- energy_detector(files = "simulated_1.wav", bp = c(5, 8), threshold = 50, smooth = 150, path = tempdir())

# plot spectrogram
label_spectro(wave = simulated_1,  detection = detection)


## ---- fig.height=4, fig.width=10, eval = TRUE, echo = TRUE------------------------------

# Detect
detection <-
  energy_detector(
    files = "simulated_1.wav",
    bp = c(0, 6),
    threshold = 50,
    min.duration = 1,
    smooth = 150,
    path = tempdir()
  )

# plot spectrogram
label_spectro(wave = simulated_1,  detection = detection)


## ---- eval = TRUE, warning=FALSE, message=FALSE-----------------------------------------

#Creating simulated song
set.seed(12)

#Creating vector for duration
durs <- rep(c(0.3, 1), 5)

sim_2 <-
  sim_songs(
    n = 10,
    durs = durs,
    freqs = 5,
    sig2 = 0.01,
    gaps = 0.5,
    harms = 1,
    bgn = 0.1,
    path = tempdir(),
    file.name = "simulated_2",
    selec.table = TRUE,
    shape = "cos",
    fin = 0.3,
    fout = 0.35,
    samp.rate = 18,
    am.amps = c(1, 2, 3, 2, 0.1, 2, 3, 3, 2, 1)
  )

# extract wave object and selection table
simulated_2 <- sim_2$wave
sim2_sel_table <- sim_2$selec.table

# plot spectrogram
label_spectro(wave = simulated_2, envelope = TRUE)


## ---- eval = TRUE-----------------------------------------------------------------------

# detect sounds
detection <- energy_detector(files = "simulated_2.wav", threshold = 50, path = tempdir())

# plot spectrogram
label_spectro(wave = simulated_2, envelope = TRUE, threshold = 50, detection = detection)


## ---- eval = TRUE-----------------------------------------------------------------------

# detect sounds
detection <-
  energy_detector(
    files = "simulated_2.wav",
    threshold = 50,
    min.duration = 1,
    path = tempdir(),
    hold.time = 200
  )

# plot spectrogram
label_spectro(
  wave = simulated_2,
  envelope = TRUE,
  threshold = 50,
  detection = detection
)


## ---- eval = TRUE-----------------------------------------------------------------------

# detect sounds
detection <-
  energy_detector(
    files = "simulated_2.wav",
    threshold = 50,
    min.duration = 1,
    path = tempdir(),
    smooth = 350
  )

# plot spectrogram
label_spectro(
  wave = simulated_2,
  envelope = TRUE,
  threshold = 50,
  detection = detection,
  smooth = 350
)


## ---------------------------------------------------------------------------------------

optim_detection <-
  optimize_energy_detector(
    reference = sim2_sel_table,
    files = "simulated_2.wav",
    threshold = 50,
    min.duration = 1,
    path = tempdir(),
    smooth = c(100, 250, 350)
  )

optim_detection[, c(1, 2:5, 7:12, 17:18)]


## ---------------------------------------------------------------------------------------

feature_reference(reference = lbh_reference, path = tempdir())


## ---- eval = FALSE, echo = TRUE---------------------------------------------------------
#  
#  # get mean structure template
#  template <-
#    get_templates(reference = lbh1_reference, path = tempdir())
#  

## ---- fig.width=6, fig.height=5, eval = TRUE, echo = FALSE------------------------------

par(mar = c(5, 4, 1, 1))

# get mean structure template
template <-
  get_templates(reference = lbh1_reference, path = tempdir())


## ---------------------------------------------------------------------------------------

# get correlations
correlations <-
  template_correlator(templates = template,
                      files = "lbh1.wav",
                      path = tempdir())


## ---------------------------------------------------------------------------------------

# print
correlations


## ---------------------------------------------------------------------------------------

# run detection
detection <-
  template_detector(template.correlations = correlations, threshold = 0.4)

detection

## ---- warning=FALSE---------------------------------------------------------------------

# plot spectrogram
label_spectro(
  wave = lbh1,
  detection = detection,
  template.correlation = correlations$`lbh1.wav-10/lbh1.wav`,
  flim = c(0, 10),
  threshold = 0.4,
  hop.size = 10, ovlp = 50)


## ---------------------------------------------------------------------------------------

#diagnose
diagnose_detection(reference = lbh1_reference, detection = detection)


## ---------------------------------------------------------------------------------------

# run optimization
optimization <-
  optimize_template_detector(
    template.correlations = correlations,
    reference = lbh1_reference,
    threshold = seq(0.1, 0.5, 0.1)
  )

# print output
optimization

## ---------------------------------------------------------------------------------------

# run optimization
optimize_template_detector(
  template.correlations = correlations,
  reference = lbh1_reference,
  threshold = c(0.6, 0.7),
  previous.output = optimization
)


## ---------------------------------------------------------------------------------------

# get correlations
correlations <-
  template_correlator(
    templates = lbh_reference[c(1, 10),],
    files = c("lbh1.wav", "lbh2.wav"),
    path = tempdir()
  )

# run detection
detection <-
  template_detector(template.correlations = correlations, threshold = 0.5)

correlations <-
  template_correlator(
    templates = lbh_reference[c(1, 10),],
    files = c("lbh1.wav", "lbh2.wav"),
    path = tempdir()
  )


## ---------------------------------------------------------------------------------------

#diagnose
diagnose_detection(reference = lbh_reference, detection = detection)


## ---------------------------------------------------------------------------------------

# labeling detection
labeled <-
  label_detection(reference = lbh_reference, detection = detection)


## ---------------------------------------------------------------------------------------

table(labeled$detection.class)


## ---------------------------------------------------------------------------------------

# filter
filtered <- filter_detection(detection = labeled, by = "scores")

# diagnose
diagnose_detection(reference = lbh_reference, detection = filtered)

## ---- eval = FALSE, echo=FALSE----------------------------------------------------------
#  Observaciones:
#  
#  avoid having overlapping selections in reference (check with overlapping_sels())
#  
#  downsample to a freq range just enough for the sound events of interest
#  
#  use hop.size instead of wl
#  
#  after split_acoustic_data() another function that returns the position in the original unsplit sound file
#  
#  count number of detections per unit of time

## ----session info, echo=F---------------------------------------------------------------

sessionInfo()


