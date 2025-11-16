library(GGIR)
library(GGIRread)

GGIR2(
  mode = c(1, 2, 3, 4, 5),
  datadir = "./data/raw/JD-ow-test__100336_2025-11-10 14-59-44.bin",
  outputdir = "./data/ggir",
  studyname = "SEC3",
  do.report = c(2, 4, 5),
  overwrite = TRUE,
  windowsizes = c(5, 600, 1800),
  do.cal = TRUE,
  do.enmo = TRUE,
  do.anglez = TRUE,
  chunksize = 1,
  printsummary = TRUE,
  #=====================
  # Part 2:
  ### strategy = 1 means select data based on hrs.del.start and hrs.del.end, strategy = 2 makes that only the data between the first midnight and the last midnight is used, 3 is most active days, 4 is only after 1st midnight ###
  #=====================
  strategy = 1,
  hrs.del.start = 0,
  hrs.del.end = 0,
  maxdur = 32,
  includedaycrit = 7,
  #originally set to 16
  qwindow = c(0, 24),
  mvpathreshold = c(100),
  excludefirstlast = FALSE,
  includenightcrit = 7,
  epochvalues2csv = TRUE,
  #originally set to 16
  #=====================
  # Part 3 + 4:
  ### I think outliers.only - do.visual only apply if sleep log data present, but keeping in for now ###
  #=====================
  def.noc.sleep = 1,
  timethreshold = 5,
  anglethreshold = 5,
  ignorenonwear = TRUE,
  outliers.only = FALSE,
  criterror = 3,
  do.visual = TRUE,
  #=====================
  # Part 5
  #=====================
  threshold.lig = c(30),
  threshold.mod = c(100),
  threshold.vig = c(400),
  boutcriter = 0.8,
  boutcriter.in = 0.9,
  boutcriter.lig = 0.8,
  boutcriter.mvpa = 0.8,
  boutdur.in = c(1, 10, 30),
  boutdur.lig = c(1, 10),
  boutdur.mvpa = c(1),
  includedaycrit.part5 = 2 / 3,
  save_ms5rawlevels = TRUE,
  save_ms5raw_format = "csv",
  save_ms5raw_without_invalid = FALSE,
  #=====================
  # Visual report
  #=====================
  timewindow = c("WW"),
  visualreport_without_invalid = FALSE,
  #If TRUE, then reports generated with visualreport = TRUE only show the windows with sufficiently valid data according to includedaycrit when viewingwindow = 1 or includenightcrit when viewingwindow = 2
  visualreport = TRUE
)
