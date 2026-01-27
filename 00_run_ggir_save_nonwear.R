library(GGIR)
library(GGIRread)

validation_files <- list.files("./data/validation/raw/bin/", full.names = TRUE)
non_validation_files <- list.files("./data/raw/bin/", full.names = TRUE)
outputdir <- "./data/ggir"

for (file in c(validation_files, non_validation_files)) {
  
  id <- stringr::str_c((strsplit(basename(file), "-")[[1]][1]), collapse = "")
  fn <- file.path(outputdir, paste0("output_", id))
  if (stringr::str_detect(file, "validation")) {
    fn <- paste0(fn, "_validation")
    study_name <- paste0(id, "_validation")
  } else {
    study_name <- id
  }
  
  if (file.exists(fn)) {
    next
  }
  
  print(paste("Processing", id))
  GGIR(
    mode = c(1, 2, 3, 4, 5),
    datadir = file,
    outputdir = outputdir,
    studyname = study_name,
    do.report = c(2, 4, 5),
    overwrite = TRUE,
    windowsizes = c(5, 900, 3600),
    do.cal = TRUE,
    do.enmo = TRUE,
    do.anglez = TRUE,
    chunksize = 1,
    printsummary = TRUE,
    strategy = 1,
    hrs.del.start = 0,
    hrs.del.end = 0,
    maxdur = 32,
    includedaycrit = 7,
    qwindow = c(0, 24),
    mvpathreshold = c(100),
    excludefirstlast = FALSE,
    includenightcrit = 7,
    epochvalues2csv = TRUE,
    def.noc.sleep = 1,
    timethreshold = 5,
    anglethreshold = 5,
    ignorenonwear = TRUE,
    outliers.only = FALSE,
    criterror = 3,
    do.visual = TRUE,
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
    timewindow = c("WW"),
    visualreport_without_invalid = FALSE,
    visualreport = TRUE
  )  
}

folders <- list.files(outputdir)
df_nonwear <- purrr::map_dfr(folders, function(folder) {
  file <- list.files(file.path(outputdir, folder, "meta", "basic"), full.names = T)
  folder <- str_remove(folder, "output_")
  id <- str_remove(folder, "_validation")
  load(file)
  imp <- g.impute(M, I)
  out <- cbind(imp$metashort, ggir_is_worn = ifelse(imp$r5long == 0, "wear", "non-wear"))
  out$date_time <- out$timestamp
  out$id <- id
  return(out[!colnames(out) %in% c("timestamp")])
})

write.csv(df_nonwear, "./data/interim/data_ggir_nonwear.csv", row.names = FALSE)