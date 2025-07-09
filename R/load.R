##############################################
## Load ReEDS run or repo data
##############################################

#' List ReEDS runs
#'
#' Prints the names of any ReEDS runs in a given run folder. A batch name
#' can also be specified to filter to select runs.
#'
#' @param path path to a set of ReEDS runs
#' @param batch optional prefix to filter runs; can be specified as a regex pattern
#' @examples
#' list_runs("path/to/ReEDS-2.0/runs")
#' list_runs("path/to/ReEDS-2.0/runs", "20241129_testrun")
#' @export
list_runs <- function(path, batch=NULL){
  check_dir_exists(path)
  folders <- basename(list.dirs(path, recursive=F))
  if(!is.null(batch)){
    folders <- folders[grepl(batch, folders)]
    cat(sprintf("ReEDS runs with %s at %s:\n", batch, path))
  } else {
    cat(sprintf("ReEDS runs at %s:\n", path))
  }
  run_count <- length(folders)
  cat(paste("-", folders), sep="\n")
  cat(sprintf("%d runs in total", run_count))
}

#' Summarize ReEDS runs
#'
#' Function to summarize metadata on set of ReEDS run in a given run folder, with the option to subset to runs that match
#' a specified batch prefix. The prefix can be specified as a regex string to select runs from multiple batches.
#'
#' The function returns a data table with the following information on the runs:
#' - run name (dropping any batch prefix if supplied)
#' - batch name (if supplied)
#' - whether files were detected in the 'outputs' folder
#' - the last ReEDS step that completed, as listed in meta.csv
#' - the run time in hours, calculated from meta.csv
#' - full path to the run
#'
#' @param path path to a set of ReEDS runs
#' @param batch optional batch prefix to filter runs; can be specified as a regex pattern
#' @return datatable object of ReEDS runs and their paths;
#' @import data.table
#' @import stringr
#' @examples
#' runs <- load_run_summary("path/to/ReEDS-2.0/runs", "20230416")
#'
#' # runs from multiple batches can be matched by using regex expressions
#' runs <- load_run_summary("path/to/ReEDS-2.0/runs", "20230416|20230422")
#' @export
run_summary <- function(path, batch=""){
  # check if directory exists
  check_dir_exists(path)
  # get folders at specified path
  runs <- list.dirs(path, recursive = F)
  # subset to runs with a batch if provided
  if(batch != ""){
    # batch batch can be a single string or a regex string (e.g., v2022_11_22|v2022_12_06)
    runs <- runs[grepl(batch, runs)]
  }
  # check for valid runs
  if (length(runs) == 0){
    stop("No runs found; specify a different path or batch name")
  }
  runs_base <- data.table::data.table(run=basename(runs), path=runs)

  # get timing and last step
    run_meta <- load_meta(runs_base, summarize=T)
    if(is.null(run_meta)){
      runs_out <- runs_base
      meta_cols <- c()
    } else {
      runs_out <- merge(runs_base, run_meta, all.x=T)
      meta_cols <- c("last_step", "runtime_hours")
    }

  # check for outputs.h5 file
  runs_out$outputs_h5 <- file.exists(file.path(runs_out$path, "outputs", "outputs.h5"))

  # separate batch from run name
  if(batch != ""){
    runs_out$run <- str_replace(runs_out$path, paste0(".*(", batch, ")_"), "")
    runs_out$batch <- str_extract(runs_out$path, batch)
    cols_out <- c("run", "batch", meta_cols, "outputs_h5", "path")
  } else {
    runs_out$run <- basename(runs_out$path)
    cols_out <- c("run", meta_cols, "outputs_h5", "path")
  }
  # subset outputs
  runs_out <- runs_out[, cols_out, with=F]
  return(runs_out)
}

#' Load run metadata
#'
#' Load and format meta.csv files from list of runs. Can provide detailed runtime by step or a
#' summary of total runtime and the last processing step for each run.
#'
#' @param runs data table object with run and path information, generated from run_summary()
#' @param summarize if TRUE summarizes run time and last processing step
#' @param skip_lines number of lines to skip in the metadata header
#'
#' @return data table object of run metadata
#' @import data.table
#' @export
load_meta <- function(runs, summarize=FALSE, skip_lines=3){
  check_cols(runs, c("run", "path"))
  meta_out <- list()
  for(run_id in 1:nrow(runs)){
    run_name <- runs[run_id]$run
    path_name <- runs[run_id]$path
    print(paste("Loading meta file for", run_name))
    tryCatch(
      {
        # read in meta.csv, skipping comments at top
        df <- data.table::fread(file.path(path_name, "meta.csv"), skip=skip_lines)
        df$run <- run_name
        # enforce date columns as POSIXct; this helps avoid
        # issues when combining meta files later
        df$starttime <- as.POSIXct(df$starttime)
        df$stoptime <- as.POSIXct(df$stoptime)
        meta_out <- append(meta_out, list(df))
      },
      error=function(cond)
      {
        print(paste("Error reading meta file for", run_name))
      }
    )
  }

  if (length(meta_out) == 0){
    return(NULL)
  } else {
    meta_out <- data.table::rbindlist(meta_out, fill=T)
    #drop rows with "end"
    meta_out <- meta_out[process!="end"]
    # aggregate by run
    if(summarize){
      seconds_per_hour <- 3600
      meta_out <- meta_out[, by=.(run), .(runtime_hours=round(sum(processtime)/seconds_per_hour,1), last_step=paste(process[length(process)], year[length(year)], sep="_"))]
    } else {
      setnames(meta_out, "processtime", "runtime_secs")
    }
    return(meta_out)
  }
}

#' List output files
#'
#' Prints a list of files in a folder of runs. Defaults to looking at the outputs folder of the first run specified.
#'
#' @param runs datatable of runs to list outputs, generated from run_summary()
#' @param folder optional parameter to specify folder to look for files (default: outputs)
#' @param runval optional parameter to specify name of specific run in runs to look at
#'
#' @import data.table
#' @examples
#' list_outputs(runs)
#' list_outputs(runs, runval="runname")
#' list_outputs(runs, folder="inputs_case")
#'
#' @export
list_outputs <- function(runs, folder="outputs", runval=NULL, keyword=NULL){
  # unless specified, get files from the first run
  check_cols(runs, c("path"))
  if (is.null(runval)){
    path <- runs[1]$path
  } else {
    path <- runs[run == runval]$path
  }
  # check if directory exists
  check_dir_exists(path)
  all_files <- list.files(file.path(path, folder))
  if(!is.null(keyword)){
    files_out <- all_files[grepl(keyword, all_files)]
    cat(sprintf("Outputs with %s keyword in %s:", keyword, path), sep="\n")
  } else {
    files_out <- all_files
    cat(sprintf("Outputs in %s:", path), sep="\n")
  }
  if(length(files_out)==0){
    cat("No outputs found.", sep="\n")
  } else{
    cat(files_out, sep="\n")
  }
}


#' Load outputs from ReEDS runs
#'
#' Collect csv output data from a set of ReEDS runs. Defaults to data in the "outputs" folder of the ReEDS runs,
#' but can also be used for data stored in "inputs_case" or other ReEDS run folders.
#'
#' @param runs datatable of runs to load outputs, generated from run_summary()
#' @param filename filename to load
#' @param folder optional parameter to specify folder where filename resides (default: outputs)
#'
#' @return data table with results by run name
#' @import data.table
#' @examples
#' cap <- load_run_d ata(runs, "cap.csv")
#'
#' # data from other folders can also be extracted
#' hierarchy <- load_run_data(runs, "hierarchy.csv", folder="inputs_case")
#' @export
load_run_data <- function(runs, filename, folder="outputs", header=T) {
  check_cols(runs, c("run", "path"))
  tic <- proc.time()
  # helpers for keeping track run output
  df_all <- list()
  newcolname_all <- c()

  for(run_id in 1:nrow(runs)){
    # collect run and path information
    run_name <- runs[run_id]$run
    run_folder <- runs[run_id]$path
    filepath <- file.path(run_folder, folder)
    # get list of files to load (for some may be multiple)
    if(grepl("\\*", filename)){
      files <- Sys.glob(file.path(filepath, filename))
    } else {
      files <- file.path(filepath, filename)
    }
    # iterate over files for this run
    for (file in files){
      df <- load_reeds_file(file, run_name, header=header)
      # if data was found, add it to the list
      if(nrow(df) > 0){
        df_all <- append(df_all, list(df))
      }
    }
    # check for output information from e_report_params
    if(folder == "outputs" & file.exists(file.path(run_folder, "e_report_params.csv"))){
      e_report_params <- read.csv(file.path(run_folder, "e_report_params.csv"), comment.char = "#")
      e_report_params$filename <- gsub("\\(.*?\\)", "", e_report_params$param)
      e_report_params$filename <- ifelse(e_report_params$output_rename=="",
                                         e_report_params$filename,
                                         e_report_params$output_rename)
      file_units <- e_report_params[e_report_params$filename==gsub(".csv", "", filename), "units"]
      newcolname <- paste(gsub(".csv", "", filename), file_units, sep="_")
      newcolname_all <- c(newcolname_all, newcolname)
    }
  }

  # if list is empty, stop here
  if (length(df_all)==0){
    stop("No files loaded; check filename is specified correctly and that runs have completed successfully.")
  }
  # combine and merge scenario information
  df_out <- data.table::rbindlist(df_all, fill=T)

  # specific formatting for output columns
  if(folder == "outputs"){
    # get output column
    valcolname <- colnames(df_out)[grepl("Val", colnames(df_out))]
    # check that all runs have e_report_params.csv file
    if(length(newcolname_all) != nrow(runs)){
      cat("Caution: not all runs have 'e_report_params.csv'. Will skip renaming 'Val' column.")
    } else if (length(unique(newcolname_all)) > 1) {
      cat(sprintf("Caution: multiple column names detected from 'e_report_params.csv' across runs: %s.
                  Will skip renaming 'Val' column.", paste(unique(newcolname_all), collapse=", ")))
    } else {
      # rename if new name is supplied
      cat(sprintf("Updated column name: %s --> %s", valcolname, unique(newcolname_all)), sep="\n")
      colnames(df_out)[colnames(df_out) == valcolname] <- unique(newcolname_all)
      valcolname <- unique(newcolname_all)
    }
  }
  # report elapsed time
  elapsed <- proc.time() - tic
  cat(paste0("All files loaded (", as.numeric(round(elapsed["elapsed"]), 2), " secs.)"), sep="\n")
  return(df_out)
}

#' Load data from outputs.h5 from ReEDS runs
#'
#' Collect data from outputs.h5 files for a set of ReEDS runs.
#'
#' @param runs datatable of runs to load outputs, generated from run_summary()
#' @param resultname data key to load from h5 file
#' @param folder optional parameter to specify folder where the h5 file resides (default: outputs)
#'
#' @return data table with results by run name
#' @import data.table
#' @import rhdf5
#' @export
load_h5_data <- function(runs, resultname, folder="outputs"){
  check_cols(runs, c("run", "path"))
  tic <- proc.time()
  # helpers for keeping track run output
  df_all <- list()
  newcolname_all <- c()

  # exclude runs missing outputs.h5 file
  missing_runs <- runs[outputs_h5==F]
  cat(nrow(missing_runs), "runs are missing outputs.h5 file and will be skipped.\n", sep=" ")
  runs <- runs[outputs_h5==T]

  for(run_id in 1:nrow(runs)){
    # collect run and path information
    run_name <- runs[run_id]$run
    run_folder <- runs[run_id]$path
    h5path <- file.path(run_folder, "outputs", "outputs.h5")
    if(file.exists(h5path)){
      h5 <- rhdf5::H5Fopen(h5path)
      h5result <- rhdf5::h5read(h5, name = resultname)
      df <- data.table::data.table()
      for(col in h5result$columns){
        df$new <- h5result[[col]]
        setnames(df, "new", col)
      }

      if(nrow(df) > 0){
        # add run information
        df$run <- run_name
        df$filename <- "outputs.h5"
        df_all <- append(df_all, list(df))
        cat(paste("Loaded", resultname, "from outputs.h5 for", run_name), sep="\n")
      }
      # close h5 file
      rhdf5::h5closeAll()
    } else{
      cat(paste("outputs.h5 file missing for", run_name))
    }

    # check for output information from e_report_params
    if(folder == "outputs" & file.exists(file.path(run_folder, "e_report_params.csv"))){
      e_report_params <- read.csv(file.path(run_folder, "e_report_params.csv"), comment.char = "#")
      e_report_params$filename <- gsub("\\(.*?\\)", "", e_report_params$param)
      e_report_params$filename <- ifelse(e_report_params$output_rename=="",
                                         e_report_params$filename,
                                         e_report_params$output_rename)
      file_units <- e_report_params[e_report_params$filename==gsub(".csv", "", resultname), "units"]
      newcolname <- paste(gsub(".csv", "", resultname), file_units, sep="_")
      newcolname_all <- c(newcolname_all, newcolname)
    }
  }

  # if list is empty, stop here
  if (length(df_all)==0){
    stop("No files loaded; check filename is specified correctly and that runs have completed successfully.")
  }
  # combine and merge scenario information
  df_out <- data.table::rbindlist(df_all, fill=T)

  # specific formatting for output columns
  if(folder == "outputs"){
    # get output column
    valcolname <- colnames(df_out)[grepl("Val", colnames(df_out))]
    # check that all runs have e_report_params.csv file
    if(length(newcolname_all) != nrow(runs)){
      cat("Caution: not all runs have 'e_report_params.csv'. Will skip renaming 'Val' column.")
    } else if (length(unique(newcolname_all)) > 1) {
      cat(sprintf("Caution: multiple column names detected from 'e_report_params.csv' across runs: %s.
                    Will skip renaming 'Val' column.", paste(unique(newcolname_all), collapse=", ")))
    } else {
      # rename if new name is supplied
      cat(sprintf("Updated column name: %s --> %s", valcolname, unique(newcolname_all)), sep="\n")
      colnames(df_out)[colnames(df_out) == valcolname] <- unique(newcolname_all)
      valcolname <- unique(newcolname_all)
    }
  }
  # report elapsed time
  elapsed <- proc.time() - tic
  cat(paste0("All data loaded (", as.numeric(round(elapsed["elapsed"]), 2), " secs.)"), sep="\n")
  return(df_out)
}

#' Load system cost
#'
#' See 'pre_systemcost' function in ReEDS bokehpivot.
#'
#' @param runs datatable of runs to load system costs, generated from run_summary()
#'
#' @return data table with system costs
#' @import data.table
#' @export
load_system_cost <- function(runs) {

  ## load relevant datasets
  sys_cost_raw <- load_run_data(runs, "systemcost_ba.csv")
  crf <- load_run_data(runs, "crf.csv", folder="inputs_case")
  df_capex_init <- load_run_data(runs, "df_capex_init.csv", folder="inputs_case")
  sw <- load_run_data(runs, "switches.csv", folder="inputs_case")
  scalars <- load_run_data(runs, "scalars.csv", folder="inputs_case")

  # get evaluation period
  sys_eval_years = sw[V1=='sys_eval_years']
  trans_crp = scalars[V1=='trans_crp']

  addyears = max(sys_eval_years, trans_crp)

  #TODO: add checks for reading these files
  sys_cost_inflated <- inflate_cost_data(runs, sys_cost_raw)

  sys_cost_raw$cost


  ## inflate


  ## annualize using appropriate crp



  ## discount


}




#' List bokeh reports
#'
#' Function to list available boke reports from a set of runs
#'
#' @param runs data table of runs to search for bokeh report
#' @param runval optional parameter to specify name of specific run in runs to look at
#'
#' @import data.table
#'
#' @examples
#' list_bokeh_reports(runs)
#' list_bokeh_reports(runs, runval="runname")
#'
#' @export
list_bokeh_reports <- function(runs, runval=NULL){
  # TODO: this is very slow, not sure why
  if (is.null(runval)){
    path <- runs[1]$path
  } else {
    path <- runs[run == runval]$path
  }
  print(sprintf("Checking for reports in %s", path))
  bokeh_dirs <- basename(list.dirs(file.path(path, "outputs"), recursive=F))
  print("Possible bokeh reports:")
  for (b in bokeh_dirs){print(b)}
}


#' List bokeh report fields
#'
#' Function to list fields in the report.xlsx file associated with a bokeh report
#'
#' @param runs data table of runs to search for bokeh report
#' @param runval optional parameter to specify name of specific run in runs to look at
#' @param report optional parameter to specify bokeh report to look for (default: reeds-report)
#'
#' @import data.table
#' @import readxl
#'
#' @examples
#' list_bokeh_fields(runs)
#' list_bokeh_fields(runs, runval="runname")
#' list_bokeh_fields(runs, report="reeds-report-expanded")
#'
#' @export
list_bokeh_fields <- function(runs, runval=NULL, report="reeds-report"){
  # look for report
  if (is.null(runval)){
    path <- runs[1]$path
  } else {
    path <- runs[run == runval]$path
  }
  # check if directory exists
  print(sprintf("Checking for fields in %s", path))
  bokeh <- file.path(path, "outputs", report, "report.xlsx")
  if(file.exists(bokeh)){
    print(readxl::excel_sheets(bokeh))
  } else {
    print(sprintf("%s/report.xlsx does not exist."))
    list_bokeh_reports(runs)
  }
}


#' Load outputs from bokeh reports
#'
#' Function to collect bokeh report data from a set of ReEDS runs. Defaults to data from "reeds-report"
#' but be used to extract values from any bokeh report.
#'
#' @param runs data table of runs to load outputs
#' @param filename filename to load
#' @param folder optional parameter to specify folder where filename resides (default: outputs)
#' @param newcolname optional parameter to rename value column
#'
#' @return data table with results by run name
#' @import data.table
#' @import readxl
#'
#' @examples
#' avgcost <- load_bokeh_data(runs, "18_National Average Electricity")
#'
#' @export
load_bokeh_data <- function(runs, result_name, report_name="reeds-report") {
  # TODO: revise and test this function
  # TODO: slow when run data is on nrelnas, is there a way to speed up?
  # TODO: add regular expression matching for fields?
  tic <- proc.time()
  df_all <- list()
  for(run_id in 1:nrow(runs)){
    run_name <- runs[run_id]$run
    run_folder <- runs[run_id]$path
    # check for bokeh report
    bokeh_report <- file.path(run_folder, "outputs", report_name, "report.xlsx")
    if (file.exists(bokeh_report)){
      print(sprintf("Loading %s from %s/report.xlsx for %s", result_name, report_name, run_name))
    } else{
      print(sprintf("%s/report.xlsx does not exist for %s, skipping. ", report_name, run_name))
    }
    tryCatch(
      {
        df <- readxl::read_excel(file.path(run_folder, "outputs", report_name, "report.xlsx"), sheet=result_name)
        # if scneario column exists, overwrite it
        colnames(df)[colnames(df) == "scenario"] <- "run"
        df$run <- run_name
        df_all <- append(df_all, list(df))
      },
      error=function(cond)
      {
        print(sprintf("Error loading %s from %s/report.xlsx for %s, skipping", result_name, report_name, run_name))
      }
    )
  }
  # combine and data from runs
  df_out <- data.table::rbindlist(df_all, fill=T)
  elapsed <- proc.time() - tic
  print(paste("Elapsed time:", as.numeric(round(elapsed["elapsed"]), 2), "seconds") )
  return(df_out)
}

#' @import sf
#' @export
load_ba_shapefile <- function(reedspath, shapefile="US_PCA") {
  check_dir_exists(reedspath)
  shp <- sf::st_read(file.path(reedspath, "inputs", "shapefiles", shapefile, paste0(shapefile, ".shp")))
  colnames(shp)[colnames(shp) == "rb"] <- "r"
  return(shp)
}

#' @import sf
#' @export
load_tx_shapefile <- function(reedspath, shapefile="transmission_routes") {
  check_dir_exists(reedspath)
  shp <- sf::st_read(file.path(reedspath, "inputs", "shapefiles", shapefile, paste0(shapefile, ".shp")))
  return(shp)
}



## load supply curve data ####
#' @export
load_sc_files <- function(tech, update, folder=NULL, filebase="_supply-curve.csv"){

  if(.Platform$OS.type == "unix") {
    sc_path <- "/Volumes/ReEDS/Supply_Curve_Data"
  } else {
  # TODO: test windows path
    sc_path <- "/nrelnas01/ReEDS/Supply_Curve_Data"
  }

  rev_path <- file.path(sc_path, tech, update, "reV")
  check_dir_exists(rev_path)

  ## first find relevant sc files
  # if folder is supplied, look in there for post-processied supply curve files
  if(!is.null(folder)){
    allfiles <- list.files(file.path(rev_path, folder))
    scfiles <-  file.path(rev_path, folder, allfiles[grepl(filebase, allfiles)] )
    # otherwise, find sc files in scenario folders directly
  } else{
    alldirs <- list.dirs(file.path(rev_path), recursive = F)
    scendirs <- alldirs[grepl("^[0-9]", basename(alldirs))]
    scfiles <- c()
    for (scendir in scendirs){
      allfiles <- list.files(scendir)
      scfiles <- c(scfiles, file.path(scendir, allfiles[grepl(filebase, allfiles)] ))
    }
  }

  # check for supply curve files
  if (length(scfiles) == 0 ){
    stop(paste("No supply curve files found; check path to post-processing folder"))
  } else{
    print("Located the following sc files:")
    for(scfile in scfiles){
      print(scfile)
    }
  }

  ## now loop over sc files and load
  sc_data_all <- list()

  for (scfile in scfiles) {
    scenario <- gsub(filebase, "", basename(scfile))
    print(paste0("Processing ", scenario))

    # for testing
    #sc_data <- fread(scfile, nrow=100)
    sc_data <- fread(scfile)

    # add update version and scenario info
    sc_data$update <- update
    sc_data$scenario <- scenario
    sc_data_all <- append(sc_data_all, list(sc_data))
    rm(sc_data)
  }

  # select for these  columns to simplify data
  # col_select <- c("update", "scenario", "sc_point_gid", "cnty_fips", "state",
  #                 "capacity_mw_dc", "capacity_mw_ac", "latitude", "longitude",
  #                 "mean_capital_cost", "mean_system_capacity",
  #                 "trans_cap_cost_per_mw_ac",
  #                 "reinforcement_cost_per_mw_ac", "reinforcement_dist_km",
  #                 "lcot", "total_lcoe",
  #                 "eos_mult", "reg_mult")

  # for (l in sc_data_all){
  #   if (sum(!col_select %in% colnames(l)) > 0) {
  #     print(sprintf("%s is missing the following columns: %s", unique(l$scenario), paste(col_select[! col_select %in% colnames(l)], collapse=", ")))
  #   }
  # }

  # merge list results into 1 data.table
  #sc_data_all <- lapply(sc_data_all, function(x) subset(x, select = col_select))
  sc_data_out <- rbindlist(sc_data_all, use.names=T, fill=T)

  return(sc_data_out)
}
