## load.R ####

#' List ReEDS runs
#'
#' Function to discover a set of ReEDS run in a given run folder.
#'
#' @param path Path to a set of ReEDS runs
#'
#' @examples
#' runs <- list_runs("path/to/ReEDS-2.0/runs")
#'
#' @export
list_runs <- function(path){
  check_dir_exists(path)
  print(sprintf("Found the following folder in %s:", path))
  folders <- basename(list.dirs(path, recursive=F))
  for (f in folders){print(f)}
}

#' Summarize ReEDS runs
#'
#' Function to summarize metadata on set of ReEDS run in a given run folder, with the option to subset to runs that match
#' a specified batch prefix. The prefix can be specified as a regex string if runs from multiple batches
#' are of interest.
#'
#' The function returns a data table with the following information on the runs:
#' - run name (dropping any batch prefix if supplied)
#' - batch name (if supplied)
#' - whether files were detected in the 'outputs' folder
#' - the last ReEDS step that completed, as listed in meta.csv
#' - the run time, as listed in meta.csv
#' - full path to the run
#'
#' @param path Path to a set of ReEDS runs
#' @param batch optional prefix to filter runs; can be specified as a regex pattern
#'
#' @return data table of ReEDS scenarios and their paths
#' @import data.table
#' @import stringr
#'
#' @examples
#' runs <- load_run_summary("path/to/ReEDS-2.0/runs", "20230416")
#'
#' # runs from multiple batches can be matched by using regex expressions
#' runs <- load_run_summary("path/to/ReEDS-2.0/runs", "20230416|20230422")
#'
#' @export
load_run_summary <- function(path, batch=""){
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
  runs_out <- data.table::data.table(run=basename(runs), path=runs)
  # get timing on runs
  runs_out <- load_meta(runs_out)
  # check for output files
  runs_out <- check_outputs(runs_out)
  # separate batch from run name
  if(batch != ""){
    runs_out$run <- str_replace(runs_out$path, paste0(".*(", batch, ")_"), "")
    runs_out$batch <- str_extract(runs_out$path, batch)
    cols_out <- c("run", "batch", "outputs", "last_step", "runtime_hours", "path")
  } else {
    runs_out$run <- basename(runs_out$path)
    cols_out <- c("run", "outputs", "last_step", "runtime_hours", "path")
  }
  # subset outputs
  runs_out <- runs_out[, cols_out, with=F]
  return(runs_out)
}

#' List output files
#'
#' Prints a list of files in a folder of runs. Defaults to looking at the outputs folder of the first run specified.
#'
#' @param runs data table of runs to list outputs;
#' @param runval optional parameter to specify name of specific run in runs to look at
#' @param folder optional parameter to specify folder to look for files (default: outputs)
#'
#' @import data.table
#' @examples
#' list_outputs(runs)
#' list_outputs(runs, runval="runname")
#' list_outputs(runs, folder="inputs_case")
#'
#' @export
list_outputs <- function(runs, runval=NULL, folder="outputs"){
  # unless specified, get files from the first run
  if (is.null(runval)){
    path <- runs[1]$path
  } else {
    path <- runs[run == runval]$path
  }
  print(sprintf("Checking for files in %s", path))
  # check if directory exists
  check_dir_exists(path)
  print(list.files(file.path(path, folder)))
}


#' Load outputs from ReEDS runs
#'
#' Function to collect csv data from a set of ReEDS runs. Defaults to data in the "outputs" folder of the ReEDS runs,
#' but can also be used for data stored in inputs_case.
#'
#' @param runs data table of runs to load outputs
#' @param filename filename to load
#' @param folder optional parameter to specify folder where filename resides (default: outputs)
#' @param newcolname optional parameter to rename value column
#'
#' @return data table with results by run name
#' @import data.table
#' @export
#'
#' @examples
#' cap <- load_run_data(runs, "cap.csv")
#'
#' # newcolname can be used to rename a column with "Val" in it
#' cap <- load_run_data(runs, "cap.csv", newcolname="cap_MW")
#'
#' # data from other folders can also be extracted
#' hierarchy <- load_run_data(runs, "hierarchy.csv", folder="inputs_case")
load_run_data <- function(runs, filename, folder="outputs", newcolname=NULL) {
  tic <- proc.time()
  df_all <- list()
  for(run_id in 1:nrow(runs)){
    run_name <- runs[run_id]$run
    run_folder <- runs[run_id]$path

    # if loading outputs but run has no outputs files, skip to next run
    if (runs[run_id]$outputs==F & folder=="outputs"){
      print(paste0("No outputs for ", run_name, ", skipping."))
    } else{
      print(paste0("Loading ", folder, "/", filename, " for ", run_name))
      tryCatch(
        {
          df <- data.table::fread(file.path(run_folder, folder, filename))
          df$run <- run_name
          df_all <- append(df_all, list(df))
        },
        error=function(cond)
        {
          print(paste("Error reading ", folder, "/", filename, "for", run_name))
        }
      )
    }
  }
  # combine and merge scenario information
  df_out <- data.table::rbindlist(df_all, fill=T)

  # specific formatting for output columns
  if(folder == "outputs"){
    # get output column
    valcolname <- colnames(df_out)[grepl("Val", colnames(df_out))]
    if(!is.null(newcolname)){
      # rename if new name is supplied
      colnames(df_out)[colnames(df_out) == valcolname] <- newcolname
      valcolname <- newcolname
    }
    # pivot by run info to fill in missing values with zeros
    run_list <- unique(df_out$run)
    df_out <- data.table::data.table(tidyr::pivot_wider(df_out, names_from=run, values_from=valcolname, values_fill=0))
    df_out <- data.table::melt(df_out, measure.vars=run_list, variable.name="run", value.name=valcolname)
  }

  elapsed <- proc.time() - tic
  print(paste("Elapsed time:", as.numeric(round(elapsed["elapsed"]), 2), "seconds") )
  return(df_out)
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

#load_timeseries <- function(runs){
 # hmap <- load_run_data(runs, "hmap_myr.csv", folder="inputs_case")
  #
#}
