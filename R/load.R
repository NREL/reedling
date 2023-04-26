
#TODO: add checking for nrelnas connection

#' @export
list_runs <- function(path){
  check_dir_exists(path)
  print(basename(list.dirs(path, recursive=F)))
}

#' Get ReEDS runs
#'
#' Function to discover a set of ReEDS run in a given run folder, with the option to subset to runs that match
#' a specified batch prefix. The prefix can be specified as a regex string if runs from multiple batches
#' are of interest. The function collects information on the full path to the run, the runtime, the last ReEDS step
#' that successfully completed, and whether there are files in the 'outputs' folder of each run.
#'
#' @param path Path to a set of ReEDS runs.
#' @param prefix Optional prefix to filter runs; can be specified as a regex pattern.
#'
#' @return Data Table of ReEDS scenarios and their paths, or NULL if no scenarios are found.
#' @import data.table
#' @import stringr
#' @export
#'
#' @examples
#' runs <- list_runs("path/to/ReEDS-2.0/runs", "20230416")
#'
#' # runs from multiple batch names can be matched by joining prefixes with the '|' operator
#' runs <- list_runs("path/to/ReEDS-2.0/runs", "20230416|20230422")
load_run_summary <- function(path, prefix=""){
  # check if directory exists
  check_dir_exists(path)
  # get folders at specified path
  runs <- list.dirs(path, recursive = F)
  # subset to runs with a prefix if provided
  if(prefix != ""){
    # batch prefix can be a single string or a regex string (e.g., v2022_11_22|v2022_12_06)
    runs <- runs[grepl(prefix, runs)]
  }
  # check for valid runs
  if (length(runs) == 0){
    stop("No runs found; specify a different path or batch prefix.")
  }
  runs_out <- data.table::data.table(run=basename(runs), path=runs)
  # get timing on runs
  runs_out <- load_meta(runs_out)
  # check for output files
  runs_out <- check_outputs(runs_out)
  # separate prefix from run name
  # TODO: add switch to keeping version in run title?
  if(prefix != ""){
    runs_out$run <- str_replace(runs_out$path, paste0(".*(", prefix, ")_"), "")
    runs_out$version <- str_extract(runs_out$path, prefix)
  } else {
    runs_out$run <- basename(runs_out$path)
    runs_out$version <- NA
  }
  # subset outputs
  runs_out <- runs_out[, c("run", "version", "outputs", "last_step", "runtime_hours", "path"), with=F]
  return(runs_out)
}


#' Loads outputs from ReEDS runs; for outputs with generator techs also performs some formatting
#'
#' @param runs Data Table of runs to load outputs.
#' @param output_file Filename of output to load.
#'
#' @return Data Table with results by run name
#' @import data.table
#' @export
#'
#' @examples
#' scens <- list_runs("path/to/ReEDS-2.0/runs", "20230416")
#' scens <- list_runs("path/to/ReEDS-2.0/runs", "20230416|20230422")
load_run_data <- function(runs, filename, folder="outputs", newcolname=NULL) {
  tic <- proc.time()
  df_all <- list()

  # TODO: add check for run file
  # TODO: nicer printing
  for(run_id in 1:nrow(runs)){
    run_name <- runs[run_id]$run
    run_folder <- runs[run_id]$path

    # if loading ouputs but run has no outputs files, skip to next run
    if (runs[run_id]$outputs==F & folder=="outputs"){
      print(paste0("No outputs for ", run_name, ", skipping."))
      #print(paste("Skipping runs without output files:", paste(runs[outputs==F]$run , collapse=", ")))
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

#' TODO: complete description here
#'
#' @param runs Data Table of runs to list outputs.
#'
#' @return Data Table with results by run name
#' @import data.table
#' @import readxl
#' @export
list_outputs <- function(runs, runval=NULL, folder="outputs", bokeh=F){
    # unless specified, get files from the first run
    if (is.null(runval)){
      path <- runs[1]$path
    } else {
      path <- runs[run == runval]$path
    }
    # check if directory exists
    check_dir_exists(path)
    # look for results
    if (bokeh){
      bokeh <- file.path(path, "outputs", folder, "report.xlsx")
      if(file.exists(bokeh)){
        print(readxl::excel_sheets(bokeh))
      } else {
        print("Bokeh report folder does not exist.")
        # TODO: improve reading bokeh reports
        bokeh_dirs <- basename(list.dirs(file.path(path, "outputs")))
        print(paste("Possible bokeh reports:", paste(bokeh_dirs, collapse=",")))
      }
    } else {
      print(list.files(file.path(path, folder)))
    }
  }


# TODO: revise and test this function
load_bokeh_result <- function(runs, filename, folder="outputs", bokeh=F) {

  tic <- proc.time()
  df_all <- list()

  if (nrow(scens) == 0){
    print("No scenarios in list")
    return(NULL)
  }

  print("Skipping runs without output files")
  runs <- runs[outputs==T]

  for(run_id in 1:nrow(runs)){
    run_name <- runs[run_id]$run
    run_folder <- runs[run_id]$path

    if (bokeh){
      print(paste("Loading", filename, "from report.xlsx in", folder, "for", run_name))
    } else{
      print(paste("Loading", filename, "for", run_name))
    }
    tryCatch(
      {
        if (bokeh){
          df <- read_xlsx(file.path(run_folder, "outputs", folder, "report.xlsx"), sheet=filename)
        }  else{
          df <- fread(file.path(run_folder, "outputs", filename))
        }
        df$scenario <- run_name
        df_all <- append(df_all, list(df))
      },
      error=function(cond)
      {
        if (bokeh){
          print(paste("Missing", filename, "from", folder, "for", run_name))
        } else {
          print(paste("Missing", filename, "for", run_name))
        }
      }
    )
  }
  # combine and merge scenario information
  df_out <- rbindlist(df_all, fill=T)
  df_out <- merge(df_out, scens, by="scenario", all=T)
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
