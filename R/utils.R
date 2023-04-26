


check_dir_exists <- function(path){
  if(!dir.exists(path)){
    stop(paste("Could not find the following path:", path))
  }
}

#' Internal function to get runtime from meta.csv file
#'
#' @param runs Data Table with run information
#'
#' @return Data Table of ReEDS scenarios with the total runtime and last time step
#' @import data.table
load_meta <- function(runs){
  meta_out <- list()
  for(run_id in 1:nrow(runs)){
    run_name <- runs[run_id]$run
    path_name <- runs[run_id]$path
    print(paste("Loading meta file for", run_name))
    tryCatch(
      {
        df <- data.table::fread(file.path(path_name, "meta.csv"), skip=3)
        df$run <- run_name
        df$path <- path_name
        meta_out <- append(meta_out, list(df))
      },
      error=function(cond)
      {
        print(paste("Error reading meta file for", run_name))
      }
    )
  }
  meta_out <- data.table::rbindlist(meta_out, fill=T)
  #drop rows with "end"
  meta_out <- meta_out[process!="end"]
  # aggregate by run
  seconds_per_hour <- 3600
  meta_out <- meta_out[, by=.(run,path), .(runtime_hours=round(sum(processtime)/seconds_per_hour,1), last_step=paste(process[length(process)], year[length(year)], sep="_"))]
  return(meta_out)
}

#' Internal function to check for output files
#'
#' @param runs Data Table with run information
#'
#' @return Data Table of ReEDS scenarios with boolean 'outputs' column indicating the presence of output csv files
#' @import data.table
check_outputs <- function(runs, prefix, file_count=10){
  # determine which runs have outputs folders
  runs$outputs <- F
  for (f in runs$path) {
    if (length(list.files(file.path(f, "outputs"))) > file_count){
      runs[path==f, "outputs"] <- T
    }
  }
  return(runs)
}

#' Internal function intended to throw an error if a needed
#' column isn't found
#' TODO: add info about data, function
check_columns <- function(df, cols){
 missing_cols <- cols[! cols %in% colnames(df)]
 if (length(missing_cols) > 0){
   stop(paste0("The following columns are required but not found: ", paste0(cols, collapse = ", ")))
 }
}


