## util.R ####

#' Check if a path is valid
#' @param path folder or filepath
check_dir_exists <- function(path){
  if(!dir.exists(path)){
    stop(paste("Could not find the following path:", path))
  }
}

#' Check if required columns are present
#' @param df dataframe or datatable object
#' @param cols vector of column names to check
check_cols <- function(df, cols){
  matched_cols <- intersect(colnames(df), cols)
  missing_cols <- cols[!(cols %in% matched_cols)]
  if(length(missing_cols)>0){
    stop(paste("The following columns are required: ", paste(missing_cols, sep=",")))
  }
}

#' Load a ReEDS run file
#' @param path path to file to load
#' @param run_name name of run
load_reeds_file <- function(path, run_name, header){
  filename <- basename(path)
  tryCatch(
    {
      df <- data.table::fread(path, header=header)
      cat(paste("Loaded", basename(path), "for", run_name), sep="\n")
      df$run <- run_name
      df$filename <- filename
      return(df)
    },
    error=function(cond)
    {
      cat(paste("Error reading", basename(path), "for", run_name), sep="\n")
      df <- data.table::data.table(run=factor(), data=numeric())
      return(df)
    }
  )
}
