## format.R ####

#' Set default ggplot settings for tech plots
#'
#' Set ggplot2 theme settings for bar charts
#'
#' @examples
#' set_tech_plot_default()
#'
#' @export
set_tech_plot_default <- function() {
  ggplot2::theme_set(ggplot2::theme_bw())
  ggplot2::theme_update(text=ggplot2::element_text(size=8),
                legend.key.size = ggplot2::unit(1, 'mm'),       #change legend key size
                legend.key.height = ggplot2::unit(1, 'mm'),    #change legend key height
                legend.key.width = ggplot2::unit(2, 'mm'),     #change legend key width
                legend.title = ggplot2::element_blank(),       #change legend title font size
                legend.text = ggplot2::element_text(size=6),   #change legend text font size
              )
}


#' Set tech map
#'
#' Set the mappingfile for generator techs, which are used to aggregate ReEDS generator techs into fewer categories.
#' Defaults to taking bokehpivot mapping in "tech_map.csv" when passed a path to a ReEDS repo,
#' but can also be used to load any custom mapping file.
#'
#' @param path path to ReEDS repository or a directory with a custom tech mapping file
#' @param filename typically "tech_map.csv" but can optionally be specified as any custom file (default = "tech_map.csv")
#' @param custom flag indicating whether to use a custom tech mapping or the ReEDS default in bokehpivot (default = FALSE)
#'
#' @return dataframe of tech categories
#'
#' @examples
#' tech_map <- set_tech_map("path/to/ReEDS-2.0")
#' tech_map <- set_tech_map("path/to/custommap", custom=T, file="custom_tech_map.csv")
#'
#' @export
set_tech_map <- function(path, filename="tech_map.csv", custom=F){
  if (custom){
    mappath <- path
    print(paste0("Reading custom tech mapping file: ", file.path(path, filename)))
  } else {
    mappath <- file.path(path, "postprocessing", "bokehpivot", "in", "reeds2")
    print(paste0("Reading default tech mapping from bokehpivot: ", file.path(mappath, filename)))
  }
  check_dir_exists(mappath)
  tryCatch(
    {
      tech_map <- read.csv(file.path(mappath, filename))
    },
  error=function(cond)
    {
      print("Could not read tech mapping file; check path")
    }
  )
  return(tech_map)
}

#' Set tech style
#'
#' Set the color style scheme file for generator techs. Should correspond to the techs specified in the tech mapping file (see set_tech_map)
#' Defaults to taking bokehpivot mapping in "tech_style.csv" when passed a path to ReEDS,
#' but can also be used to load any custom style file
#'
#' @param path path to ReEDS repository or a directory with a custom tech style
#' @param filename typically "tech_style.csv" but can optionally be specified as any custom file (default = "tech_style.csv")
#' @param custom flag indicating whether to use a custom tech style or the ReEDS default in bokehpivot (default = FALSE)

#'
#' @return named list of techs and colors
#'
#' @examples
#' tech_style <- set_tech_style("path/to/ReEDS-2.0")
#' tech_map <- set_tech_map("path/to/customstyle", custom=T, file="custom_tech_style.csv")
#'
#' @export
set_tech_style <- function(path, filename="tech_style.csv", custom=F){
  if (custom){
    colpath <- path
    print(paste0("Reading custom tech colors: ", file.path(colpath, filename)))
  } else {
    colpath <- file.path(path, "postprocessing", "bokehpivot", "in", "reeds2")
    print(paste0("Reading default tech style from bokehpivot: ", file.path(colpath, filename)))

  }
  check_dir_exists(colpath)

  tryCatch(
    {
      tech_style_in <- read.csv(file.path(colpath, filename), stringsAsFactors = F)
    },
    error=function(cond)
    {
      print("Could not read tech style file; check path")
    }
  )

  # format as named list for plotting
  tech_style <- tech_style_in$color
  names(tech_style) <- tech_style_in$order

  return(tech_style)
}

#' Format techs
#'
#' Recategorizes data by generator techs based on a tech mapping file. Outputs the same data with a new column named `tech` that includes the new mapping.
#'
#' @param df data table with generator technologies to map in a column specified by `tech_col`
#' @param mapping data table with mapping of old to new techs (see `set_tech_map`)
#' @param tech_colors optional named vector of techs with colors in order for plotting; used to set factor levels for new tech (see `set_tech_style`)
#' @param tech_col column from `df` with tech names (default = `i`)
#' @param col_from column from `mapping` with old tech names (default = `raw`)
#' @param col_to column from `mapping` with new tech names (default = `display`)`)

#'
#' @return Data Table of tech categories
#' @import data.table
#' @import stringr
#' @import plyr
#'
#' @export
format_techs <- function(df, mapping, tech_colors=NULL, tech_col="i", col_from="raw", col_to="display"){
  # check for the correct columns
  check_cols(df, c(tech_col))
  if (!col_to %in% colnames(mapping) | !col_from %in% colnames(mapping)){
    print(paste0("Cannot find column in tech mapping file; specify one of the following columns: ", paste0(colnames(mapping), collapse =", ")))
    return(NULL)
  }
  # if using a different column than i for techs, set to i
  if (tech_col != "i"){
    if("i" %in% colnames(df)){
      colnames(df)[colnames(df) == "i"] <- "i_old"
    }
    df$i <- df[[tech_col]]
  }
  # for columns with *, replace with .* for matching regular expressions
  # mapping[, (col_from) := gsub("\\*", ".*", get(col_from))]
  mapping[, col_from] <- gsub("\\*", ".*", mapping[, col_from])
  # Sort the mapping by length of the matching pattern in descending order;
  # this helps make sure that the closest match is applied
  mapping <- mapping[order(-stringr::str_length(mapping[, col_from])),]
  # special adjustment for demand response (dr): make sure if only matches
  # tech when it is at the beginning of the string
  mapping[, col_from] <- plyr::mapvalues(mapping[, col_from], from="dr.*", to="^dr.*")
  # map values
  # TODO: finish updating here
  #df$tech <- stringr::str_replace_all(tolower(df$i), stats::setNames(as.character(mapping[, col_to]),
  #                                                                   as.character(mapping[, col_from])))
  df$tech <- plyr::mapvalues(tolower(df$i), from=mapping[, col_from], to=mapping[, col_to])

  # check for unmapped technologies
  missing_techs <- unique(df$i[is.na(df$tech)])
  if (length(missing_techs) > 1){
    print(paste("The following technologies were unmapped:", paste(missing_techs, collapse = ", ")))
  }
  df_mapped <- df[,by=.(i,tech), .(obs=length(i))]
  cat("Mapped the following technologies:\n\n")
  df_mapped <- df_mapped[order(df_mapped$i)]
  print(sprintf("%-25s --> %s", df_mapped$i, df_mapped$tech))
  # apply levels for tech_colors if specified
  if (!is.null(tech_colors)){
    cat("Formatting tech levels\n\n")
    missing_levels <- unique(df$tech)[!unique(df$tech) %in% names(tech_colors)]
    duplicated_levels <- tech_colors[duplicated(names(tech_colors))]
    if ( length(missing_levels) > 0 ){
      print("Missing the following techs from colors:")
      print(missing_levels)
      print("Techs mapped but levels not set")
    } else if (length(duplicated_levels) > 0) {
      print("The following techs are duplicated in color mapping:")
      print(duplicated_levels)
      print("Techs mapped but levels not set")
    } else {
      df$tech <- factor(df$tech, levels=rev(names(tech_colors)))
    }
  }
  return(df)
}

#' Inflate cost data
#'
#' @param runs datatable of runs to load system costs, generated from run_summary()
#' @param df data with cost information
#' @param cols column names to inflate
#' @param year_from integer of original dollar year of data (default: 2004)
#' @param year_to integer of dollar year to conver to
#' @param drop_original if TRUE drop original dollar year data
#' @export
inflate_cost_data <- function(runs, cost_df, year_from=2004, year_to=NULL,
                              cols=NULL, drop_original=T){

  # load deflator
  deflators <- load_run_data(runs, "deflator.csv", folder="inputs_case")
  data.table::setnames(deflators, c("*Dollar.Year", "Deflator"), c("dollar_year", "deflator"))

  # get from year
  deflators_from <- deflators[dollar_year == year_from]

  # use target present value year; if unspecified convert to latest year
  if(is.null(year_to)){
    year_to <- max(deflators$dollar_year)
  }

  deflators_to <- deflators[dollar_year == year_to]

  # rename and merge
  data.table::setnames(deflators_from, "deflator", "numerator")
  data.table::setnames(deflators_to, "deflator", "denominator")
  deflate_values <- merge(deflators_from, deflators_to, by=c("run", "filename"))
  deflate_values$inflator <- deflate_values$numerator / deflate_values$denominator
  cost_df <- merge(cost_df, deflate_values[,c("run", "inflator")], all.x=T)

  # check for cost columns
  if(is.null(cols)){
    cols <- colnames(cost_df)[grepl("\\$", colnames(cost_df))]
    if (length(cols)==0){
      stop("No cost columns identified; please specify columns to convert via 'cols' argument.")
    }
  }

  # apply inflation to relevant columns
  for(col in cols){
    cat(sprintf("Converting %s from %d to %d", col, year_from, year_to))
    cost_df$new <- cost_df[, get(col)] * cost_df$inflator

    # rename column, checking if original column had
    if (grepl(paste0(year_from, "\\$"), col)){
      newcol <- stringr::str_replace(col, as.character(year_from), as.character(year_to))
    } else {
      newcol <- paste(col, year_to, sep="_")
    }
    data.table::setnames(cost_df, "new", newcol)

    if(drop_original){
      setnames(cost_df, col, "drop")
      cost_df$drop <- NULL
    }
  }

  # remove inflator column and return
  cost_df$inflator <- NULL

  return(cost_df)
}

#' @export
map_rs_to_r <- function(df, path) {
  rsmap <- read.csv(file.path(path, "rsmap.csv"),
                        col.names = c("r", "rs"), stringsAsFactors = F)
  df$r <- plyr::mapvalues(df$r, from=rsmap$rs, to=rsmap$r, warn_missing = F)
  return(df)
}

#' @export
categorize_scenarios <- function(runs, scenmapping){
  scenmapping <- data.frame(lapply(scenmapping, as.character), stringsAsFactors=FALSE)
  # add new scenario columns
  newcols <- unique(scenmapping$col)
  scenarios <- copy(runs)
  scenarios[, (newcols) := list(NA_character_)]
  # iterate over mappings
  for(row in 1:nrow(scenmapping)){
    scenarios[, (scenmapping[row, "col"])] <- ifelse(is.na(scenarios[,get(scenmapping[row, "col"])]),
                                                  stringr::str_extract(scenarios$run, scenmapping[row, "find"]),
                                                  scenarios[,get(scenmapping[row, "col"])])
  }
  # # TODO: reformat to have one output (use runs?)
  # # TODO: improve printing format?
  # df_mapping_results <- list()
  # for (col in newcols){
  #   # check mapping is reasonable
  #   #print(paste(col, "scenario"))
  #   #print(table(df$run, df[,get(col)]))
  #   df_mapping_results[[col]] <- data.frame(table(df$run, df[,get(col)]))
  #   # order factors
  #   levelvals <- unique(scenmapping[scenmapping$col == col, "find"])
  #   # TODO: toggle factor levels since this can break if adding more
  #   #df[, (col) := factor(get(col), levels=levelvals)]
  # }
  # df_mapping_results_out <- do.call(cbind, df_mapping_results)
  # print(df_mapping_results_out)

  return(scenarios)
}

#' @export
calc_difference <- function(df, widecol, valcol, baseval, compval, excludecol=NULL){
  if(is.null(excludecol) | valcol == "run"){
    df_wide <- tidyr::pivot_wider(df, names_from=widecol, values_from=valcol, values_fill = 0)
  } else {
    df_wide <- tidyr::pivot_wider(df, names_from=widecol, values_from=valcol, values_fill = 0, id_cols=-excludecol)
  }
  df_wide <- data.table(df_wide)

  # check for columns to difference
  check_cols(df_wide, c(baseval, compval))

  # compute difference
  df_wide$diff <- df_wide[[compval]] - df_wide[[baseval]]
  df_wide$colname <- paste(compval, baseval, sep=" - ")
  colnames(df_wide)[colnames(df_wide) %in% c("diff", "colname")] <- c(valcol, widecol)
  for (col in excludecol){
    df_wide[, (col) := NA]
  }
  df_wide <- df_wide[, colnames(df), with=F]
  return(df_wide)

}

#' @export
format_transmission <- function(df, reedspath, routetype="transmission_endpoints"){
  # load shapefile with transmission endpoints
  tx_shp <- load_tx_shapefile(reedspath, routetype)
  # get X and Y coordinates for linestrings
  tx_endpoints <- data.frame(r=as.character(tx_shp$ba_str), X=st_coordinates(tx_shp)[,"X"], Y=st_coordinates(tx_shp)[,"Y"])

  check_cols(df, c("r_from", "r_to"))
  df <- merge(df, tx_endpoints, all.x=T, by.x="r_from", by.y="r")
  colnames(df)[colnames(df) %in% c("X", "Y")] <- c("X_from", "Y_from")
  df <- merge(df, tx_endpoints, all.x=T, by.x="r_to", by.y="r")
  colnames(df)[colnames(df) %in% c("X", "Y")] <- c("X_to", "Y_to")

  # put points into matrix for generating linestrings
  from_points = df[, c("X_from", "Y_from")]
  names(from_points) = c("long", "lat")
  to_points = df[, c("X_to", "Y_to")]
  names(to_points) = c("long", "lat")

  # compute linestrings
  df$geometry = do.call(
    "c",
    lapply(seq(nrow(from_points)), function(i) {
      st_sfc(
        st_linestring(
          as.matrix(
            rbind(from_points[i, ], to_points[i, ])
          )
        ),
        crs = st_crs(tx_shp)
      )
    }))

  df <- st_as_sf(df)
  return(df)
}


## add function to format timeries
# read hmap and merge
# add timestamp

#' @export
#' @import data.table
#' @import lubridate
format_timeseries <- function(runs, df, hmap_file="hmap_myr.csv", hmap=NULL){
  # first load hmapping for the runs
  if(is.null(hmap)){
    hmap <- load_run_data(runs, hmap_file, folder="inputs_case")
  }
  check_cols(hmap, c("*timestamp", "h","run"))
  check_cols(df, c("allh","run"))

  # datetimes from ReEDS are currently in Eastern time
  hmap$`*timestamp` <- lubridate::force_tz(hmap$`*timestamp`, tzone="Etc/GMT+5")
  hmap$`*timestamp` <- lubridate::force_tz(hmap$`*timestamp`, tzone="UTC")
  # merge data and timeseries, with timeseries on left to ensure each time is matched
  df_ts <- merge(hmap, df, by.x=c("run","h"), by.y=c("run","allh"), all=T, allow.cartesian = T)
  # TODO: check on merge

  # add month label (useful for plotting)
  df_ts$month_label <- lubridate::month(df_ts$`*timestamp`, label=T)

  # order
  df_ts <- df_ts[order(df_ts$run, df_ts$`*timestamp`)]
  return(df_ts)
}

## add funtion to load / format storage data




# # TO DO: these currently use tech_map/tech_style as globals, may want to revist that
# map_tech <- function(df, tech_map=tech_map, tech_style=tech_style, col_from="raw",col_to="display"){
#   # check for the correct columns
#   if (!col_from %in% colnames(tech_map)){
#     print(paste0("Cannot find '", col_from, "' in tech mapping file. Specify a different column: ", paste0(colnames(tech_map), collapse =", ")))
#     print(colnames(tech_map))
#     return(NULL)
#   }
#   df$tech_agg <- mapvalues(tolower(df$tech), from=tech_map[, get(col_from)], to=tech_map[, get(col_to)], warn_missing = F)
#   df$tech_agg <- factor(df$tech_agg, levels=rev(unique(names(tech_style))))
#
#
#
#
#   # check for unmapped technologies
#   missing_techs <- unique(df$tech[is.na(df$tech_agg)])
#   if (length(missing_techs) > 1){
#     print(paste("The following technologies were unmapped:", paste(missing_techs, collapse = ", ")))
#   }
#   return(df)
# }
