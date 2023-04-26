#' Load tech mapping
#'
#' Load mapping for generator techs. Defaults to taking bokehpivot mapping in "tech_map.csv" from a specified ReEDS folder,
#' but can also be used to load any custom mapping.
#'
#' @param path path to ReEDS repository or a directory with a custom tech mapping
#' @param reeds_default Boolean variable indicating whether to use the ReEDS default tech mapping in bokehpivot (defaults to True)
#' @param filename optional parameter for specifying filename of custome tech mapping
#'
#' @return dataframe of tech categories
#' @export
set_tech_map <- function(path=NULL, reeds_default=T, filename="tech_map.csv"){
  #TODO: reeds_default is clunky here
  if (reeds_default){
    print("Reading default tech mapping from bokehpivot")
    tech_map <- read.csv(file.path(path, "postprocessing", "bokehpivot", "in", "reeds2", filename))
  } else {
    print(paste0("Reading custom tech mapping in ", file.path(path, filename)))
    tryCatch(
      {
        tech_map <- read.csv(file.path(path, filename))
      },
      error=function(cond)
      {
        print("Could not read tech mapping file; check path")
      }
    )
  }
  return(tech_map)
}

#' Load tech colors
#' @export
set_tech_colors <- function(path=NULL, reeds_default=T, filename="tech_style.csv"){
  if (reeds_default){
    print("Reading default tech style from bokehpivot.")
    tech_style_in <- read.csv(file.path(path, "postprocessing", "bokehpivot", "in", "reeds2", filename), stringsAsFactors = F)
  } else {
    print("Reading custom tech style.")
    tryCatch(
      {
        tech_style_in <- read.csv(file.path(path, filename), stringsAsFactors = F)
      },
      error=function(cond)
      {
        print("Could not read tech style file; check path")
      }
    )
  }
  tech_style <- tech_style_in$color
  names(tech_style) <- tech_style_in$order
  return(tech_style)
}

#' Format techs
#'
#' Recategorizes generator techs based on mapping.
#'
#' @param path path to ReEDS repository or a directory with a custom tech mapping
#' @param reeds_default Boolean variable indicating whether to use the ReEDS default tech mapping in bokehpivot (defaults to True)
#' @param filename optional parameter for specifying filename of custome tech mapping
#'
#' @return Data Table of tech categories
#' @import data.table
#' @import stringr
#' @import plyr


#' @export
format_techs <- function(df, mapping, col_from="raw", col_to="display"){

  # check for the correct columns
  check_columns(df, c("i"))

  if (!col_to %in% colnames(mapping) | !col_from %in% colnames(mapping)){
    print(paste0("Cannot find column in tech mapping file; specify one of the following columns: ", paste0(colnames(mapping), collapse =", ")))
    return(NULL)
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
  df$tech <- stringr::str_replace_all(tolower(df$i), stats::setNames(as.character(mapping[, col_to]),
                                                                     as.character(mapping[, col_from])))
  # check for unmapped technologies
  missing_techs <- unique(df$tech[is.na(df$tech)])
  if (length(missing_techs) > 1){
    print(paste("The following technologies were unmapped:", paste(missing_techs, collapse = ", ")))
  }
  df_mapped <- df[,by=.(i,tech), .(obs=length(i))]
  # TODO: format output here
  print("Mapped the following technologies: ")
  print(df_mapped)
  return(df)
}



# deflates monetary values
# TODO: separate deflator?
#' @export
deflate_values <- function(df, col, year_to, reedspath, year_from=2004){
  deflator <- data.table:::fread(file.path(reedspath, "inputs", "deflator.csv"))
  colnames(deflator) <- c("t","deflator")
  # deflator values are multiplied to get to $2004, so
  # divide by deflator to get to
  nom <- deflator[t==year_from]$deflator    # first convert to $2004
  denom <- deflator[t==year_to]$deflator  # second convert to target year
  df$new <- df[, get(col)] * (nom/denom)
  setnames(df, col, paste(col, year_from, sep="_"))
  setnames(df, "new", paste(col, year_to, sep="_"))
  return(df)
}

#' @export
map_rs_to_r <- function(df, reedspath) {
  rsmap <- read.csv(file.path(reedspath, "inputs", "rsmap.csv"),
                    col.names = c("r", "rs"), stringsAsFactors = F)
  df$r <- plyr::mapvalues(df$r, from=rsmap$rs, to=rsmap$r, warn_missing = F)
  return(df)
}

#' @export
categorize_scenarios <- function(df, scenmapping){
  scenmapping <- data.frame(lapply(scenmapping, as.character), stringsAsFactors=FALSE)
  # add new scenario columns
  newcols <- unique(scenmapping$col)
  df[, (newcols) := list(NA_character_)]
  # iterate over mappings
  for(row in 1:nrow(scenmapping)){
    df[, (scenmapping[row, "col"])] <- ifelse(is.na(df[,get(scenmapping[row, "col"])]),
                                                  stringr::str_extract(df$run, scenmapping[row, "find"]),
                                                  df[,get(scenmapping[row, "col"])])
  }
  # TODO: reformat to have one output (use runs?)
  # TODO: improve printing format?
  df_mapping_results <- list()
  for (col in newcols){
    # check mapping is reasonable
    #print(paste(col, "scenario"))
    #print(table(df$run, df[,get(col)]))
    df_mapping_results[[col]] <- data.frame(table(df$run, df[,get(col)]))
    # order factors
    levelvals <- unique(scenmapping[scenmapping$col == col, "find"])
    # TODO: toggle factor levels since this can break if adding more
    #df[, (col) := factor(get(col), levels=levelvals)]
  }
  df_mapping_results_out <- do.call(cbind, df_mapping_results)
  print(df_mapping_results_out)

  return(df)
}

#' @export
calc_difference <- function(df, widecol, valcol, baseval, compval, excludecol=NULL){
  if(is.null(excludecol) | valcol == "run"){
    df_wide <- tidyr::pivot_wider(df, names_from=widecol, values_from=valcol, values_fill = 0)
  } else {
    df_wide <- tidyr::pivot_wider(df, names_from=widecol, values_from=valcol, values_fill = 0, id_cols=-excludecol)
  }
  df_wide <- data.table(df_wide)
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

  check_columns(df, c("r_from", "r_to"))
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
