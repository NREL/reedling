

#' @export
plot_map <- function(ba_shp, df, fill_col,
                     year="last", diff=F,
                     facet_col=NULL, facet_row=NULL,
                     scale=1,
                     legendname="", title="", titleheight=1,
                     palette="Blues"){
  #TODO: check columns

  # subset year to plot
  if (year == "last"){
    year <- max(df$t)
  }
  df_plot <- df[t==year]
  df_plot$fillval <- df_plot[,get(fill_col)] / scale

  # finish merge with actual shape file  data
  df_plot <- merge(ba_shp, df_plot, by="r")

  # handle NA values
  df_plot$fillval <- ifelse(is.na(df_plot$fillval), 0, df_plot$fillval)

  # get states
  st_shp <- sf::st_buffer(ba_shp,0) %>%
    dplyr::group_by(st) %>%
    dplyr::summarize(geometry = sf::st_union(geometry))

  plot_out <- ggplot2::ggplot() +
              #ggplot2::geom_sf(data=ba_shp, fill=NA, lwd=0) +
              ggplot2::geom_sf(data=df_plot, mapping=ggplot2::aes(fill=fillval), linewidth=0, color=NA) + #"lightgrey") +
              ggplot2::geom_sf(data=st_shp, fill=NA) +
              ggplot2::theme_void() +
              ggplot2::theme(legend.position="bottom",
                             plot.margin=ggplot2::unit(c(0,0,0,0), "null"),
                             plot.title.position = "plot",
                             plot.title = element_text(hjust=0.1, lineheight = titleheight, size=8),
                             plot.subtitle = element_blank()) +
              ggplot2::guides(fill=ggplot2::guide_colourbar(title.position="top",
                                                            title.hjust = 0.5,
                                                            barwidth = 12,
                                                            ticks.colour = "black",
                                                            frame.colour = "black",
                                                            frame.linewidth = 0.5,
                                                            title.theme=ggplot2::element_text(face="bold"))) +
              #ggplot2::ggtitle(stringr::str_wrap(title, width=30))
              ggplot2::ggtitle(title)

  # fill formatting (separate approach for difference plots)
  if(diff){
    limits <- max(abs(df_plot$fillval)) * c(-1,1)
    plot_out <- plot_out +
                ggplot2::scale_fill_distiller(legendname, palette=palette,
                                              direction=1, na.value="white",
                                              type="div", limit=limits)
  } else {
    legendname <- paste(year, legendname)
    plot_out <- plot_out +
                #ggplot2::scale_fill_distiller(legendname, palette=palette,
                #                              direction=1, na.value="white")
                scale_fill_gradientn(legendname, colours=c("white", "#8C510A", "#01665E", "#6BAED6", "#084594"),
                                       na.value="white")
    #strsplit(test, split="|", fixed=T)

  }
  # add faceting if requested
  if(!is.null(facet_row) | !is.null(facet_col)) {
    plot_out <- plot_out +
                ggplot2::facet_grid(as.formula(paste0(facet_col, "~", facet_row)))
  }
  #ggplot2::scale_fill_gradientn(legendname, colours = c("white", "brown", "green", "blue"))

  return(plot_out)
}


#' @export
plot_map_panel <- function(df, col, baseval, compval, techval, var, legendname, scale=1) {

  dfsub <- df[tech==techval]

  #TODO: scale data here to avoid passing more parameter,
  # also use this + var to determine legend name and units dynamically?
  # scale values
  dfsub[,(var) := get(var) / scale]

  units <- gsub(".*_", "", var)
  varleg <- gsub("_.*", "", var)

  if(scale == 1000){
    if(units == "MW"){
      units <- "GW"
    } else if(units == "MWh"){
      units <- "GWh"
    }
  } else if(scale == 1e6){
    if(units == "MW"){
      units <- "TW"
    } else if(units == "MWh"){
      units <- "TWh"
    }
  }


  legendname = paste0(techval, " ", varleg, " [", units, "]")


  dfplot_base <- dfsub[get(col) == baseval]
  dfplot_comp <- dfsub[get(col) == compval]

  p1 <- plot_map(ba_shp, dfplot_base,
                 fill_col=var,
                 year=2035, legendname = paste0(techval, " ", varleg, " [",units, "]"),
                 palette = "PuBuGn", title=baseval)

  p2 <- plot_map(ba_shp, dfplot_comp,
                 fill_col=var,
                 year=2035, legendname = paste0(techval, " ", varleg, " [",units, "]"),
                 palette = "PuBuGn", title=compval)

  dfsubdiff <- calc_difference(dfsub, widecol=col, valcol=var, baseval=baseval, compval=compval)

  p3 <- plot_map(ba_shp, dfsubdiff,
                 fill_col=var, diff=T,
                 year=2035, legendname = paste0("Difference [", units, "]"),
                 palette = "RdBu", title=paste(compval, baseval, sep=" - "))

  p_out <- grid.arrange(p1, p2, p3, nrow=1)

  return(p_out)

}

