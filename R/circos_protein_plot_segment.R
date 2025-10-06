#### circos.triangulate package ###

devtools::install_deps(dependencies = TRUE)
#' @title circos protein plot segment
#' @param circos_data data.table containing all of the data to plot
#' @param total_track_number total number of tracks to plot (can be less than sources of data if only plotting subset)
#' @param track_id_column name of column containing variable to determine which track e.g. method / data source
#' @param segment_names_column column containing factor to subset data on
#' @param segment_to_display segment to zoom in on
#' @param protein_column column containing names of proteins/metabolites
#' @param beta_column column containing beta value
#' @param se_column column containing standard error
#' @param primary_track variable from "track_id_column" to determine which track is first to plot and used to generate names
#' @param odds_ratios (optional) boolean value, whether to generate and plot odds ratios from beta and se (default = FALSE)
#' @param error_bar_ends (optional) boolean value, whether to add ends to error bars (default = TRUE)
#' @param custom_palette provide custom colour palette, supports viridis or custom vector
#' @param axis_label_size (optional) custom label size for axis
#' @param text_size (optional) custom text size for outer labels
#' @param point_size (optional) custom size for points on plots
#' @param add_legend add in track legend (default = FALSE)
#' @param track_name_column (optional) column containing track names (default = uses track_id_column)
#' @export circos_protein_plot_segment
circos_protein_plot_segment <- function(circos_data,
                                total_track_number,
                                track_id_column,
                                segment_names_column,
                                segment_to_display,
                                protein_column,
                                beta_column,
                                se_column,
                                odds_ratios,
                                custom_palette,
                                primary_track,
                                error_bar_ends,
                                axis_label_size,
                                text_size,
                                point_size,
                                add_legend = FALSE,
                                track_name_column = track_id_column,
                                highlights,
                                highlight_prots,
                                highlight_colour) {
  if(missing(primary_track)) {
    primary_track <- 1
  }

  if(missing(custom_palette)) {
    custom_palette <- viridis::viridis(n = total_track_number+5)[total_track_number+5:1]
  }

  if(missing(odds_ratios)) {
    odds_ratios <- F
  }
  if(missing(error_bar_ends)) {
    error_bar_ends <- T
  }

  if(missing(axis_label_size)) {
    axis_label_size = 0.5
  }

  if(missing(text_size)) {
    text_size = 1.5
  }

  if(missing(point_size)) {
    point_size = 1
  }
  if(missing(highlights)) {
    highlights = F
  }

  ### determine if colour is dark or light for chosing point colour

  colour_contrast_checker <- function(colour) {
    redgreenblue <- col2rgb(colour)
    red <- redgreenblue[1]
    green <- redgreenblue[2]
    blue <- redgreenblue[3]

    brightness = sqrt(0.299*red^2 + 0.587*green^2 + 0.144*blue^2)

    brightness

    if (brightness > 129) {
      "#000000"
    } else {
      "#FFFFFF"
    }
  }

  ### lighten/darken value
  lighten_or_darken_value <- function(colour) {
    redgreenblue <- col2rgb(colour)
    red <- redgreenblue[1]
    green <- redgreenblue[2]
    blue <- redgreenblue[3]

    brightness = sqrt(0.299*red^2 + 0.587*green^2 + 0.144*blue^2)


    if (brightness > 129) {
      red_new <- red-20
      green_new <- green-20
      blue_new <- blue-20

      new_colour <- rgb(red=as.integer(red_new)/255, green=as.integer(green_new)/255, blue=as.integer(blue_new)/255)
      new_colour
    } else {
      red_new <- red+50
      green_new <- green+50
      blue_new <- blue+50

      new_colour <- rgb(red=as.integer(red_new)/255, green=as.integer(green_new)/255, blue=as.integer(blue_new)/255)
      new_colour
    }
  }

  circos_data$tier_section <- circos_data[[segment_names_column]]
  circos_data <- circos_data %>% filter(tier_section == segment_to_display)
  circos_data$protein <- circos_data[[protein_column]]
  circos_data$track_id <- circos_data[[track_id_column]]
  circos_data$track_names <- circos_data[[track_name_column]]

  if(missing(primary_track)) {
    primary_track <- as.factor(circos_data$track_id)[1]
  }

  circos_data <- circos_data %>% mutate(track_id = factor(track_id))


  wrapper <- function(x, ...)
  {
    paste(strwrap(x, ...), collapse = "\n")
  }



  col_text <- "grey"


  if(odds_ratios == T) {
    generate_odds_ratios <- function(data)
    {
      data$lo_ci <- data[[beta_column]] - 1.96 * data[[se_column]]
      data$up_ci <- data[[beta_column]] + 1.96 * data[[se_column]]
      data$or <- exp(data[[beta_column]])
      data$lo_ci95 <- exp(data$lo_ci)
      data$up_ci95 <- exp(data$up_ci)
      return(data)
    }

    circos_data <- generate_odds_ratios(circos_data)

    y_value <- "or"
    null <- 1

  }
  if (odds_ratios == F) {
    y_value <- beta_column

    circos_data$lo_ci95 <- circos_data[[beta_column]] - 1.96 * circos_data[[se_column]]
    circos_data$up_ci95 <- circos_data[[beta_column]] + 1.96 * circos_data[[se_column]]

    null <- 0

  }



  circos_data_track_main <- circos_data %>% dplyr::group_by(protein) %>% filter(track_id == primary_track)

  freq_table <- circos_data_track_main %>% dplyr::group_by(tier_section) %>% summarise(length(tier_section)) %>% as.data.frame()

  circos_data_track_main$x <- with(circos_data_track_main %>% filter(track_id_column == primary_track), ave(seq_along(circos_data_track_main[[segment_names_column]]),
                                                                                                            circos_data_track_main[[segment_names_column]], FUN = seq_along))
  circos_data_track_main$order <- seq_along(circos_data_track_main$protein)

  circos_data_track_main$n <- NA
  circos_data_track_main$ncat <- NA

  for (i in 1:nrow(circos_data_track_main)) {
    circos_data_track_main$n[i] <- as.numeric(nrow(subset(circos_data_track_main, circos_data_track_main[[segment_names_column]] ==
                                                            circos_data_track_main[[segment_names_column]][i])))
    circos_data_track_main$ncat[i] <- circos_data_track_main$x[i]/circos_data_track_main$n[i]
  }

  legend_names <- vector()

  circlize::circos.clear()
  col_text <- "grey40"

  circlize::circos.par("track.height"= total_track_number, gap.degree = 270, start.degree=90, cell.padding=c(0, 0, 0, 0), canvas.xlim = c(0,1.05), canvas.ylim=c(0, 1), circle.margin=rep(1, 4))
  circlize::circos.initialize(factors=circos_data_track_main$tier_section, xlim=matrix(c(rep(0, nrow(freq_table)), freq_table$`length(tier_section)`), ncol=2))
  circlize::circos.par("points.overflow.warning" = F)

  suppressMessages(
  circlize::circos.trackPlotRegion(factors = circos_data_track_main$tier_section,
                                   track.index = 1, x = circos_data_track_main$x - 0.5, ylim = c(0, 1),
                                   track.height = 0.05, panel.fun = function(x, y) {
                                     chr = circlize::get.cell.meta.data("sector.index")
                                     xlim = circlize::get.cell.meta.data("xlim")
                                     ylim = circlize::get.cell.meta.data("ylim")
                                     circlize::circos.text(circos_data_track_main$order -0.5, mean(ylim), sector.index = circlize::get.current.sector.index(), labels = circos_data_track_main %>% filter(tier_section == chr) %>% pull(protein), facing = "clockwise", niceFacing = TRUE, adj=0.1, cex = text_size/1.1, col = 'black')
                                   }, bg.border = NA)
  )

  circlize::circos.trackPlotRegion(factors = circos_data_track_main$tier_section,
                                   track.index = 2, x = circos_data_track_main$ncat, ylim = c(0, 1),
                                   track.height = 0.08, panel.fun = function(x, y) {
                                     chr = circlize::get.cell.meta.data("sector.index")
                                     xlim = circlize::get.cell.meta.data("xlim")
                                     ylim = circlize::get.cell.meta.data("ylim")
                                     circlize::circos.rect(xlim[1], 0, xlim[2], 1, border = NA,
                                                           col = 'grey80')
                                     circlize::circos.text(mean(xlim), mean(ylim)*1.2, chr,
                                                           cex = text_size, facing = "bending.outside", niceFacing = TRUE,
                                                           col = col_text)
                                   }, bg.border = NA)

  for(i in 1:total_track_number) {
    legend_names[i] <- circos_data$track_names[circos_data$track_id == i] %>% unique()
    assign(paste0("circos_data_track", i), circos_data %>% filter(track_id == i) %>% merge(circos_data_track_main[c("protein", "x", "order", "n","ncat", "tier_section")]))

    circlize::circos.track(factors = circos_data$tier_section,
                           track.index = i+2, x = circos_data$ncat, ylim=c(min(get(paste0("circos_data_track", i))$lo_ci95), max(get(paste0("circos_data_track", i))$up_ci95)),
                           track.height = 0.6/total_track_number, panel.fun = function(x, y) {
                             chr = circlize::get.cell.meta.data("sector.index")
                             xlim = circlize::get.cell.meta.data("xlim")
                             ylim = circlize::get.cell.meta.data("ylim")
                             circlize::circos.rect(xlim[1], ylim[1], xlim[2], ylim[2], border = NA,
                                                   col = custom_palette[i])

                             circlize::circos.axis(
                               h = null,
                               major.at = NULL,
                               labels = F,
                               major.tick = F,
                               sector.index = circlize::get.current.sector.index(),
                               track.index = circlize::get.current.track.index(),
                               col = lighten_or_darken_value(custom_palette[i]),
                               lwd = point_size*2)

                             circlize::circos.points(
                               x=get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(x) -0.5,
                               y=get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(y_value),
                               pch = 16,
                               cex = point_size,
                               col = colour_contrast_checker(custom_palette[i]))

                             circlize::circos.segments(
                               x0 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(x) -0.5,
                               y0 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(lo_ci95),
                               x1 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(x) -0.5,
                               y1 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(up_ci95),
                               col = colour_contrast_checker(custom_palette[i]),
                               straight = T,
                               lwd = point_size*1.5)

                             if(error_bar_ends == T) {
                               circlize::circos.segments(
                                 y0 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(lo_ci95),
                                 x0 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(x) - 0.5 +0.05,
                                 y1 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(lo_ci95),
                                 x1 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(x) - 0.5 -0.05,
                                 col = colour_contrast_checker(custom_palette[i]),
                                 straight = T,
                                 lwd = point_size*1.5)

                               circlize::circos.segments(
                                 y0 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(up_ci95),
                                 x0 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(x) - 0.5 +0.05,
                                 y1 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(up_ci95),
                                 x1 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(x) - 0.5 -0.05,
                                 col = colour_contrast_checker(custom_palette[i]),
                                 straight = T,
                                 lwd = point_size*1.5)
                             }
                             circlize::circos.yaxis(
                               side = "left",
                               at = NULL,
                               labels = TRUE,
                               tick = TRUE,
                               labels.cex = axis_label_size,
                               tick.length = 0.2,
                               labels.niceFacing = T
                             )}, bg.border = NA)
  }
  if(highlights == TRUE) {

    if(missing(highlight_colour)) {
      highlight_colour = paste0(lighten_or_darken_value(custom_palette[1]), 80)
    } else {
      if(nchar(highlight_colour) < 9) {
        highlight_colour = paste0(lighten_or_darken_value(highlight_colour), 80)
      } else {
        highlight_colour <- strsplit(highlight_colour, "FF") %>% paste0("80")
      }}

    circos_data_track_main$start.angle = seq((90/(length(circos_data_track_main$x)+1)), 90, length.out=length(circos_data_track_main$x))[length(circos_data_track_main$x):1]+0.25
    circos_data_track_main$end.angle = (circos_data_track_main$start.angle)-(90/(length(circos_data_track_main$protein)+1))

    for(h in highlight_prots) {
      circlize::draw.sector(
        start.degree = circos_data_track_main$start.angle[grep(h, circos_data_track_main$protein)],
        end.degree = circos_data_track_main$end.angle[grep(h, circos_data_track_main$protein)],
        rou1 = 1.1,
        rou2 = 0.15,
        col = highlight_colour,
        border = NA)
    }
  }
  if(add_legend == TRUE) {
    plot_legend <-
      ComplexHeatmap::Legend(labels = legend_names[1:total_track_number],
                             legend_gp = grid::gpar(fill = custom_palette[1:total_track_number]),
                             title = "Tracks",
                             grid_height = grid::unit(point_size/1.8, "cm"),
                             grid_width = grid::unit(point_size/1.8, "cm"),
                             labels_gp = gpar(fontsize = text_size*5),
                             title_gp = gpar(fontsize = text_size*5,
                                             fontface = "bold"),
                             gap = unit(point_size/2, "cm"))

    ComplexHeatmap::draw(plot_legend,
                         x = grid::unit(point_size/100, "npc"),
                         y = grid::unit(point_size/100, "npc"),
                         just = c("left", "bottom"),
                         test=F)
  }
}

