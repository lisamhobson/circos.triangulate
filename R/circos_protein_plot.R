#### circos.triangulate package ###

# devtools::install_deps(dependencies = TRUE)
#' @title Circos Protein Plot
#' @description
#' A solution to displaying multiple sources of data for the same values on multiple tracks with segements based on custom criteria, provided with a data frame, customise which tracks data is displayed on, how many tracks are required, the segments to subset the data by, custom colour scheme, generate odds ratios and deal with missing data. This package uses the 'circlize' package to create beautiful publication ready circular plots for protein or metabolite data from a number of different analyses, adjusting track size for the best visualisation.
#' @import circlize
#' @import dplyr
#' @import data.table
#' @import viridis
#' @param circos_data data.table containing all of the data to plot
#' @param total_track_number total number of tracks to plot (can be less than sources of data if only plotting subset)
#' @param track_id_column name of column containing variable to determine which track e.g. method / data source
#' @param segment_names_column column containing factor to subset data on
#' @param protein_column column containing names of proteins/metabolites
#' @param beta_column column containing beta value
#' @param se_column column containing standard error
#' @param primary_track variable from "track_id_column" to determine which track is first to plot and used to generate names
#' @param odds_ratios (optional) boolean value, whether to generate and plot odds ratios from beta and se (default = FALSE)
#' @param error_bar_ends (optional) boolean value, whether to add ends to error bars (default = TRUE)
#' @param custom_palette provide custom colour palette, supports viridis or custom vector
#' @export
circos_protein_plot <- function(circos_data,
                                total_track_number,
                                track_id_column,
                                segment_names_column,
                                protein_column,
                                beta_column,
                                se_column,
                                odds_ratios,
                                custom_palette,
                                primary_track,
                                error_bar_ends) {

  if(missing(custom_palette)) {
    custom_palette <- viridis::viridis(n = total_track_number+5)[total_track_number+5:1]
  }

  if(missing(odds_ratios)) {
    odds_ratios <- F
  }
  if(missing(error_bar_ends)) {
    error_bar_ends <- T
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
  circos_data$protein <- circos_data[[protein_column]]
  circos_data$track_id <- circos_data[[track_id_column]]

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
    y_value <- "b"

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

  circlize::circos.clear()
  col_text <- "grey40"

  circlize::circos.par("track.height"= total_track_number, gap.degree = 15, start.degree=90, cell.padding=c(0, 0, 0, 0), circle.margin=rep(0.1, 4))
  circlize::circos.initialize(factors=circos_data_track_main$tier_section, xlim=matrix(c(rep(0, nrow(freq_table)), freq_table$`length(tier_section)`), ncol=2))
  circlize::circos.par("points.overflow.warning" = F)


  circlize::circos.trackPlotRegion(factors = circos_data_track_main$tier_section,
                                   track.index = 1, x = circos_data_track_main$x - 0.5, ylim = c(0, 1),
                                   track.height = 0.05, panel.fun = function(x, y) {
                                     chr = circlize::get.cell.meta.data("sector.index")
                                     xlim = circlize::get.cell.meta.data("xlim")
                                     ylim = circlize::get.cell.meta.data("ylim")
                                     circlize::circos.text(circos_data_track_main$order -0.5, mean(ylim), sector.index = circlize::get.current.sector.index(), labels = circos_data_track_main %>% filter(tier_section == chr) %>% pull(protein), facing = "clockwise", niceFacing = TRUE, adj=0.1, cex = 1, col = 'black')
                                   }, bg.border = NA)

  circlize::circos.trackPlotRegion(factors = circos_data_track_main$tier_section,
                                   track.index = 2, x = circos_data_track_main$ncat, ylim = c(0, 1),
                                   track.height = 0.08, panel.fun = function(x, y) {
                                     chr = circlize::get.cell.meta.data("sector.index")
                                     xlim = circlize::get.cell.meta.data("xlim")
                                     ylim = circlize::get.cell.meta.data("ylim")
                                     circlize::circos.rect(xlim[1], 0, xlim[2], 1, border = NA,
                                                           col = 'grey80')
                                     circlize::circos.text(mean(xlim), mean(ylim), chr,
                                                           cex = 1, facing = "bending.outside", niceFacing = TRUE,
                                                           col = col_text)
                                   }, bg.border = NA)

  for(i in 1:total_track_number) {
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
                               col = lighten_or_darken_value(custom_palette[i]))

                             circlize::circos.points(
                               x=get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(x) -0.5,
                               y=get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(y_value),
                               pch = 16,
                               cex = .5,
                               col = colour_contrast_checker(custom_palette[i]))

                             circlize::circos.segments(
                               x0 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(x) -0.5,
                               y0 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(lo_ci95),
                               x1 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(x) -0.5,
                               y1 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(up_ci95),
                               col = colour_contrast_checker(custom_palette[i]),
                               straight = T)

                             if(error_bar_ends == T) {
                               circlize::circos.segments(
                                 y0 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(lo_ci95),
                                 x0 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(x) - 0.5 +0.05,
                                 y1 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(lo_ci95),
                                 x1 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(x) - 0.5 -0.05,
                                 col = colour_contrast_checker(custom_palette[i]),
                                 straight = T)

                               circlize::circos.segments(
                                 y0 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(up_ci95),
                                 x0 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(x) - 0.5 +0.05,
                                 y1 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(up_ci95),
                                 x1 = get(paste0("circos_data_track", i)) %>% filter(tier_section == circlize::get.cell.meta.data("sector.index")) %>% pull(x) - 0.5 -0.05,
                                 col = colour_contrast_checker(custom_palette[i]),
                                 straight = T)
                             }
                             circlize::circos.yaxis(
                               side = "left",
                               at = NULL,
                               labels = TRUE,
                               tick = TRUE,
                               labels.cex = .4,
                               tick.length = 0.2,
                               labels.niceFacing = T
                             )}, bg.border = NA)
  }

}
