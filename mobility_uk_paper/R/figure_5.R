figure_5 <- function(fb, pop_density, quartile_colours, 
                    log_scale = TRUE){
  fb$date <- as.Date(fb$date)
  fb$days <- weekdays(fb$date)
  fb_wednesdays <- fb[fb$days == "Wednesday",]

  fb_wednesdays$lad <- snakecase::to_snake_case(fb_wednesdays$lad)

  d <- st_drop_geometry(pop_density) %>%
        right_join(fb_wednesdays, 
                    by = "lad")

  d$mean_trip_length <- d$n_trips_adjusted/
    d$total_distance_travelled_adjusted

  letters <- LETTERS[1:length(unique(d$density_quartile))]
  letter_date_map <- list()
  for (i in 1:length(unique(d$density_quartile))){
    q <- sort(unique(d$density_quartile))[[i]]
    letter_date_map[[q]] <- letters[[i]]
  }

  labels <- vector("character", length = nrow(d))
  for (i in 1:length(d$density_quartile)){
    labels[[i]] <- letter_date_map[[d$density_quartile[[i]]]]
  }
  d$label <- labels

  d$date <- factor(d$date)

  label_df <- unique(d[,c("label", "density_quartile")])
  label_df$density_quartile <- as.numeric(as.character(
                                label_df$density_quartile))
  label_df <- label_df[order(label_df$density_quartile),]  

  p <- 
    ggplot(data = d,aes(x = date, y = mean_trip_length)) + 
    geom_violin(aes(colour = label),
                draw_quantiles = c(0.25,0.5,0.75),
                lwd = 1) +
    plot_theme + 
    scale_colour_manual(name = "Population Density Quartile", 
      values=quartile_colours) +
    ylab('Mean Distance per Journey (km)') + 
    xlab('Date') + 
    facet_wrap(~ label, ncol = 2) + 
    facet_theme + 
    theme(legend.position="none",
      strip.text.x = element_blank()) +
      geom_text(data = label_df, aes(label = label),
          x = -Inf, y = Inf, vjust = 1.1, hjust = -0.5) 

    if (log_scale){
      p <- p + 
        scale_y_log10(limits = c(NA, max(d$mean_trip_length)))
    }else{
      p
    }
}