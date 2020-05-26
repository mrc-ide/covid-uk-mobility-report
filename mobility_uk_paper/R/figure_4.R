figure_4 <- function(mob_data, pop_density, quartile_colours) {

  pop_density_df <- st_drop_geometry(pop_density) %>%
                      merge(y = mob_data,
                        by.x = "lad",
                        by.y = "local_authority_district")

  # look at data only in lower and upper quartile
  lower_quart <- pop_density_df %>% filter(density_quartile == 1) 
  higher_quart <- pop_density_df %>% filter(density_quartile == 4) 

  lower_min_max <- lower_quart %>% 
    group_by(date) %>%
    summarize(min_density = min(diff),
              max_density = max(diff),
              median_density = median(diff))

  higher_min_max <- higher_quart %>% 
    group_by(date) %>%
    summarize(min_density = min(diff),
              max_density = max(diff),
              median_density = median(diff))

  lower_upper <- bind_rows(lower_min_max %>% mutate(region_type = "rural"), 
                    higher_min_max %>% mutate(region_type = "urban"))

  #days where data was missing
  missing_days <- seq(as.Date("2020-04-14"), as.Date("2020-04-19"), 
                      by = "day")
  #create NA rows so they are not plotted
  NA_rows <-
    rbind(
      cbind(as.character(missing_days),NA,NA,NA,"rural"),
      cbind(as.character(missing_days),NA,NA,NA,"urban")
    ) %>%
    as.data.frame()
  names(NA_rows) <- names(lower_upper)
  lower_upper <- rbind(lower_upper, NA_rows)

  lower_upper$min_density <- as.numeric(lower_upper$min_density)
  lower_upper$max_density <- as.numeric(lower_upper$max_density)
  lower_upper$median_density <- as.numeric(lower_upper$median_density)

  p <- 
    ggplot(data = lower_upper) +
    geom_ribbon(aes(x = date, ymin = min_density, 
                  ymax = max_density, fill = region_type), 
                alpha = 0.2) +
    geom_line(aes(x = date, y = median_density, 
                  col = region_type), 
              lwd = 1) +
    ylab("% Difference in Journeys Made") +
    xlab("Date") +
    scale_fill_manual(name = "region_type",
                      labels= c("rural" = "Low Population Density",
                                "urban"  = "High Population Density"),                               
                      values = c("urban"   = quartile_colours[[4]],
                                "rural"  = quartile_colours[[1]])) +
    scale_colour_manual(name = "region_type",
                    labels= c("rural" = "Low Population Density",
                              "urban"  = "High Population Density"),                               
                    values = c("urban"   = quartile_colours[[4]],
                              "rural"  = quartile_colours[[1]])) +
    theme(legend.position = "bottom",
          panel.grid.major = element_line(colour = gridline_col),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.spacing.x = unit(0.2, "lines"),
          panel.spacing.y = unit(0.2, "lines"),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          legend.title = element_blank()) +
    facet_theme +
    theme(legend.key=element_blank()) +
    geom_vline(xintercept=as.Date("2020-03-23"), 
              linetype="dashed", colour = "#616161")
}