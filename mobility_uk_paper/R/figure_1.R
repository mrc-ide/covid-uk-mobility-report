figure_1 <- function(all, shp_lad){
  pc <- function(d){
    d$date <- as.Date(d$date)
    first_week <- seq(min(d$date), min(d$date) + 6, by = "days")
    baseline <- mean(d[d$date %in% first_week,]$n_trips_adjusted)
    d$n_trips_baseline <- rep(baseline, nrow(d))
    d$percent_change <- (d$n_trips_adjusted - d$n_trips_baseline)/
                        d$n_trips_baseline * 100
    d
  }

  grouped <- split(all, f = all$lad)
  grouped <- lapply(grouped, pc)

  all <- do.call("rbind", grouped)
  row.names(all) <- seq(1, nrow(all))

  UK <- map_data("world") %>% filter(region==c("UK", "Ireland"))

  UK <- UK[UK$lat <= 58.7,]

  #for bounds of coloured plotting
  max_change <- ceiling(max(all$percent_change))
  min_change <- floor(min(all$percent_change))
  #make upper and lower bounds equal or colour scheme will be biased
  diff <- max(abs(min_change),abs(max_change))
  max_change <- diff
  min_change <- -diff

  dates <- seq(as.Date("2020-03-18"), 
            as.Date("2020-03-26"),
            by = "days")
  
  df <- all[all$date %in% dates,]

  letters <- LETTERS[1:length(unique(df$date))]
  letter_date_map <- list()
  for (i in 1:length(unique(df$date))){
    d <- unique(df$date)[[i]]
    letter_date_map[[d]] <- letters[[i]]
  }

  labels <- vector("character", length = nrow(df))
  for (i in 1:length(df$date)){
    labels[[i]] <- letter_date_map[[df$date[[i]]]]
  }

  df$label <- labels

  df <- left_join(df, shp_lad, by = "lad")

  d_sh <- st_as_sf(df)
  d_sh <- st_set_crs(d_sh,27700)
  d_sh <- st_transform(d_sh, 4326)

  days <- data.frame(days = paste(weekdays(as.Date(unique(df$date))), 
            unique(df$date), sep = ", "),
            label = unique(df$label))

  p <-  ggplot() + 
        geom_polygon(data = UK, 
          aes(x=long, y = lat, group = group), 
          fill="grey") +
        geom_sf(data = d_sh, 
          aes(fill = percent_change), lwd = 0.1) +
        scale_fill_gradient2(
          low = "#2ECC71",
          mid = "#F9E79F",
          high = "#E74C3C",
          midpoint = 0,
          space = "Lab",
          guide = "colourbar",
          aesthetics = "fill",
          limits = c(min_change, 
                  max_change)) +
        facet_wrap( ~ label, ncol=3) +
        plot_theme + 
        facet_theme + 
        xlab("") +
        ylab("") + 
        theme(strip.text.x = element_blank()) +
        geom_text(data = days, aes(label = days),
          x = Inf, y = -Inf, size = 2, vjust = -0.5, hjust = 1) + 
        geom_text(data = days, aes(label = label),
          x = -Inf, y = Inf, vjust = 1.1, hjust = -0.5)

}