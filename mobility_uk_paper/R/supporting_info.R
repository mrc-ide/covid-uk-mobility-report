figure_s1 <- function(pop_density, quartile_colours){
  p <-
    ggplot() + 
    geom_sf(data = pop_density, 
      aes(fill = density_quartile), lwd = 0) + 
    plot_theme +
    scale_fill_manual(name = "Population Density Quartile", 
      values=quartile_colours) +
    theme(legend.title = element_text(), legend.position="right")
}

figure_s2 <- function(fb_pop, pop_density, quartile_colours){
  d_sf <- st_as_sf(fb_pop, coords = c("lon", "lat"))
  d_sf <- st_set_crs(d_sf, 4326)
  d_sf <- st_join(d_sf, pop_density)

  d <- st_drop_geometry(d_sf)

  df <- aggregate(d$n_crisis, 
          by = list("date" = d$date,
                    "density_quartile" = d$density_quartile),
          FUN = sum)

  grouped_df <- split(df, f = df$density_quartile)

  pc <- function(i){
    i$percent_change <- i$x/i$x[1] * 100
    return(i)
  }
  grouped_df <- lapply(grouped_df, pc)

  df <- do.call("rbind", grouped_df)

  df$density_quartile <- factor(df$density_quartile)

  p <- 
    ggplot() + 
    geom_line(data = df, aes(x = date, y = percent_change,
              colour = density_quartile), lwd = 1) + 
    ylab("Facebook Population Size as a Percentage of 2020-03-10") + 
    xlab("Date") + 
    plot_theme + 
    scale_colour_manual(name = "Population Density Quartile",
                        values = quartile_colours) +
    theme(legend.title = element_text(),
          text = element_text(size = 11)) + 
    guides(colour = guide_legend(title.position = "top"))
}