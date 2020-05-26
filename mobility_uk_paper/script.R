all_data <- import_data()
fb <- all_data[[1]]
fb_pop <- all_data[[2]]
mno1 <- all_data[[3]]
lads <- all_data[[4]]
shp_lads <- all_data[[5]]

#for plotting population density
quartile_colours <- c("#084594", "#382E67", "#69173A", "#99000D")
pop_density <- get_pop_density(mno1, shp_lads)

dir.create("plots")

fig1 <- figure_1(fb, shp_lads)
ggsave("plots/paper_maps.png", fig1, width = 12, height = 10.6, 
  dpi = 700)

#data for figures 2 and 3 requires extra formatting step
formatted <- cities_and_countries(fb, mno1) 
city_country <- formatted[[1]]
mob_data <- formatted[[2]]

fig2 <- figure_2(city_country)
ggsave("plots/city_country_plot.png", fig2, dpi = 600)

#figure 3 is plotted in separate task which fits the segmented linear models

fig4 <- figure_4(mob_data, pop_density, quartile_colours)
ggsave("plots/rural_urban_plot.png", fig4, dpi = 700, width = 6, 
  height = 4)

fig5 <- figure_5(fb, pop_density, quartile_colours)
ggsave("plots/journey_length_distributions.png", fig5, dpi = 700)

#supplementary figures
figS1 <- figure_s1(pop_density, quartile_colours)
ggsave("plots/population_density_map.png", figS1, dpi = 700)
figS2 <- figure_s2(fb_pop, pop_density, quartile_colours)
ggsave("plots/facebook_population_by_density_quartile.png", figS2, 
  dpi = 700)
