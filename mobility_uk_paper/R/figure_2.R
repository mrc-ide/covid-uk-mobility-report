figure_2 <- function(city_country){

  city_plot_labels <- c("london" = "London",
                  "glasgow_city" = "Glasgow",
                  "belfast" = "Belfast",
                  "cardiff" = "Cardiff",
                  "birmingham" = "Birmingham",
                  "manchester" = "Manchester")

  #rename glasgow_city to glasgow
  city_country$area <- stringr::str_replace(city_country$area, 
                                            "_city", 
                                            "")
  #remove underscore from NI
  city_country$area <- stringr::str_replace(city_country$area, 
                                            "_", 
                                            " ")

  city_country$area <- snakecase::to_title_case(city_country$area)
  city_country$provider <- snakecase::to_title_case(city_country$provider)


  letters <- LETTERS[1:length(unique(city_country$area))]
  letter_date_map <- list()
  for (i in 1:length(unique(city_country$area))){
    d <- as.character(unique(city_country$area)[[i]])
    letter_date_map[[d]] <- letters[[i]]
  }

  labels <- vector("character", length = nrow(city_country))
  for (i in 1:length(city_country$area)){
    labels[[i]] <- letter_date_map[[city_country$area[[i]]]]
  }
  city_country$label <- labels

  #order that facet plots will be placed in
  city_country$area <- factor(city_country$area,
                            levels = c("England",
                                      "London",
                                      "Wales",
                                      "Cardiff",
                                      "Scotland",
                                      "Glasgow",
                                      "Northern Ireland",
                                      "Belfast"))
  city_country$label <- factor(city_country$label)

  city_country[city_country$provider == "Mobile Phone",]$provider <- "O2"

  areas <- data.frame(area = levels(city_country$area),
                    label = levels(city_country$label))
  areas$label2 <- sprintf("%s (%s)",areas$label, areas$area)

  city_country_plot <- 
    ggplot(city_country,
      aes(x = date,
          y = diff,
          col = provider)) +
    geom_line(lwd = 1) +
    ylim(c(-100, 50)) +
    ylab("% Difference in Journeys Made") +
    xlab("Date") + 
    theme(legend.position = "bottom",
          legend.key=element_blank(),
          panel.grid.major = element_line(colour = gridline_col),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.spacing.x = unit(0.2, "lines"),
          panel.spacing.y = unit(0.2, "lines"),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          legend.title = element_blank(),
          panel.border = element_rect(fill = NA,
                                      size = rel(1/2),
                                      color = col_text)) +
    facet_theme +
    scale_color_manual(name = "city",
                      labels=c("Facebook", "O2"),
                      values = c("Facebook" = brewer.pal(8, "Blues")[7],
                                  "O2" = brewer.pal(8, "Reds")[6] )) +
    theme(legend.position = "bottom",
          legend.key=element_blank()) +
    facet_wrap(~label, nrow = 4) + 
    theme(strip.text.x = element_blank()) +
    geom_text(data = areas, aes(label = area),
      x = Inf, y = -Inf, 
      inherit.aes = FALSE, 
    size = 3, vjust = -0.5, hjust = 1)  + 
    geom_text(data = areas, aes(label = label),
            x = -Inf, y = Inf, vjust = 1.1, hjust = -0.1, 
            inherit.aes = FALSE) +
    geom_vline(xintercept=as.Date("2020-03-23"), 
                      linetype="dashed", colour = "#616161")
}