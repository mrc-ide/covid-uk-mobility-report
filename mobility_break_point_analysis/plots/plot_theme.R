col_text <- "black"  

# theme for the plots, controlling white space
plot_theme <- theme(legend.position = "bottom",
                    legend.key=element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    panel.spacing.x = unit(0.2, "lines"),
                    panel.spacing.y = unit(0.2, "lines"),
                    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
                    legend.title = element_blank(),
                    panel.border = element_rect(fill = NA, 
                                                size = rel(1/2), 
                                                color = col_text),
                    axis.text=element_text(size=14, face="bold"),
                    axis.title=element_text(size=14,face="bold"))


# theme for facet grid, control label text and box
facet_theme <-  theme(
  strip.text.x = element_text(
    size = 12, color = col_text
  ),
  strip.text.y = element_text(
    size = 12, color = col_text
  ),
  strip.background = element_blank()
)

