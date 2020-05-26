# colours
col_cases <- brewer.pal(8, "Reds")[6] 
col_2019 <- brewer.pal(8, "Blues")[5] 
col_2020 <- brewer.pal(8, "Blues")[8] 
col_Rt <- brewer.pal(8, "Greens")[5] 
col_inter_corr <- brewer.pal(8, "RdPu")[5] 
col_intra_corr <- brewer.pal(8, "RdPu")[8]
col_text <- "black"  
col_box <- "#2c3e50"  # colour found from running 'calc_element("axis.text.x", theme_tq())'
gridline_col <- "#EDEDED" 

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
                                                color = col_text))
  

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