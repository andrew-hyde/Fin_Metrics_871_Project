
graph_1_reit_funcs <- function(df_data, title, subtitle, caption, xlabel, ylabel){

    library(tidyverse)
################################################################################





################################################################################

graph <- df_data %>% ggplot() +

    geom_line(aes(x = date, y = Return, color = Tickers), alpha = 0.5,
              size = 0.2) +

    #geom_line(aes(x = date, y = value, color = label), alpha = 0.8,
    # size = 1) +

    facet_wrap(~Tickers, scales = "free_y") +


    labs(title = title,
     subtitle = subtitle,
     caption = caption,
     y = ylabel,
     x = xlabel) +

    theme() +
    theme(axis.text.x = element_blank(),
          axis.text.y=element_blank()) +
    theme(legend.position="none")

#scale_x_discrete(guide = guide_axis(n.dodge=1.2))

graph

}

# graph_1_reit_func(df_data = data_combined_ALSI_REIT,
#                   title = "JSE listed REITs over time ",
#                   subtitle = "",
#                   caption = "Note how many REITs have complete data sets",
#                   xlabel = "",
#                   ylabel = "")




