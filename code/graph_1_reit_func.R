
graph_1_reit_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){

    library(tidyverse)
################################################################################


# add individual property stock and their weighted returns to perform DCC
property_returns <- data_ALSI_returns %>%
        filter(Sector == "Property") %>%
        select(date, Tickers, Return, J433, Sector) %>%
        na.omit(J433) %>% # remove observations that don't have weights i.e. NA
        arrange(date, Tickers) %>%
        mutate(daily_weighted_returns = Return * J433) %>%
        select(date, Tickers, daily_weighted_returns) # %>%
        #spread(Tickers, daily_weighted_returns)



################################################################################

graph <- property_returns %>% ggplot() +

    geom_line(aes(x = date, y = daily_weighted_returns, color = Tickers), alpha = 0.5,
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




