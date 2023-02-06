
graph_cum_return_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){

    library(tidyverse)

################################################################################

#   ALSI less REITs

ALSI_returns_performance <- data_ALSI_returns %>%
        filter( !Sector %in% "Property") %>%
        group_by(date) %>%
        # Make weights sum to 1:
        mutate(across(starts_with("J"), ~./sum(., na.rm=T))) %>%
        summarise(Ret = sum( J203 * Return, na.rm=T)) %>%
        mutate(CP = cumprod(1+Ret)) %>%
        mutate(ALSI = CP) %>%
        select(date, ALSI)

#   REITs

REIT_returns_performance <- data_ALSI_returns %>%
        filter( Sector %in% "Property") %>%
        group_by(date) %>%
        # Make weights sum to 1:
        mutate(across(starts_with("J"), ~./sum(., na.rm=T))) %>%
        summarise(Ret = sum( J203 * Return, na.rm=T))  %>%
        mutate(CP = cumprod(1+Ret)) %>%
        mutate(REIT = CP) %>%
        select(date, REIT)


df_cum_returns_ALSI_REIT_data <- left_join(ALSI_returns_performance,
                                           REIT_returns_performance,

                                               by = "date") %>%

                                                gather(Name, Value, -date)


    #-------------------------------------------------------------------------------

    graph <-  ggplot(df_cum_returns_ALSI_REIT_data) +
        geom_line(aes(x = date, y = Value, color = Name), alpha = 0.8,
                  size = 0.5) +

        scale_color_manual(values=c('black', 'red')) +

        #facet_wrap(~Name, scales = "free_y", nrow = 3) +

        #geom_bar(aes(x = reorder(location, value), y = value, colour = label, fill = label), stat = "identity") +
        #geom_text(aes(x = continent, y = value, label = value), vjust = 0) +

        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             x = xlabel,
             y = ylabel) +

        theme() +
        theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
        theme(legend.position="bottom") +
        theme(legend.title = element_blank())


    graph

}


