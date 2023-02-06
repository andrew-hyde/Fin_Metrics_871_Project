

sector_graph_cum_return_func <- function(df_data, title, subtitle, caption, xlabel, ylabel){

    library(tidyverse)

    ################################################################################



    REIT_returns_performance <- data_ALSI_returns %>%
        filter( Sector %in% "Property") %>%
        group_by(date) %>%
        # Make weights sum to 1:
        mutate(across(starts_with("J"), ~./sum(., na.rm=T))) %>%
        summarise(Ret = sum( J203 * Return, na.rm=T))  %>%
        # mutate( CW = Market.Cap / sum(Market.Cap, na.rm=T)) %>%
        # summarise(Ret = sum( CW * Return, na.rm=T)) %>%
        mutate(CP = cumprod(1+Ret)) %>%
        mutate(Property = CP) %>%
        select(date, Property)


    Industrials_returns_performance <- data_ALSI_returns %>%
        filter( Sector %in% "Industrials") %>%
        group_by(date) %>%
        # Make weights sum to 1:
        mutate(across(starts_with("J"), ~./sum(., na.rm=T))) %>%
        summarise(Ret = sum( J203 * Return, na.rm=T))  %>%
        # mutate( CW = Market.Cap / sum(Market.Cap, na.rm=T)) %>%
        # summarise(Ret = sum( CW * Return, na.rm=T)) %>%
        mutate(CP = cumprod(1+Ret)) %>%
        mutate(Industrials = CP) %>%
        select(date, Industrials)

    Financials_returns_performance <- data_ALSI_returns %>%
        filter( Sector %in% "Financials") %>%
        group_by(date) %>%
        # Make weights sum to 1:
        mutate(across(starts_with("J"), ~./sum(., na.rm=T))) %>%
        summarise(Ret = sum( J203 * Return, na.rm=T))  %>%
        # mutate( CW = Market.Cap / sum(Market.Cap, na.rm=T)) %>%
        # summarise(Ret = sum( CW * Return, na.rm=T)) %>%
        mutate(CP = cumprod(1+Ret)) %>%
        mutate(Financials = CP) %>%
        select(date, Financials)

    Resources_returns_performance <- data_ALSI_returns %>%
        filter( Sector %in% "Resources") %>%
        group_by(date) %>%
        # Make weights sum to 1:
        mutate(across(starts_with("J"), ~./sum(., na.rm=T))) %>%
        summarise(Ret = sum( J203 * Return, na.rm=T))  %>%
        # mutate( CW = Market.Cap / sum(Market.Cap, na.rm=T)) %>%
        # summarise(Ret = sum( CW * Return, na.rm=T)) %>%
        mutate(CP = cumprod(1+Ret)) %>%
        mutate(Resources = CP) %>%
        select(date, Resources)

    # COMBINE
    df_cum_returns_ALSI_REIT_data <- left_join(Resources_returns_performance,
                                           REIT_returns_performance,

                                           by = "date") %>%

        left_join(., Financials_returns_performance, by = "date") %>%

        left_join(., Industrials_returns_performance, by = "date") %>%

        gather(Tickers, Return, -date) %>%
        arrange(date)

    #-------------------------------------------------------------------------------

    graph <-  ggplot(df_cum_returns_ALSI_REIT_data) +
        geom_line(aes(x = date, y = Return, color = Tickers), alpha = 0.8,
                  size = 0.5) +

        scale_color_manual(values=c('orange', 'red', 'blue', 'green')) +

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


