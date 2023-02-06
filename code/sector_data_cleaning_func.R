
# wrap in a function to neaten it up
sector_data_cleaning_func <- function(df_data){

    #   REITs
    REIT_returns_performance <- data_ALSI_returns %>%
        filter( Sector %in% "Property") %>%
        group_by(date) %>%
        # Make weights sum to 1:
        mutate(across(starts_with("J"), ~./sum(., na.rm=T))) %>%
        summarise(Ret = sum( J203 * Return, na.rm=T))  %>%
        mutate(Property = Ret) %>%
        select(date, Property)

    #   Resources
    Resources_returns_performance <- data_ALSI_returns %>%
        filter( Sector %in% "Resources") %>%
        group_by(date) %>%
        # Make weights sum to 1:
        mutate(across(starts_with("J"), ~./sum(., na.rm=T))) %>%
        summarise(Ret = sum( J203 * Return, na.rm=T))  %>%
        mutate(Resources = Ret) %>%
        select(date, Resources)

    #   Financials
    Financials_returns_performance <- data_ALSI_returns %>%
        filter( Sector %in% "Financials") %>%
        group_by(date) %>%
        # Make weights sum to 1:
        mutate(across(starts_with("J"), ~./sum(., na.rm=T))) %>%
        summarise(Ret = sum( J203 * Return, na.rm=T))  %>%
        mutate(Financials = Ret) %>%
        select(date, Financials)

    #  Industrials
    Industrials_returns_performance <- data_ALSI_returns %>%
        filter( Sector %in% "Industrials") %>%
        group_by(date) %>%
        # Make weights sum to 1:
        mutate(across(starts_with("J"), ~./sum(., na.rm=T))) %>%
        summarise(Ret = sum( J203 * Return, na.rm=T))  %>%
        mutate(Industrials = Ret) %>%
        select(date, Industrials)


    # COMBINE
    df_returns_ALSI_REIT_data <- left_join(Resources_returns_performance,
                                           REIT_returns_performance,

                                           by = "date") %>%

                                left_join(., Financials_returns_performance, by = "date") %>%

                                left_join(., Industrials_returns_performance, by = "date") %>%

        gather(Tickers, Return, -date) %>%
        arrange(date)

    # convert to xts for graphing func
    xts_data_combined <- df_returns_ALSI_REIT_data %>%
        tbl2xts::tbl_xts(., cols_to_xts = "Return", spread_by = "Tickers")


    xts_data_combined

}


