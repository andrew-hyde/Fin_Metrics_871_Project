
# wrap in a function to neaten it up
compare_vol_mv_garch_func <- function(df_data){


#-------------------------------------------------------------------------------
# wrap in a function to neaten it up

compare_vol_data_cleaning_func <- function(df_data){


    # ALSI excl. REITs
    ALSI_returns_performance <- data_ALSI_returns %>%
        filter( !Sector %in% "Property") %>%
        group_by(date) %>%
        # Make weights sum to 1:
        mutate(across(starts_with("J"), ~./sum(., na.rm=T))) %>%
        summarise(Ret = sum( J203 * Return, na.rm=T)) %>%
        # mutate( CW = Market.Cap / sum(Market.Cap, na.rm=T)) %>%
        # summarise(Ret = sum( CW * Return, na.rm=T)) %>%
        mutate(ALSI = Ret) %>%
        select(date, ALSI)

    #   REITs
    REIT_returns_performance <- data_ALSI_returns %>%
        filter( Sector %in% "Property") %>%
        group_by(date) %>%
        # Make weights sum to 1:
        mutate(across(starts_with("J"), ~./sum(., na.rm=T))) %>%
        summarise(Ret = sum( J203 * Return, na.rm=T))  %>%
        # mutate( CW = Market.Cap / sum(Market.Cap, na.rm=T)) %>%
        # summarise(Ret = sum( CW * Return, na.rm=T)) %>%
        mutate(REIT = Ret) %>%
        select(date, REIT)

    # COMBINE
    df_returns_ALSI_REIT_data <- left_join(ALSI_returns_performance,
                                           REIT_returns_performance,

                                           by = "date") %>%

        gather(Tickers, Return, -date) %>%
        arrange(date)

    #filter(date >= as.Date("2005-01-01") & date <= as.Date("2022-12-31"))

    comapare_vol_xts_data_combined <- df_returns_ALSI_REIT_data %>%
        tbl2xts::tbl_xts(., cols_to_xts = "Return", spread_by = "Tickers")



    comapare_vol_xts_data_combined

}

# result
compare_vol_xts_data_combined_use <- compare_vol_data_cleaning_func(df_data)


#--------------------------------------------------------------------------------

DCCPre <- dccPre(compare_vol_xts_data_combined_use, include.mean = T, p = 0)

# estimates of volatility for each series.
Vol <- DCCPre$marVol
colnames(Vol) <- colnames(compare_vol_xts_data_combined_use)

Vol <- data.frame(cbind(date = index(compare_vol_xts_data_combined_use), Vol)) %>%
    # Add date column
    mutate(date = as.Date(date)) %>%  tbl_df()


# volatility
Vol

}

