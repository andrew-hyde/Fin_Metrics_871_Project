
# wrap in a function to neaten it up
data_cleaning_func_indi_REIT <- function(df_data){


# COMBINE

    # ALSI excl. REITs
    ALSI_returns_performance <- data_ALSI_returns %>%
            filter( !Sector %in% "Property") %>%
            group_by(date) %>%
        # Make weights sum to 1:
        mutate(across(starts_with("J"), ~./sum(., na.rm=T))) %>%
        summarise(ALSI = sum( J203 * Return, na.rm=T)) %>%
        #mutate(ALSI = mean( J203 * Return, na.rm=T)) %>%
            # mutate( CW = Market.Cap / sum(Market.Cap, na.rm=T)) %>%
            # summarise(Ret = sum( CW * Return, na.rm=T)) %>%
            #mutate(ALSI = Ret) %>%
            #(Tickers ="ALSI") %>%
            select(date, ALSI)

# REITs_Selected <- list("CCO"," EMI", "GRT", "HPB","HYP","IPF","OCT", "RDF", "RES", "SAC", "VKE")

    # REITs
    REIT_returns_performance <- data_ALSI_returns %>%
            filter( Sector %in% "Property") %>%
            group_by(date) %>%
            #na.omit(J203) %>%
            # mutate( CW = Market.Cap / sum(Market.Cap, na.rm=T)) %>%
            # mutate(Return = CW* Return)  %>%
            # Make weights sum to 1:
            na.omit(J203) %>%
            mutate(across(starts_with("J"), ~./sum(., na.rm=T))) %>%
            mutate(Return = J203 * Return)  %>%
            # mutate( CW = Market.Cap / sum(Market.Cap, na.rm=T)) %>%
            # summarise(Ret = sum( CW * Return, na.rm=T)) %>%
            #mutate(REIT = Ret) %>%
            select(date, Tickers, Return) %>%
                spread(Tickers, Return)


            # select the columns that correspond to the following REITs equities.
            # REITs to include: CCO, EMI, GRT, HYP, RDF, RES, SAC
            data_alsi_REIT_reduced <- REIT_returns_performance[, c(1,25,29,42,44,48,55)]
            #data_alsi_REIT_reduced <- REIT_returns_performance[, c(1,18,25,42,55)]
            #data_alsi_REIT_reduced <- REIT_returns_performance[, c(1,13,18,25,29,42,44,48,55)]

            #data_alsi_REIT_red <- data_alsi_REIT_reduced %>%
                #gather(Tickers, Return, -date)

#-------------------------------------------------------------------------------

 # COMBINE BOTH DATA SETS
    df_returns_ALSI_REIT_data <- left_join(ALSI_returns_performance,
                                           data_alsi_REIT_reduced,

                                           by = "date") %>%

                gather(Tickers, Return, -date) %>%
                arrange(date) %>%
                na.omit(Return) %>%
            filter(date >= as.Date("2013-01-01") & date <= as.Date("2022-10-31"))

xts_data_combined <- df_returns_ALSI_REIT_data %>%
            tbl2xts::tbl_xts(., cols_to_xts = "Return", spread_by = "Tickers")



xts_data_combined

}

