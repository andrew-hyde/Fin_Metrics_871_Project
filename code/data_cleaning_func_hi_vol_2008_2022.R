
# wrap in a function to neaten it up
data_cleaning_func_hi_vol_2008_2022 <- function(df_data){

#-------------------------------------------------------------------------------

    zar <-  read_rds("data/usdzar.rds") %>%
        mutate(Return = Price/lag(Price)-1)%>%
        filter(date >= as.Date("2007-01-01") & date <= as.Date("2022-12-31")) %>%
        filter(Name == "SouthAfrica_Cncy") %>%
        select(-Name)


    ZARSD <- zar %>%
        mutate(YearMonth = format(date, "%Y%B")) %>%
        group_by(YearMonth) %>% summarise(SD = sd(Return)*sqrt(252)) %>%
        # Top Decile Quantile overall (highly volatile month for ZAR:
        mutate(TopQtile = quantile(SD, 0.8),
               BotQtile = quantile(SD, 0.2))


    Hi_Vol <- ZARSD %>% filter(SD > TopQtile) %>% pull(YearMonth)
    Low_Vol <- ZARSD %>% filter(SD < BotQtile) %>% pull(YearMonth)

#-------------------------------------------------------------------------------

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
#-------------------------------------------------------------------------------
    # COMBINE
    df_returns_ALSI_REIT_data <- left_join(ALSI_returns_performance,
                                           REIT_returns_performance,

                                           by = "date") %>%

        gather(Tickers, Return, -date) %>%
        arrange(date) %>%
        #filter(date >= as.Date("2007-01-01") & date <= as.Date("2022-12-31")) %>%
          mutate(YearMonth = format(date, "%Y%B")) #%>% # create year months column to filter against




hi_vol_data <- df_returns_ALSI_REIT_data %>% filter(YearMonth %in% Hi_Vol) %>% # filter for months of high volatility
            select(date, Return, Tickers)

    xts_data_combined <-
        hi_vol_data %>%
        tbl2xts::tbl_xts(., cols_to_xts = "Return", spread_by = "Tickers")



    xts_data_combined

}




