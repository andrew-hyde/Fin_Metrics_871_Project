


# wrap in a function to neaten it up
data_cleaning_func_hi_vol_2008_2009 <- function(df_data){

#-------------------------------------------------------------------------------

    zar <-  read_rds("data/usdzar.rds") %>%
        mutate(Return = Price/lag(Price)-1)%>%
        filter(date >= as.Date("2008-01-01") & date <= as.Date("2010-12-31")) %>%
        filter(Name == "SouthAfrica_Cncy") %>%
        select(-Name)


    ZARSD <- zar %>%
        mutate(YearMonth = format(date, "%Y%B")) %>%
        group_by(YearMonth) %>% summarise(SD = sd(Return)*sqrt(252)) %>%
        # Top Decile Quantile overall (highly volatile month for ZAR:
        mutate(TopQtile = quantile(SD, 0.9),
               BotQtile = quantile(SD, 0.1))


    Hi_Vol <- ZARSD %>% filter(SD > TopQtile) %>% pull(YearMonth)
    #Low_Vol <- ZARSD %>% filter(SD < BotQtile) %>% pull(YearMonth)

#-------------------------------------------------------------------------------

    combined_data_ALSI_REIT <- bind_rows(
        # REITs
        imputed_REIT_returns_spread %>%
            gather(Tickers, Returns, -date) %>%
            arrange(date) %>%
            select(date, Tickers, Returns), # need the right order

        # ALSI less REITs
        imputed_ALSI_returns_spread %>%
            gather(Tickers, Return, -date) %>%
            arrange(date) %>%
            group_by(date) %>% # group_by date so summation will be calculated for each Ticker for the same date
            mutate(Returns = mean(Return)) %>%
            distinct(., date, .keep_all = TRUE) %>% # to select a unique date row
            mutate(Tickers = "ALSI") %>%
            select(date, Tickers, Returns)

    ) %>% mutate(Return = # scale the log scaled daily returns
                     Returns - mean(Returns, na.rm = F)) %>%

        select(date, Tickers, Return) %>%
        arrange(date) %>%
        filter(date >= as.Date("2008-01-01") & date <= as.Date("2010-12-31")) %>%
          mutate(YearMonth = format(date, "%Y%B")) #%>% # create year months column to filter against


hi_vol_data <- combined_data_ALSI_REIT %>% filter(YearMonth %in% Hi_Vol) %>% # filter for months of high volatility
            select(date, Return, Tickers)

    xts_data_combined <-
        hi_vol_data %>%
        tbl2xts::tbl_xts(., cols_to_xts = "Return", spread_by = "Tickers")



    xts_data_combined

}




