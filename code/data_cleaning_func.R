
# wrap in a function to neaten it up
data_cleaning_func <- function(df_data){



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
            group_by(date) %>% # group_by date so summation will be calculated for each Tciker for the same date
            mutate(Returns = Return/sum(Return)) %>%
            distinct(., date, .keep_all = TRUE) %>% # to select a unique date row
            mutate(Tickers = "ALSI") %>%
            select(date, Tickers, Returns)

    ) %>% mutate(Return = # scale the log scaled daily returns
                     Returns - mean(Returns, na.rm = F)) %>%

        select(date, Tickers, Return) %>%
        arrange(date) %>%

        filter(date >= as.Date("2011-01-01") & date <= as.Date("2022-10-31"))


    xts_data_combined <-
        combined_data_ALSI_REIT %>%
        tbl2xts::tbl_xts(., cols_to_xts = "Return", spread_by = "Tickers")



    xts_data_combined

}

