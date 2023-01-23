
mv_garch_COMBINED_nested_function <- function(df_data) {


################################################################################

# wrap in a function to neaten it up
mv_garch_func <- function(df_data){


#-------------------------------------------------------------------------------
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

# result
xts_data_combined_use <- data_cleaning_func(df_data)

#--------------------------------------------------------------------------------


DCCPre <- dccPre(xts_data_combined_use, include.mean = T, p = 0)

    # estimates of volatility for each series.
    Vol <- DCCPre$marVol
    colnames(Vol) <- colnames(xts_data_combined_use)

    Vol <- data.frame(cbind(date = index(xts_data_combined_use), Vol)) %>%
        # Add date column
        mutate(date = as.Date(date)) %>%  tbl_df()


    # volatility
    Vol

}

# result
Vol <- mv_garch_func(df_data)

################################################################################

# use 'Vol' in the volatility plot
ggplot(Vol %>% gather(Tickers, Sigma, -date)) +
    geom_line(aes(x = date, y = Sigma, colour = Tickers)) +

    labs(title = "Volatility of Returns for ALSI and Individual REITs",
         subtitle = "From 2012 to 2022",
         caption = "",
         x = "",
         y = "Sigma") +

    fmxdat::theme_fmx(legend.pos = "bottom")


}



