
mv_garch_REIT_nested_function <- function(df_data) {


    ################################################################################

    # wrap in a function to neaten it up
    mv_garch_func <- function(df_data){


        #-------------------------------------------------------------------------------
        # wrap in a function to neaten it up
        data_cleaning_func <- function(df_data){



            garch_data_ALSI_REIT <- imputed_REIT_returns_spread %>%
                    gather(Tickers, Returns, -date) %>%
                    arrange(date) %>%
                    mutate(Return = # scale the log scaled daily returns
                             Returns - mean(Returns, na.rm = F)) %>%

                select(date, Tickers, Return) %>%
                mutate(Return = Return) %>%
                arrange(date) %>%

                filter(date >= as.Date("2011-01-01") & date <= as.Date("2022-10-31"))


            xts_data_combined <-
                garch_data_ALSI_REIT %>%
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

        labs(title = "Volatility of Returns for the past decade",
             subtitle = "Different Asset Classes",
             caption = "Commodities, Equities, Real Estate and Bonds",
             x = "",
             y = "Sigma") +

        fmxdat::theme_fmx(legend.pos = "bottom")


}

mv_garch_REIT_nested_function(df_data)


