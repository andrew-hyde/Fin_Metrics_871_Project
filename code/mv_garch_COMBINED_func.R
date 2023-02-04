
mv_garch_COMBINED_nested_function <- function(df_data) {


################################################################################

# use 'Vol' in the volatility plot
ggplot(Vol_ALSI_REIT %>% gather(Tickers, Sigma, -date)) +
    geom_line(aes(x = date, y = Sigma, colour = Tickers)) +

    labs(title = "Volatility of Returns for ALSI and REITs",
         subtitle = "From 2005 to 2022",
         caption = "",
         x = "",
         y = "Sigma") +

    fmxdat::theme_fmx(legend.pos = "bottom")


}



