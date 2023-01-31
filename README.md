# Setting up

I begin my project by cleaning my environment and loading the necessary
functions to be used in order to

I make use of the ‘Texevier’ package to create the project

    Texevier::create_template(
        directory = "C:/Masters Economics/Fin Metrics/Fin_Metrics_Project",
                template_name = "Fin_Metrics_Project", build_project = TRUE, open_project = FALSE)

I then load the packages to used in this analysis. I make use of the
‘tidyverse’ to clean and wrangle the data as well as perform
visualization.

    # load pacakges to be used in the analysis
    pacman::p_load("tidyverse", "devtools", "rugarch", "rmgarch", 
        "forecast", "tbl2xts", "lubridate", "PerformanceAnalytics", 
        "ggthemes", "ks", "MTS", "robustbase", "tbl2xts")

# Import the data

I read in the Alsi\_Returns data and remove the words ‘SJ’ and ‘Equity’
from Tickers column to neaten up the data. Next, I view that data noting
the characteristics of the data such a date range and the various
columns of information.

I do notice that there are Tickers with missing values and some that
have not been included in the ALSI as they have zero weights.

I log the data before performing imputing the missing values in the data
set as I anticipate that it will result in NA/NaN/-Inf. The
‘impute\_missing\_values’ function can address Nas and Nan, so I will
set Returns with -Inf to zero.

Next, I source in all the functions to be used to conduct the analysis.

    set.seed(123)
    # read in the data
    data_ALSI_returns <- read_rds("data/Alsi_Returns.rds")
    # Remove the 'SJ' and 'Equity' from Tickers
    data_ALSI_returns$Tickers <- gsub("SJ|Equity", "", data_ALSI_returns$Tickers)
    # there are many NAs/NaNs that could pose a problem when using the mGARCH model
    # therefore I log the Returns before imputing missing values i.e. NA/NaN
    data_alsi <- data_ALSI_returns %>% 
        mutate(Weighted_Returns = Return * J433) %>% 
        select(date, Tickers, Weighted_Returns, Sector, J433) %>% 
        mutate(Return = log(Weighted_Returns)) %>% 
        arrange(date, Tickers) 
        #filter(date >= as.Date("2012-01-01") & date <= as.Date("2022-10-31"))

    # always view the data before starting with the analysis
    # replace '-Inf' with NA, so that a value is imputed when the 'impute_missig_value' fucntion is run
    data_alsi[data_alsi == -Inf] <- NA


    # source in fuctions
    list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))

## Data Insights

I make use of the dplyr package to determine how many unique sectors are
in the data set and to determine how many unique property stocks or
REITS are included in the data set and if they change over time, by
arbitrarily entering dates within the data’s range.

If find that from 2005 to 2022, the number of REITS changes over time.
My next step is therefore to plot the data where each property ticker is
plotted in it’s own panel. To do this I make use of ‘facet\_wrap’ in the
package ‘ggplot2’

    # determine which sectors are included in the data set
    data_alsi %>% 
        select(Sector) %>%
        unique()

    # input any date with in the data range to determine how many unique property stocks there are and if this changes over time
    data_alsi %>% filter(date == "2013-02-03") %>%
        filter(Sector == "Property") %>%
        arrange(date, Tickers) %>% 
        group_by(Tickers) %>%
        select(date, Tickers) %>% 
        unique()

    # count the number of NAs or missing values in the data
    data_alsi %>% 
        select(date, Tickers, Return, J433) %>% 
        group_by(Tickers) %>%
        select(date, Tickers) %>% 
        unique()
        
    # some wrangling to determine if the J433 sums to 1 for an arbitrary date
    # can set date to any week date
    # there for can make use of na.omit or set NAs to zero for the weights column 'J433'
    # Which is tested below
    data_alsi %>% filter(date == "2012-02-03") %>% 
        select(date, Tickers, Return, J433, Sector) %>% 
        na.omit(J433) %>% 
        mutate(sum(J433)) 

I make use of the tidyverse package in to wrangle the data into a usable
format to conduct the analysis. The task here is to remove the property
stocks from the Alsi\_Returns data, so I can calculate the daily returns
for the ALSI less REITs. Once I have the daily weighted performance of
the AlSI less REITS I will combine these daily returns with the daily
returns data of the REITs. This is done so a comparison of individual
property stocks can be drawn with the ALSI equities.

Missing values poses a small problem to deal with, however, it won’t be
as simple as imputing all the values for the complete ALSI. The reason
being is that there appears to be many missing values, some
Tickers/equities did not exist on the JSE at the point of measurement or
were not included in the ALSI, to impute values for all equities in the
index, normal and REITs, would give the REIT observations properties of
the other equities and vice versa and, lastly, across time the number of
REITs included in the index changes and there are many instances of
REITs moving in and out of the index.

One cannot impute missing values for the entire data set together,
especially given that the purpose of this study is to examine the
correlation of property stocks with rest of the ALSI. The approach I am
taking is to separate the property stocks from the rest of the ALSI as
discussed and then plot out the all the property stocks across time in
the data set and determine which have sufficiently complete returns
observations that I may impute accurate values that have a similar
distributions, so to capture the properties of these property stocks
(given that the theory surrounding REITs is that should be uncorrelated
to other equities. Once, I have determined which REITS to include I will
impute the missing for rest of the ALSI index and then combine the data
to begin the DCC or mGARCH model.

# Seperate and plot the REITs data

    library(tidyverse)

    # add individual property stock and their weighted returns to perform DCC
    property_returns <- data_ALSI_returns %>%
            filter(Sector == "Property") %>%
            select(date, Tickers, Return, J433, Sector) %>%
            na.omit(J433) %>% # remove observations that don't have weights i.e. NA
            arrange(date, Tickers) %>%
            select(date, Tickers, Return)


    graph_1 <- graph_1_reit_funcs(df_data = property_returns,
                    title = "JSE listed REITs over time ",
                    subtitle = "",
                    caption = "Note how many REITs have complete data sets",
                    xlabel = "",
                    ylabel = "")

    graph_1

![](README_files/figure-markdown_strict/unnamed-chunk-5-1.png)

From the graph above one can see that can see that there are only a few
listed REITs that have a sufficient number of observations to offer
insight into the correlation between between REITs and the rest of the
ALSI.

I then proceed to select the following Tickers to include in the study.
REITs to include: CCO, EMI, GRT, HYP, RDF, RES, SAC.

Next, I filter the ALSI Property data for the REITs listed above and
impute the missing values in this filtered data.

# Prepare data for DCC Model

## Impute missing values: Property equities

    #
    data_alsi_REIT <- data_alsi %>%
            filter(Sector == "Property") %>%
            select(date, Tickers, Return) %>%
            spread(Tickers, Return)

    # select the columns that correspond to the following REITs equities.
    # REITs to include: CCO, EMI, GRT, HYP, RDF, RES, SAC
    data_alsi_REIT_red<- data_alsi_REIT[, c(1,14,19,26,30,45,47,52)]
    # Remember to include the date column
    # the dplyr func 'filter' nor the 'select' (once spread) did not work
    # the therfore diverted to base R to filter for the REITs that had data for decade

    # make tidy format once again
    data_alsi_REIT_reduced <- data_alsi_REIT_red %>% 
                                gather(Tickers, Return, -date)

    # Now impute missing values using the 'impute_missing_values' function
    imputed_REIT_returns_spread <- impute_missing_returns(
        
        return_mat = data_alsi_REIT_reduced %>%
            select(date, Tickers, Return) %>%
            spread(Tickers, Return),
            
        impute_returns_method = "Drawn_Distribution_Own")

## Impute missing values: ALSI less REITs

    # create a data set where property stocks have been removed from the Alsi_Returns data and re-weighted.
    # Now impute missing values using the 'impute_missing_values' function

    imputed_ALSI_returns_spread <- impute_missing_returns(
        
        return_mat = data_alsi %>% 
            filter(Sector != "Property") %>%
            select(date, Tickers, Return) %>% 
            #na.omit(J433) %>% # remove observations that don't have weights i.e. NA
            arrange(date) %>% 
            #mutate(weights = J433/sum(J433)) %>%
            #mutate(weighted_return = Return * weights) %>% # re-weight ALSI less property stocks
            #group_by(date) %>% 
            #mutate(Return = sum(weighted_return)) %>% 
            #distinct(., date, .keep_all = T) %>% 
            #mutate(Tickers = "ALSI") %>% 
            select(date, Tickers, Return) %>% 
            spread(Tickers, Return),
            
        impute_returns_method = "Drawn_Distribution_Collective")

# Combine the data

I now use the imputed data for the ALSI less REITs and make it tidy by
placing the data into 3 columns (date, Tickers, Return). Next wrangle
the data to calculate average daily returns of the ALSI less REITs.

I now want to bind the rows of the weighted returns of the ALSI with the
individual property stocks that had previously been removed separated to
impute the missing values. This will allow for correlation comparisons
with individual property stocks with the broader performance with the
ALSI equities for the other sectors.

The data wrangling described above is nested in the function
‘mv\_garch\_COMBINED\_nested\_function(df\_data)’ for the estimated
volatility.

# DCC Model multivariate GARCH model (Time varying correlation)

I follow the practical code closely to render the model. I amend code
and nested functions inside one another to keep the working document
neat. I plot the estimates of volatility for each seriesfrom ‘dccPre’.

    # use dccPre to fit the univariate GARCH models to each series in the data frame of returns.
    # Let's select a VAR order of zero for the mean equation, and use the mean of each series.

    # Then, for every series, a standard univariate GARCH(1,1) is run - giving us:
    # et and sigmat, which is then used to calculate the standardized resids, zt.
    # zt is used in DCC calcs after.

    # SEE: q6_nested_graph_function.R (NESTED FUNC)
    mv_garch_COMBINED_nested_function(df_data)

    ## Sample mean of the returns:  -0.4312428 -0.79778 0.7520471 -0.175957 0.8383758 0.04329736 -0.1133111 0.2221172 
    ## Component:  1 
    ## Estimates:  0.240096 0 0.826149 
    ## se.coef  :  0.617144 0.013065 0.45594 
    ## t-value  :  0.389044 1e-06 1.811967 
    ## Component:  2 
    ## Estimates:  0.189246 0 0.844127 
    ## se.coef  :  0.325868 0.010879 0.265836 
    ## t-value  :  0.580744 1e-06 3.17537 
    ## Component:  3 
    ## Estimates:  0.145547 0.000697 0.911828 
    ## se.coef  :  0.131556 0.005766 0.079191 
    ## t-value  :  1.106354 0.120852 11.51429 
    ## Component:  4 
    ## Estimates:  2e-06 0.000198 0.999771 
    ## se.coef  :  6.4e-05 8e-05 2.6e-05 
    ## t-value  :  0.025768 2.471748 38977.15 
    ## Component:  5 
    ## Estimates:  0.024921 0.007235 0.971692 
    ## se.coef  :  0.00897 0.0037 0.008028 
    ## t-value  :  2.778074 1.955501 121.032 
    ## Component:  6 
    ## Estimates:  0.072703 0.006828 0.950942 
    ## se.coef  :  0.039857 0.005092 0.024612 
    ## t-value  :  1.8241 1.340925 38.63775 
    ## Component:  7 
    ## Estimates:  0.003737 0.005978 0.989023 
    ## se.coef  :  0.001582 0.002091 0.001307 
    ## t-value  :  2.361871 2.859159 756.8043 
    ## Component:  8 
    ## Estimates:  0.000145 0.011366 0.971289 
    ## se.coef  :  5.6e-05 0.00362 0.007873 
    ## t-value  :  2.574266 3.139594 123.3657

![](README_files/figure-markdown_strict/unnamed-chunk-8-1.png)

Additionally, I plot the volatility of only the seven REIT equities
included in this study.

    # volatility of REITs
    mv_garch_REIT_nested_function(df_data)

    ## Sample mean of the returns:  -0.3972556 -0.7637927 0.7860344 -0.1419697 0.8723631 0.07728465 -0.0793238 
    ## Component:  1 
    ## Estimates:  0.240096 0 0.826149 
    ## se.coef  :  0.617144 0.013065 0.45594 
    ## t-value  :  0.389044 1e-06 1.811967 
    ## Component:  2 
    ## Estimates:  0.189246 0 0.844127 
    ## se.coef  :  0.325868 0.010879 0.265836 
    ## t-value  :  0.580744 1e-06 3.17537 
    ## Component:  3 
    ## Estimates:  0.145547 0.000697 0.911828 
    ## se.coef  :  0.131556 0.005766 0.079191 
    ## t-value  :  1.106354 0.120852 11.51429 
    ## Component:  4 
    ## Estimates:  2e-06 0.000198 0.999771 
    ## se.coef  :  6.4e-05 8e-05 2.6e-05 
    ## t-value  :  0.025768 2.471748 38977.15 
    ## Component:  5 
    ## Estimates:  0.024921 0.007235 0.971692 
    ## se.coef  :  0.00897 0.0037 0.008028 
    ## t-value  :  2.778074 1.955501 121.032 
    ## Component:  6 
    ## Estimates:  0.072703 0.006828 0.950942 
    ## se.coef  :  0.039857 0.005092 0.024612 
    ## t-value  :  1.8241 1.340925 38.63775 
    ## Component:  7 
    ## Estimates:  0.003737 0.005978 0.989023 
    ## se.coef  :  0.001582 0.002091 0.001307 
    ## t-value  :  2.361871 2.859159 756.8043

![](README_files/figure-markdown_strict/unnamed-chunk-9-1.png)

The ‘dccPre’ function is use to fit the univariate GARCH models to each
series in the data and a standard univariate GARCH(1,1) is run which
produces the error term and sigma, which is then used to calculate the
standardized residuals used in estimate the DCC model.

The DCC model is then run and the estimates of time-varying correlation
are produced.

# CORR GRAPHS: Co-movements

The ‘dccPre’ function is use to fit the univariate GARCH models to each
series in the data and a standard univariate GARCH(1,1) is run which
produces the error term and sigma, which is then used to calculate the
standardized residuals used in estimate the DCC model.

    # Use the cleaning func to warngle data and get into 'xts' format
    xts_ALSI_data_combined_use <- data_cleaning_func(df_data)

The DCC model is then run and the estimates of time-varying correlation
are produced.

    DCCPre <- dccPre(xts_ALSI_data_combined_use, include.mean = F, p = 0)

    ## Component:  1 
    ## Estimates:  0.236014 0 0.849869 
    ## se.coef  :  0.395437 0.007554 0.248727 
    ## t-value  :  0.596843 1e-06 3.416881 
    ## Component:  2 
    ## Estimates:  0.035274 0.01161 0.968725 
    ## se.coef  :  0.014439 0.003705 0.009526 
    ## t-value  :  2.443007 3.133659 101.6892 
    ## Component:  3 
    ## Estimates:  0.074196 0.024807 0.94064 
    ## se.coef  :  0.034824 0.007322 0.020854 
    ## t-value  :  2.130565 3.38821 45.10598 
    ## Component:  4 
    ## Estimates:  0.001448 0.001955 0.997166 
    ## se.coef  :  0.000275 0.000165 8.5e-05 
    ## t-value  :  5.271418 11.84244 11712.41 
    ## Component:  5 
    ## Estimates:  0.017266 0.01235 0.978371 
    ## se.coef  :  0.007637 0.003159 0.004828 
    ## t-value  :  2.260892 3.909929 202.6537 
    ## Component:  6 
    ## Estimates:  0.053818 0.008453 0.960486 
    ## se.coef  :  0.022248 0.004121 0.014129 
    ## t-value  :  2.41895 2.051282 67.98083 
    ## Component:  7 
    ## Estimates:  0.003344 0.006854 0.988678 
    ## se.coef  :  0.001283 0.001715 0.001226 
    ## t-value  :  2.605311 3.996903 806.4823 
    ## Component:  8 
    ## Estimates:  0.000271 0.033203 0.962433 
    ## se.coef  :  0.000209 0.007467 0.008732 
    ## t-value  :  1.296008 4.446729 110.2252

    # After saving now the standardized residuals:
    StdRes <- DCCPre$sresi
    # We can now use these sresids to calculate the DCC model.
    # In order to fit the DCC model detach the tidyr and dplyr packages, 
    # once detached can now run dccFit
    # when done then tidyr and dplyr 
    detach("package:tidyverse", unload=TRUE)
    detach("package:tbl2xts", unload=TRUE)
    DCC <- dccFit(StdRes, type="Engle")

    ## Estimates:  0.95 0.03267332 20 
    ## st.errors:  NaN NaN 0.9758897 
    ## t-values:   NaN NaN 20.49412

    pacman::p_load("tidyverse", "tbl2xts", "broom")

    graph_rename_func_mv(input_name_1 = "ALSI_",
                         input_name_2 = "_ALSI",
                         title = "Dynamic Conditional Correlations: ALSI",
                         subtitle = "",
                         caption = "",
                         xlabel = "",
                         ylabel = "Rho")

![](README_files/figure-markdown_strict/unnamed-chunk-12-1.png)

    graph_rename_func_mv(input_name_1 = "ALSI_CCO",
                         input_name_2 = "_ALSI",
                         title = "Dynamic Conditional Correlations: ALSI and CCO",
                         subtitle = "2008 to 2022",
                         caption = "",
                         xlabel = "",
                         ylabel = "Correlation")

    graph_rename_func_mv(input_name_1 = "ALSI_EMI",
                         input_name_2 = "_ALSI",
                         title = "Dynamic Conditional Correlations: ALSI and EMI",
                         subtitle = "2008 to 2022",
                         caption = "",
                         xlabel = "",
                         ylabel = "Correlation")

    graph_rename_func_mv(input_name_1 = "ALSI_GRT",
                         input_name_2 = "_ALSI",
                         title = "Dynamic Conditional Correlations: ALSI and GRT",
                         subtitle = "2008 to 2022",
                         caption = "",
                         xlabel = "",
                         ylabel = "Correlation")

    graph_rename_func_mv(input_name_1 = "ALSI_HYP",
                         input_name_2 = "_ALSI",
                         title = "Dynamic Conditional Correlations: ALSI and HYP",
                         subtitle = "2008 to 2022",
                         caption = "",
                         xlabel = "",
                         ylabel = "Correlation")

    graph_rename_func_mv(input_name_1 = "ALSI_RES",
                         input_name_2 = "_ALSI",
                         title = "Dynamic Conditional Correlations: ALSI and RES",
                         subtitle = "2008 to 2022",
                         caption = "",
                         xlabel = "",
                         ylabel = "Correlation")

    graph_rename_func_mv(input_name_1 = "ALSI_RDF",
                         input_name_2 = "_ALSI",
                         title = "Dynamic Conditional Correlations: ALSI and RDF",
                         subtitle = "2008 to 2022",
                         caption = "",
                         xlabel = "",
                         ylabel = "Correlation")

    graph_rename_func_mv(input_name_1 = "ALSI_SAC",
                         input_name_2 = "_ALSI",
                         title = "Dynamic Conditional Correlations: ALSI and SAC",
                         subtitle = "2008 to 2022",
                         caption = "",
                         xlabel = "",
                         ylabel = "Correlation")

<img src="README_files/figure-markdown_strict/figures-side-1.png" width="50%" /><img src="README_files/figure-markdown_strict/figures-side-2.png" width="50%" /><img src="README_files/figure-markdown_strict/figures-side-3.png" width="50%" /><img src="README_files/figure-markdown_strict/figures-side-4.png" width="50%" /><img src="README_files/figure-markdown_strict/figures-side-5.png" width="50%" /><img src="README_files/figure-markdown_strict/figures-side-6.png" width="50%" /><img src="README_files/figure-markdown_strict/figures-side-7.png" width="50%" />

    graph_rename_func_mv(input_name_1 = "CCO.._",
                         input_name_2 = "_CCO..",
                         title = "Dynamic Conditional Correlations: Capco and SA REITs",
                         subtitle = "",
                         caption = "",
                         xlabel = "",
                         ylabel = "Rho")

![](README_files/figure-markdown_strict/unnamed-chunk-13-1.png)

# Periods of Rand Volatility

In this section, periods of high USD/ZAR (Dollar Rand) volatility are
isolated and used as to filter for the ALSI combined and imputed data.
The premise being that periods of high Rand volatility can act as an
indicator for high levels of volatility in South Africa financial
markets and other asset classes. These highly volatile periods are then
used as an index to filter the returns data for periods where South
African markets were volatile.

Given that the high volatility combine imputed ALSI returns data will
have large missing gaps due to periods of moderate or low volatility,
dynamic correlations between equity pairs will have to be charted for
short periods of a time. This is due to the fact that the graphing
function used will not skip whole year periods.

Following this methodology of running multiple DCC models on smaller
periods of high volatility decreases the run time of the model.

    hi_vol_graph_rename_func_mv(input_name_1 = "ALSI_",
                                        input_name_2 = "_ALSI",
                                        title = "Dynamic Conditional Correlations: ALSI",
                                        subtitle = "2008 to 2009",
                                        caption = "",
                                        xlabel = "",
                                        ylabel = "Rho")

![](README_files/figure-markdown_strict/unnamed-chunk-15-1.png)

    hi_vol_graph_rename_func_mv(input_name_1 = "ALSI_",
                                        input_name_2 = "_ALSI",
                                        title = "Dynamic Conditional Correlations: ALSI",
                                        subtitle = "2011",
                                        caption = "",
                                        xlabel = "",
                                        ylabel = "Rho")

![](README_files/figure-markdown_strict/unnamed-chunk-17-1.png)

    hi_vol_graph_rename_func_mv(input_name_1 = "ALSI_",
                                        input_name_2 = "_ALSI",
                                        title = "Dynamic Conditional Correlations: ALSI",
                                        subtitle = "2016",
                                        caption = "",
                                        xlabel = "",
                                        ylabel = "Rho")

![](README_files/figure-markdown_strict/unnamed-chunk-19-1.png)

    hi_vol_graph_rename_func_mv(input_name_1 = "ALSI_",
                                        input_name_2 = "_ALSI",
                                        title = "Dynamic Conditional Correlations: ALSI",
                                        subtitle = "2020",
                                        caption = "",
                                        xlabel = "",
                                        ylabel = "Rho")

![](README_files/figure-markdown_strict/unnamed-chunk-21-1.png)
