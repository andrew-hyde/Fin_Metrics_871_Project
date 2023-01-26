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
        "ggthemes", "ks", "MTS", "robustbase")

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

# Impute missing values: Property equities

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

# graph the performance of the REITS

# Impute missing values: ALSI less REITs

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

    ## Sample mean of the returns:  -0.4359894 -0.816097 0.8009922 -0.1499985 0.8661694 0.02661527 -0.1251878 0.2229124 
    ## Component:  1 
    ## Estimates:  0.817525 0.036139 0.412812 
    ## se.coef  :  0.784452 0.02253 0.542006 
    ## t-value  :  1.04216 1.604057 0.761637 
    ## Component:  2 
    ## Estimates:  0.204256 0 0.832915 
    ## se.coef  :  NaN NaN NaN 
    ## t-value  :  NaN NaN NaN 
    ## Component:  3 
    ## Estimates:  0.243877 0 0.836649 
    ## se.coef  :  0.430106 0.008508 0.289586 
    ## t-value  :  0.567017 1e-06 2.889123 
    ## Component:  4 
    ## Estimates:  0.005396 0.004853 0.991698 
    ## se.coef  :  0.00188 0.001233 0.000745 
    ## t-value  :  2.870661 3.936343 1331.394 
    ## Component:  5 
    ## Estimates:  0.014443 0.006855 0.980982 
    ## se.coef  :  0.00439 0.002662 0.00355 
    ## t-value  :  3.289675 2.575504 276.3269 
    ## Component:  6 
    ## Estimates:  0.036717 0.014349 0.963893 
    ## se.coef  :  0.013937 0.004498 0.010079 
    ## t-value  :  2.634529 3.190058 95.63708 
    ## Component:  7 
    ## Estimates:  0.005254 0.006806 0.986183 
    ## se.coef  :  0.002062 0.002575 0.002053 
    ## t-value  :  2.548397 2.642899 480.3592 
    ## Component:  8 
    ## Estimates:  0.000312 0.029001 0.934104 
    ## se.coef  :  0.000139 0.008179 0.022127 
    ## t-value  :  2.24104 3.545985 42.21595

![](README_files/figure-markdown_strict/unnamed-chunk-9-1.png)

Additionally, I plot the volatility of only the seven REIT equities
included in this study.

    # volatility of REITs
    mv_garch_REIT_nested_function(df_data)

    ## Sample mean of the returns:  -0.4019666 -0.7820742 0.835015 -0.1159757 0.9001922 0.06063808 -0.09116497 
    ## Component:  1 
    ## Estimates:  0.817525 0.036139 0.412812 
    ## se.coef  :  0.784452 0.02253 0.542006 
    ## t-value  :  1.04216 1.604057 0.761637

    ## Warning in sqrt(diag(fit$cvar)): NaNs produced

    ## Component:  2 
    ## Estimates:  0.204256 0 0.832915 
    ## se.coef  :  NaN NaN NaN 
    ## t-value  :  NaN NaN NaN 
    ## Component:  3 
    ## Estimates:  0.243877 0 0.836649 
    ## se.coef  :  0.430106 0.008508 0.289586 
    ## t-value  :  0.567017 1e-06 2.889123 
    ## Component:  4 
    ## Estimates:  0.005396 0.004853 0.991698 
    ## se.coef  :  0.00188 0.001233 0.000745 
    ## t-value  :  2.870661 3.936343 1331.394 
    ## Component:  5 
    ## Estimates:  0.014443 0.006855 0.980982 
    ## se.coef  :  0.00439 0.002662 0.00355 
    ## t-value  :  3.289675 2.575504 276.3269 
    ## Component:  6 
    ## Estimates:  0.036717 0.014349 0.963893 
    ## se.coef  :  0.013937 0.004498 0.010079 
    ## t-value  :  2.634529 3.190058 95.63708 
    ## Component:  7 
    ## Estimates:  0.005254 0.006806 0.986183 
    ## se.coef  :  0.002062 0.002575 0.002053 
    ## t-value  :  2.548397 2.642899 480.3592

![](README_files/figure-markdown_strict/unnamed-chunk-10-1.png)

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
    ## Estimates:  1.434911 0.028269 0.114422 
    ## se.coef  :  0.620411 0.017502 0.371222 
    ## t-value  :  2.312839 1.615157 0.308231 
    ## Component:  2 
    ## Estimates:  0.019775 0.007549 0.981983 
    ## se.coef  :  0.00719 0.002475 0.003698 
    ## t-value  :  2.750222 3.04972 265.5731 
    ## Component:  3 
    ## Estimates:  0.039928 0.013145 0.968123 
    ## se.coef  :  0.019974 0.004817 0.010638 
    ## t-value  :  1.999006 2.728806 91.00411 
    ## Component:  4 
    ## Estimates:  0.004087 0.005095 0.992304 
    ## se.coef  :  0.001496 0.001019 0.000636 
    ## t-value  :  2.73087 4.999766 1559.171 
    ## Component:  5 
    ## Estimates:  0.019451 0.017918 0.972074 
    ## se.coef  :  0.009634 0.004451 0.006958 
    ## t-value  :  2.018941 4.025318 139.7078 
    ## Component:  6 
    ## Estimates:  0.036638 0.014494 0.963805 
    ## se.coef  :  0.013969 0.004557 0.010101 
    ## t-value  :  2.622865 3.180738 95.41983 
    ## Component:  7 
    ## Estimates:  0.003914 0.007495 0.987395 
    ## se.coef  :  0.001618 0.00209 0.001698 
    ## t-value  :  2.418743 3.586171 581.6305 
    ## Component:  8 
    ## Estimates:  0.000494 0.044952 0.946652 
    ## se.coef  :  0.000359 0.011909 0.015016 
    ## t-value  :  1.377213 3.774614 63.04486

    # After saving now the standardized residuals:
    StdRes <- DCCPre$sresi
    # We can now use these sresids to calculate the DCC model.
    # In order to fit the DCC model detach the tidyr and dplyr packages, 
    # once detached can now run dccFit
    # when done then tidyr and dplyr 
    detach("package:tidyverse", unload=TRUE)
    detach("package:tbl2xts", unload=TRUE)
    DCC <- dccFit(StdRes, type="Engle")

    ## Estimates:  0.95 0.03338368 20 
    ## st.errors:  NaN NaN 1.125954 
    ## t-values:   NaN NaN 17.76271

    pacman::p_load("tidyverse", "rmsfuns", "fmxdat", "tbl2xts", "broom")

    graph_rename_func_mv(input_name_1 = "ALSI_",
                         input_name_2 = "_ALSI",
                         title = "Dynamic Conditional Correlations: ALSI",
                         subtitle = "",
                         caption = "",
                         xlabel = "",
                         ylabel = "Rho")

![](README_files/figure-markdown_strict/unnamed-chunk-13-1.png)

## Graph: ALSI with Individual REITS

    graph_rename_func_mv(input_name_1 = "ALSI_CCO",
                         input_name_2 = "_ALSI",
                         title = "Dynamic Conditional Correlations: ALSI and CCO",
                         subtitle = "",
                         caption = "",
                         xlabel = "",
                         ylabel = "Rho")

![](README_files/figure-markdown_strict/unnamed-chunk-14-1.png)

    graph_rename_func_mv(input_name_1 = "ALSI_EMI",
                         input_name_2 = "_ALSI",
                         title = "Dynamic Conditional Correlations: ALSI and EMI",
                         subtitle = "",
                         caption = "",
                         xlabel = "",
                         ylabel = "Rho")

![](README_files/figure-markdown_strict/unnamed-chunk-15-1.png)

    graph_rename_func_mv(input_name_1 = "ALSI_GRT",
                         input_name_2 = "_ALSI",
                         title = "Dynamic Conditional Correlations: ALSI and GRT",
                         subtitle = "",
                         caption = "",
                         xlabel = "",
                         ylabel = "Rho")

![](README_files/figure-markdown_strict/unnamed-chunk-16-1.png)

    graph_rename_func_mv(input_name_1 = "ALSI_HYP",
                         input_name_2 = "_ALSI",
                         title = "Dynamic Conditional Correlations: ALSI and HYP",
                         subtitle = "",
                         caption = "",
                         xlabel = "",
                         ylabel = "Rho")

![](README_files/figure-markdown_strict/unnamed-chunk-17-1.png)

    graph_rename_func_mv(input_name_1 = "ALSI_RES",
                         input_name_2 = "_ALSI",
                         title = "Dynamic Conditional Correlations: ALSI and RES",
                         subtitle = "",
                         caption = "",
                         xlabel = "",
                         ylabel = "Rho")

![](README_files/figure-markdown_strict/unnamed-chunk-18-1.png)

    graph_rename_func_mv(input_name_1 = "ALSI_RDF",
                         input_name_2 = "_ALSI",
                         title = "Dynamic Conditional Correlations: ALSI and RDF",
                         subtitle = "",
                         caption = "",
                         xlabel = "",
                         ylabel = "Rho")

![](README_files/figure-markdown_strict/unnamed-chunk-19-1.png)

    graph_rename_func_mv(input_name_1 = "ALSI_SAC",
                         input_name_2 = "_ALSI",
                         title = "Dynamic Conditional Correlations: ALSI and SAC",
                         subtitle = "",
                         caption = "",
                         xlabel = "",
                         ylabel = "Rho")

![](README_files/figure-markdown_strict/unnamed-chunk-20-1.png)

## Graph: CCO with Individual REITS

    graph_rename_func_mv(input_name_1 = "CCO.._",
                         input_name_2 = "CCO.._ALSI",
                         title = "Dynamic Conditional Correlations: ALSI and RDF",
                         subtitle = "",
                         caption = "",
                         xlabel = "",
                         ylabel = "Rho")

![](README_files/figure-markdown_strict/unnamed-chunk-21-1.png)
