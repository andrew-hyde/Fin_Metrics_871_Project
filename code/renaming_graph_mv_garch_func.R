
# Rhot <- DCC$rho.t
# # Right, so it gives us all the columns together in the form:
# # X1,X1 ; X1,X2 ; X1,X3 ; ....
#
# # So, let's be clever about defining more informative col names.
# # I will create a renaming function below:
# ReturnSeries = xts_q5_data_combined
# DCC.TV.Cor = Rhot

graph_rename_func_mv <- function(input_name_1, input_name_2, title, subtitle, caption, xlabel, ylabel) {

    renamingdcc <- function(ReturnSeries, DCC.TV.Cor) {

        ncolrtn <- ncol(ReturnSeries)
        namesrtn <- colnames(ReturnSeries)
        paste(namesrtn, collapse = "_")

        nam <- c()
        xx <- mapply(rep, times = ncolrtn:1, x = namesrtn)
        # Now let's be creative in designing a nested for loop to save the names corresponding to the columns of interest..

        # TIP: draw what you want to achieve on a paper first. Then apply code.

        # See if you can do this on your own first.. Then check vs my solution:

        nam <- c()
        for (j in 1:(ncolrtn)) {
            for (i in 1:(ncolrtn)) {
                nam[(i + (j-1)*(ncolrtn))] <- paste(xx[[j]][1], xx[[i]][1], sep="_")
            }
        }

        colnames(DCC.TV.Cor) <- nam

        # So to plot all the time-varying correlations wrt SBK:
        # First append the date column that has (again) been removed...
        DCC.TV.Cor <-
            data.frame( cbind( date = index(ReturnSeries), DCC.TV.Cor)) %>% # Add date column which dropped away...
            mutate(date = as.Date(date)) %>%  tbl_df()

        DCC.TV.Cor <- DCC.TV.Cor %>% gather(Pairs, Rho, -date)


    }

    Rhot_use <-
        renamingdcc(ReturnSeries = xts_ALSI_data_combined_use,
                    DCC.TV.Cor = DCC$rho.t)


    graph <-
        ggplot(Rhot_use %>% filter(grepl(input_name_1, Pairs ), !grepl(input_name_2, Pairs)) ) +
        geom_line(aes(x = date, y = Rho, colour = Pairs)) +

        labs(title = title,
             subtitle = subtitle,
             caption = caption,
             x = xlabel,
             y = ylabel) +


        theme() +
        theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
        theme(legend.position="bottom") +
        theme(legend.title = element_blank())


    graph

}



