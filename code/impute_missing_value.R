

# Note I set impute_returns_method to NONE by default. This will produce a warning if there is indeed a missing return in the matrix...

impute_missing_returns <- function(return_mat, impute_returns_method = "NONE"){
    # Make sure we have a date column called date:
    if( !"date" %in% colnames(return_mat) ) stop("No 'date' column provided in return_mat. Try again please.")

    # Note my use of 'any' below...
    # Also note that I 'return' return_mat - which stops the function and returns return_mat.
    if( impute_returns_method %in% c("NONE", "None", "none") ) {
        if( any(is.na(return_mat)) ) warning("There are missing values in the return matrix.. Consider maybe using impute_returns_method = 'Drawn_Distribution_Own' / 'Drawn_Distribution_Collective'")
        return(return_mat)
    }


    if( impute_returns_method  == "Average") {

        return_mat <-
            return_mat %>% gather(Tickers, Return, -date) %>%
            group_by(date) %>%
            mutate(Avg = mean(Return, na.rm=T)) %>%
            mutate(Avg = coalesce(Avg, 0)) %>% # date with no returns - set avg to zero
            ungroup() %>%
            mutate(Return = coalesce(Return, Avg)) %>% select(-Avg) %>% spread(Tickers, Return)

        # That is just so much easier when tidy right? See how I gathered and spread again to give back a wide df?
        return(return_mat)
    } else

        if( impute_returns_method  == "Drawn_Distribution_Own") {

            N <- nrow(return_mat)
            return_mat <-
                # DIY: see what density function does
                left_join(return_mat %>% gather(Tickers, Return, -date),
                          return_mat %>% gather(Tickers, Return, -date) %>% group_by(Tickers) %>%
                              mutate(Dens = list(density(Return, na.rm=T))) %>%
                              summarise(set.seed(as.numeric(format( Sys.time(), format = "%s"))/1e3*sample(1:100)[1]), Random_Draws = list(sample(Dens[[1]]$x, N, replace = TRUE, prob=.$Dens[[1]]$y))),
                          by = "Tickers"
                ) %>%  group_by(Tickers) %>%
                # Random draw from sample:
                mutate(Return = coalesce(Return, Random_Draws[[1]][row_number()])) %>%
                select(-Random_Draws) %>% ungroup() %>% spread(Tickers, Return)
            return(return_mat)
        } else

            if( impute_returns_method  == "Drawn_Distribution_Collective") {
                NAll <- nrow(return_mat %>% gather(Tickers, Return, -date))
                # DIY: see what density function does
                return_mat <-
                    bind_cols(
                        return_mat %>% gather(Tickers, Return, -date),
                        return_mat %>% gather(Tickers, Return, -date) %>%
                            mutate(Dens = list(density(Return, na.rm=T))) %>%
                            summarise(set.seed(as.numeric(format( Sys.time(), format = "%s"))/1e3*sample(1:100)[1]), Random_Draws = list(sample(Dens[[1]]$x, NAll, replace = TRUE, prob=.$Dens[[1]]$y))) %>%
                            unnest(Random_Draws)
                    ) %>%
                    mutate(Return = coalesce(Return, Random_Draws)) %>% select(-Random_Draws) %>% spread(Tickers, Return)
                return(return_mat)
            } else

                if( impute_returns_method  == "Zero") {
                    warning("This is probably not the best idea but who am I to judge....")
                    return_mat[is.na(return_mat)] <- 0
                    return(return_mat)
                } else
                    stop("Please provide a valid impute_returns_method method. Options include:\n'Average', 'Drawn_Distribution_Own', 'Drawn_Distribution_Collective' and 'Zero'.")

    return_mat

}

######-------USE THIS BELOW---------#######


#return_mat_Quick = T40 %>% select(date, Tickers, Return) %>%
#filter(date > lubridate::ymd(20150101)) %>% spread(Tickers, Return)

#new_data <- impute_missing_returns(return_mat_Quick, impute_returns_method = "Average")
