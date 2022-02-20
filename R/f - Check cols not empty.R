##############################################################################################
# NAME:         FUNCTION - CHECK COLS NOT EMPTY
# AUTHOR:       Pieter Overdevest
##############################################################################################

        f_check_cols_not_empty <- function(

                df.input,
                v.col,
                c.id
        ) {


        ######################################################################################
        # TEST
        ######################################################################################

        # df.input = data.frame(ID = c(1,2,3,4), pieter = c(1,2,NA,NA), bart = c(NA,NA,NA,4))
        # df.input = df.datachamp.baseline.source %>% filter(Status == "ACTIVE")
        # v.col    = v.feature.cannot.be.empty
        # v.col    = c("pieter", "bart")
        # c.id     = "ID"
        # f_check_cols_empty_cells(df.input, v.col, c.id)


        ######################################################################################
        # ERROR CHECKS
        ######################################################################################

        # Check presence of columns in df.input.
        f_check_cols_present(df.input, c.id)
        f_check_cols_present(df.input, v.unique)

        # Check that c.id does not contain NA and is unique.
        f_check_col_not_empty(df.input, c.id)
        f_check_col_unique(df.input, c.id)

        ######################################################################################
        # PROCESS
        ######################################################################################

        # Determine features with NA.
        df.temp <- df.input %>%

                # Select concerned columns
                select(all_of(v.col)) %>%

                # Determine number of NA per column
                f_info_per_column() %>%

                # Select columns with NA
                filter(n.na > 0) %>%

                # Create label.
                mutate(n.label = paste0(feature, " (", n.na, ")"))


        ######################################################################################
        # ERROR CHECK
        ######################################################################################

        if(nrow(df.temp) > 0) {

                # Create label.
                v.temp <- lapply(df.temp$feature, function(c.temp) { # c.temp = df.temp$feature[1]

                        paste0(
                                "'", c.temp, "' (",

                                f_paste(

                                        df.input %>%

                                                select(all_of(c(c.id, c.temp))) %>%

                                                filter(is.na(get(c.temp))) %>%

                                                pull(all_of(c.id))
                                ), ")"
                        )

                }) %>% unlist()


                stop(paste0(

                        "Note, the following features contain one ore more NAs, see '", c.id, "': ",
                        f_paste(v.temp)
                ))
        }

        }

