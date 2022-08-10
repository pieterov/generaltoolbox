##############################################################################################
# NAME:         FUNCTION - CHECK COLS NOT ZERO
# AUTHOR:       Pieter Overdevest
##############################################################################################

        f_check_cols_not_zero <- function(

                df.input,
                v.col.include = NULL,
                v.col.exclude = NULL,
                c.id
        ) {


        ######################################################################################
        # TEST
        ######################################################################################

        # df.input = data.frame(ID = c(11,12,13,14,15), pieter = c(1,2,0,0,0), bart = c(NA,NA,0,4,0))
        # v.col.include    = c("pieter", "bart")
        # c.id     = "ID"
        # f_check_cols_not_zero(df.input, v.col.include, c.id)

        # df.input = df.datachamp.baseline.source %>% filter(Status == "ACTIVE")
        # v.col.include    = v.feature.cannot.be.empty


        ######################################################################################
        # ERROR CHECKS
        ######################################################################################

        # Check presence of columns in df.input.
        f_check_cols_present(df.input, c.id)

        if(!is.null(v.col.include)) f_check_cols_present(df.input, v.col.include)
        if(!is.null(v.col.exclude)) f_check_cols_present(df.input, v.col.exclude)

        # Check that c.id does not contain NA and is unique
        f_check_col_not_empty(df.input, c.id)
        f_check_col_unique(df.input, c.id)


        ######################################################################################
        # INITIALIZE
        ######################################################################################

        if(is.null(v.col.include))  v.col.include <- names(df.input)
        if(!is.null(v.col.exclude)) v.col.include <- setdiff(v.col.include, v.col.exclude)


        ######################################################################################
        # PROCESS
        ######################################################################################

        # Determine features with 0.
        df.temp <- df.input %>%

                # Select concerned columns
                select(all_of(v.col.include)) %>%

                # Determine number of NA per column
                f_summarize(b.view = FALSE, b.return = TRUE) %>%

                # Select columns with NA
                filter(n.zero > 0) %>%

                # Create label.
                mutate(n.label = paste0("'", feature, "' (", n.zero, ")"))


        ######################################################################################
        # ERROR CHECK
        ######################################################################################

        if(nrow(df.temp) > 0) {

                # Comms.
                cat(paste0(
                        "We observe features with ZERO values (out of ",
                        nrow(df.input), "): ", f_paste(df.temp$n.label), ".\n\n"
                ))


                # Create label.
                v.temp <- lapply(df.temp$feature, function(c.temp) { # c.temp = df.temp$feature[1]

                        paste0(
                                "'", c.temp, "' (",

                                f_paste(

                                        df.input %>%

                                                select(all_of(c(c.id, c.temp))) %>%

                                                filter(get(c.temp) == 0) %>%

                                                pull(all_of(c.id))
                                ), ")"
                        )

                }) %>% unlist()


                stop(paste0(

                        "Note, the following features contain one ore more ZERO values, see '", c.id, "': ",
                        f_paste(v.temp)
                ))
        }

        }

