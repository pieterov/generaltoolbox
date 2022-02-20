##############################################################################################
# NAME:         FUNCTION - CHECK COLS UNIQUE
# AUTHOR:       Pieter Overdevest
##############################################################################################

        f_check_cols_unique <- function(

                df.input,
                v.col,
                c.id
        ) {


        ######################################################################################
        # TEST
        ######################################################################################

        # Test!
        # df.input <- df.datachamp.baseline.source
        # v.col    <- v.feature.must.be.unique
        # c.id     <- "ID"

        # df.input <- tibble(ID = letters[1:6], `Variants: SKU` = c(seq(5), 5), dummy1 = c(seq(4), 4, 4), dummy2 = seq(6))
        # v.col    <- c("Variants: SKU", "dummy1", "dummy2")
        # c.id     <- "ID"
        # f_check_cols_unique(df.input, v.col, c.id)


        ######################################################################################
        # INITIALIZE
        ######################################################################################

        # Remove c.id from v.col, if present.
        v.col <- v.col[v.col != c.id]


        ######################################################################################
        # ERROR CHECKS
        ######################################################################################

        # Check presence of columns in df.input.
        f_check_cols_present(df.input, c.id)
        f_check_cols_present(df.input, v.col)

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

                # Select columns that are not unique
                filter(n.unique != n.tot) %>%

                # Sort by non-uniqueness.
                arrange(desc(n.unique)) %>%

                # Create label.
                mutate(n.label = paste0("'", feature, "' (", n.unique, ")"))


        ######################################################################################
        # ERROR CHECK
        ######################################################################################

        if(nrow(df.temp) > 0) {

                v.temp <- lapply(df.temp$feature, function(c.unique) { # c.unique <- df.temp$feature[1]

                        df.input %>%

                            # Select concerned columns
                            select(all_of(c(c.id, c.unique))) %>%

                            add_count(get(c.unique)) %>%

                            filter(n > 1) %>%

                            pull(c.id) %>%

                            f_paste(., b.quotation = TRUE) %>%

                            paste0("'", c.unique, "' (", ., ")")

                }) %>% unlist()


                stop(paste0(

                          "Note, the following features do not contain unique values (", c.id, "):\n",
                          f_paste(v.temp, b.sort = FALSE), ".\n\n"
                ))
        }

        }

