##############################################################################################
# NAME:         FUNCTION - CHECK COL UNIQUE
# AUTHOR:       Pieter Overdevest
##############################################################################################

        f_check_col_unique <- function(

                df.input,
                c.col
        ) {


        ######################################################################################
        # TEST
        ######################################################################################

        # Test
        # df.input <- tibble::tibble(ID = letters[1:6], dummy1 = c(seq(4), 4, 4), dummy2 = seq(6))
        # c.col    <- "dummy1"
        # f_check_col_unique(df.input, c.col)


        ######################################################################################
        # ERROR CHECKS
        ######################################################################################

        # Check c.col in present in df.input.
        f_check_cols_present(df.input, c.col)

        # Check c.col does not contain empty cells.
        f_check_col_not_empty(df.input, c.col)

        # Check that c.col does not contain NA.
        if(!f_is_unique(df.input[[c.col]])) {

                stop(paste0(

                        "Note, '", c.col, "' (c.col) in '", deparse(substitute(df.input)),
                        "' (df.input) must contain unique values!"
                ))
        }

        }
