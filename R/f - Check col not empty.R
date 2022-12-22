#' @title Check whether column is empty
#'
#' @description Checks whether column is empty.
#'
#' @author Pieter Overdevest
#'
#' @param df.input -----
#' @param c.col -----
#'
#' @returns Nothing.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_check_col_not_empty(
#'
#'        df.input,
#'        c.col
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_check_col_not_empty <- function(

                df.input,
                c.col
        ) {


        ######################################################################################
        # TEST
        ######################################################################################

        # Test
        # df.input <- tibble(ID = letters[1:6], dummy1 = c(seq(4), NA, 4), dummy2 = seq(6))
        # c.col    <- "dummy1"
        # f_check_col_not_empty(df.input, c.col)


        ######################################################################################
        # ERROR CHECKS
        ######################################################################################

        # Check c.col in present in df.input.
        f_check_cols_present(df.input, c.col)

        # Check that c.col does not contain NA.
        if(any(is.na(df.input[[c.col]]))) {

                stop(paste0(

                        "Note, '", c.col, "' (c.col) in '", deparse(substitute(df.input)),
                        "' (df.input) cannot contain NAs!"
                ))
        }

        }
