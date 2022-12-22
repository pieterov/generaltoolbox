#' @title Check whether columns are present
#'
#' @description Checks whether columns are present.
#'
#' @author Pieter Overdevest
#'
#' @param df.input -----
#' @param c.id -----
#'
#' @returns Nothing.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_check_cols_present(
#'
#'     df.input,
#'     v.col
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_check_cols_present <- function(

                df.input,
                v.col
        ) {


        ######################################################################################
        # TEST
        ######################################################################################

        # Test
        # df.input <- tibble(ID = letters[1:6], dummy1 = c(seq(4), 4, 4), dummy2 = seq(6))
        # v.col    <- c("dummy1", "dummy2", "dummy3", "dummy4")
        # f_check_cols_present(df.input, v.col)


        ######################################################################################
        # INITIALIZE
        ######################################################################################

        v.col.not.present <- v.col[!v.col %in% names(df.input)]


        ######################################################################################
        # ERROR CHECK
        ######################################################################################

        if(length(v.col.not.present) > 0) {

                stop(paste0(

                  "Note, ", f_paste(v.col.not.present, b.quotation = TRUE),
                  " are not present in '", deparse(substitute(df.input)), "'!"
                ))
        }

        }
