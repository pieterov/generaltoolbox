#' @title Get number of columns in Google Sheet
#'
#' @description Get number of columns in Google Sheet.
#'
#' @author Pieter Overdevest
#'
#' @param c.gs.code Google Sheet code.
#' @param c.sheet Sheet name (default: "Sheet1")
#'
#' @returns Number of columns.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' n.col <- f_gs_col_number(
#'
#'       c.gs.code = "1Gc7Z1fxLgz60xg5wGpROMyfdwubk9mqeFaMzXh6WIjU"
#'       c.sheet   = "Delta"
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_gs_col_number <- function(

                c.gs.code,
                c.sheet = "Sheet1"
        ) {


        # Initialize.
        df.gs <- f_gs_url(c.gs.code, c.sheet) %>%

                gs4_get(.) %>%

                .[[6]]


        return(
                df.gs %>%

                        filter(name == c.sheet) %>%

                        pull(grid_columns)
        )

        }
