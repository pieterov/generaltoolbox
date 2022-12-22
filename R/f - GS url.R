#' @title Get URL to Google Sheet
#'
#' @description Get URL to Google Sheet.
#'
#' @author Pieter Overdevest
#'
#' @param c.gs.code Google Sheet code.
#' @param c.sheet Sheet name (default: "Sheet1")
#'
#' @returns URL to Google Sheet.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' c.url <- f_gs_url(
#'
#'       c.gs.code = "1Gc7Z1fxLgz60xg5wGpROMyfdwubk9mqeFaMzXh6WIjU"
#'       c.sheet   = "Delta"
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_gs_url <- function(

                c.gs.code,
                c.sheet = "Sheet1"
        ) {


                # Initialize.
                c.url.gs <- paste0(

                        "https://docs.google.com/spreadsheets/d/",
                        c.gs.code
                )


                df.gs <- gs4_get(c.url.gs)[[6]]


                c.sheet <- ifelse(is.null(c.sheet), "Sheet1", c.sheet)


                # Error check!
                if(!c.sheet %in% df.gs$name) {

                        stop(
                                "Let op, sheet '", c.sheet, "' komt niet voor in Google Sheet '",

                                gs4_get(c.url.gs)[[3]], "'!"
                        )
                }

                return(

                        paste0(
                                c.url.gs,
                                "/edit#gid=",
                                df.gs %>% filter(name == c.sheet) %>% pull(id)
                        )
                )
        }
