##############################################################################################
# NAME:         f_gs_url - URL van Google Sheet.
# AUTHOR:       Pieter Overdevest
##############################################################################################

        f_gs_url <- function(c.gs.code, c.sheet = "Sheet1") {

                # Testing.
                # c.gs.code <- "1Gc7Z1fxLgz60xg5wGpROMyfdwubk9mqeFaMzXh6WIjU"
                # c.sheet   <- "Delta"

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
