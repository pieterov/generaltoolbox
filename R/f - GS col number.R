##############################################################################################
# NAME:         f_gs_col_number - Number of columns of sheet in Google Sheet.
# AUTHOR:       Pieter Overdevest
##############################################################################################

        f_gs_col_number <- function(c.gs.code, c.sheet = "Sheet1") {

                # Testing.
                # c.gs.code <- "1Gc7Z1fxLgz60xg5wGpROMyfdwubk9mqeFaMzXh6WIjU"
                # c.sheet   <- "Delta"


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
