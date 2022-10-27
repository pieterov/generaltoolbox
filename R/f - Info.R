#################################################################################
# NAME:         FUNCTION - Info
# AUTHOR:       Pieter Overdevest.
# DESCRIPTION:  Give basic info on vector or data frame.
#################################################################################

        f_info <- function(

                x,
                n.top     = 10,
                show.freq = TRUE,
                n.width   = 29
        ) {


        # Error Check
        if(n.width < 29) {

                warning("Note, n.width cannot be smaller than 29. It was set to 29!")

                n.width <- 29
        }


        # Als x een dataframe is.
        if(is.data.frame(x)) {

                for (c.column in colnames(x)) { # c.column <- colnames(x)[7]

                        cat(
                                paste0(
                                        "\n\n\n\nField name: ", c.column, "\n"
                                )
                        )

                        f_vector_info(

                                v.input   = x[[c.column]],
                                name      = c.column,
                                n.top     = n.top,
                                show.freq = show.freq,
                                n.width   = n.width
                        ) }

        } else {

                f_vector_info(

                        v.input   = x,
                        name      = deparse(substitute(x)),
                        n.top     = n.top,
                        show.freq = show.freq,
                        n.width   = n.width
                )
        }
}

