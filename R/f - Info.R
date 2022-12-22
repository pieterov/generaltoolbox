#' @title Print basic statistics on vector or data frame with elements
#'
#' @description Print basic statistics on vector of elements or for each feature in a data frame. For each vector
#' (or feature) the function f_vector_info() is called.
#'
#' @author Pieter Overdevest
#'
#' @param x Vector or data frame with items to check statistics of.
#' @param n.top Max number of items to show in the list (default: 10).
#' @param show.freq Should frequency be shown? (default: TRUE).
#' @param n.width Number of characters to show in the list (default: 29).
#'
#'
#' @returns Nothing. Only prints to console.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_info(
#'
#'       x         = c("A", "B", "C", "A", "B", "A"),
#'       n.top     = 10,
#'       show.freq = TRUE,
#'       n.width   = 29
#' )


        #################################################################################
        # FUNCTIONS.
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

