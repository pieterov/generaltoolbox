#' @title Check whether column is unique
#'
#' @description Are all elements in vector unique?
#'
#' @author Pieter Overdevest
#'
#' @param v.vector Vector with elements.
#' @param v.excluding Vector with elements to exclude, e.g., 0, NA or NULL (default: NA)
#'
#' @returns Boolean whether the vector contains unique values.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' b.unique <- f_is_unique(
#'
#'     v.vector    = c(1, 2, 3, NA),    ,
#'     v.excluding = NA
#' )

#################################################################################
# FUNCTION.
#################################################################################

        f_is_unique <- function(

                v.vector,
                v.excluding = NA
        ) {


#########################################################################
# PROCESS
#########################################################################

        # Verwijder elementen die niet meegenomen moeten worden, bijv NA en NULL.
        v.output <- v.vector[!v.vector %in% v.excluding]

        b.unique <- length(unique(v.output)) == length(v.output)

        # Check of er v.excluding elementen in v.vector zitten.
        if(b.unique & any(v.excluding %in% v.vector)) {

                warning(paste0(

                        "The vector ", deparse(substitute(v.vector)), " contains unique elements, however, ",
                        "only after removing one or more NAs present in the vector!"
                ))
        }

        return(b.unique)

        }
