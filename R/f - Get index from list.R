#' @title Get index of searched items in list
#'
#' @description Get index of searched items in list.
#'
#' @author Pieter Overdevest
#'
#' @param l.input -----
#' @param v.element -----
#' @param v.search -----
#'
#' @returns Vector with indices of elements that match searched information.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' v.output <- f_get_index_from_list(
#'
#'     l.input,
#'     v.element,
#'     v.search
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_get_index_from_list <- function(

                l.input,
                v.element,
                v.search
        ) {


        #################################################################################
        # Test
        #################################################################################

        # l.input   <- df.bord.source
        # v.element <- c("drager.id", "groep.id")
        # v.search  <- c(9324728, 1)

        # l.input   = df.jpg
        # v.element = "bord.fid"
        # v.search  = "00a0fdd7-997b-4406-bc24-0200f32f2718"


        #################################################################################
        # Error check
        #################################################################################

        if(length(v.element) != length(v.search)) {

                stop("Let op, v.element moet even lang zijn als v.search!")
        }


        #################################################################################
        # Main body
        #################################################################################

        v.result <- lapply(l.input, function(df.temp) { # df.temp <- l.input[[1]]

                l.result.element <- mapply(function(c.element, c.search) {

                        ifelse(
                                any(df.temp[[c.element]] == c.search),
                                TRUE, FALSE)
                        },

                        c.element = v.element,  # c.element = v.element[1]
                        c.search  = v.search    # c.search  = v.search[1]
                        )

                return(all(l.result.element))

        }) %>% unlist()


        #################################################################################
        # Return
        #################################################################################

        return(which(v.result))

        }
