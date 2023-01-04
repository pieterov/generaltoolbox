#' @title Search values in list of data frames
#'
#' @description Search values in list of data frames.
#'
#' @author Pieter Overdevest
#'
#' @param l.input List of data frames, in which we want to determine which data frame(s) contain the requested values.
#' @param v.element Vector of volumn name(s) in the data frames in which we search for requested values.
#' @param l.search List of vectors with value(s) to search for in said column(s).
#' @param v.strategy Vector of "all" and/or "any", corresponding to each column name that is searched in. This allows to
#' match all values or to match any of the values in the respective vector in l.search. In case of the default (NULL)
#' all values in the respective vector in l.search must match.
#'
#' @returns Vector of list indices that contain requested values.
#'
#' @details Note, v.element, l.search, and v.strategy (if given) must have equal length.
#'
#' @export
#'
#' @examples
#' v.output <- f_search_values_in_list_of_data_frames(
#'
#'     l.input    = l.bord.allocation,
#'     v.element  = c("bord.type", "jaar.laatste.vastlegging"),
#'     l.search   = list(c("A01120", "A01100"), "2021"),
#'     v.strategy = NULL
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_search_values_in_list_of_data_frames <- function(

                l.input,
                v.element,
                l.search,
                v.strategy = NULL
        ) {


        #################################################################################
        # Test
        #################################################################################

        # l.input    = l.bord.allocation
        # v.element  = c("bord.type", "jaar.laatste.vastlegging")
        # l.search   = list(c("A01120", "A01100"), "2021")
        # v.strategy = NULL


        #################################################################################
        # Initialization
        #################################################################################

        if(is.null(v.strategy)) {

                v.strategy = rep("all", length(v.element))
        }


        #################################################################################
        # Error check
        #################################################################################

        if(length(v.element) != length(l.search) & length(v.element) != length(v.strategy)) {

                stop("Note, v.element, l.search, and v.strategy must have equal length!")
        }


        #################################################################################
        # Main body
        #################################################################################

        v.result <-

                lapply(l.input, function(df.temp) { # df.temp <- l.input[[1]]

                        # Error check!
                        if(!"data.frame" %in% class(df.temp)) {

                                stop("Note, the list ('l.input') must contain data frames!")
                        }

                        # Determine which data frames contain the requested values.
                        v.temp <-

                                lapply(seq_along(v.element), function(i) { # i = 1

                                        if(v.strategy[i] == "all") {

                                                # Are all arguments in l.search[[i]] present in column v.element[i]?
                                                all(l.search[[i]] %in% df.temp[[v.element[i]]])

                                        } else {

                                                # Is any of the arguments in l.search[[i]] present in column v.element[i]?
                                                any(l.search[[i]] %in% df.temp[[v.element[i]]])
                                        }

                                }) %>% unlist()

                        return( all(v.temp) )

                }) %>% unlist()


        #################################################################################
        # Return
        #################################################################################

        return(which(v.result))

        }
