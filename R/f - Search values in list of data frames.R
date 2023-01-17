#' @title Search values in list of data frames
#'
#' @description Search values in list of data frames.
#'
#' @author Pieter Overdevest
#'
#' @param l.input List of data frames, in which we want to determine which data frame(s) contain the requested values.
#' @param v.key Vector of volumn name(s) in the data frames in which we search for requested values.
#' @param l.value List of vectors with value(s) to search for in said column(s).
#' @param v.strategy Vector of "all" and/or "any", corresponding to each column name that is searched in. This allows to
#' match all values or to match any of the values in the respective vector in l.value. In case of the default (NULL)
#' all values in the respective vector in l.value must match.
#'
#' @returns Vector of list indices that contain requested values.
#'
#' @details Note, v.key, l.value, and v.strategy (if given) must have equal length.
#'
#' @export
#'
#' @examples
#' v.output <- f_search_values_in_list_of_data_frames(
#'
#'     l.input    = l.bord.allocation,
#'     v.key      = c("bord.type", "jaar.laatste.vastlegging"),
#'     l.value    = list(c("A01120", "A01100"), "2021"),
#'     v.strategy = NULL
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_search_values_in_list_of_data_frames <- function(

                l.input,
                v.key,
                l.value,
                v.strategy = NULL
        ) {


        #################################################################################
        # Test
        #################################################################################

        # l.input    = l.bord.allocation
        # v.key      = c("bord.type", "jaar.laatste.vastlegging")
        # l.value    = list(c("A01120", "A01100"), "2021")
        # v.strategy = NULL

        # l.input    = df.jpg
        # v.key      = "bord.id"
        # l.value    = list(13648927)
        # v.strategy = NULL


        #################################################################################
        # Initialization
        #################################################################################

        # Define v.strategy if not provided.
        if(is.null(v.strategy)) {

                v.strategy = rep("all", length(v.key))
        }

        # Maak l.value een list als deze dat nog niet is; alleen als lengte 1 is!
        if(length(v.key) == 1 & length(l.value) == 1 & !"list" %in% class(l.value)) {

                l.value <- list(l.value)
        }


        #################################################################################
        # Error check
        #################################################################################

        # Check dat v.key en l.value even lang zijn.
        if(length(v.key) != length(l.value) & length(v.key) != length(v.strategy)) {

                stop("v.key, l.value, and v.strategy must have equal length!")
        }

        # Check dat l.input een list is.
        if(!"list" %in% class(l.input)) {

                stop("l.input must be a list!")
        }

        # Check dat l.value een list is.
        if(!"list" %in% class(l.value)) {

                stop("l.value must be a list!")
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

                                lapply(seq_along(v.key), function(i) { # i = 1

                                        if(v.strategy[i] == "all") {

                                                # Are all arguments in l.value[[i]] present in column v.key[i]?
                                                all(l.value[[i]] %in% df.temp[[v.key[i]]])

                                        } else {

                                                # Is any of the arguments in l.value[[i]] present in column v.key[i]?
                                                any(l.value[[i]] %in% df.temp[[v.key[i]]])
                                        }

                                }) %>% unlist()

                        return( all(v.temp) )

                }) %>% unlist()


        #################################################################################
        # Return
        #################################################################################

        return(which(v.result))

        }
