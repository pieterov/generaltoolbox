#' @title  Apply function to list of items
#'
#' @description Applies function to list of items.
#'
#' @author Pieter Overdevest
#'
#' @param l.input List of items.
#' @param f.input Function to apply to each item.
#' @param mc.cores Number of cores (default: NULL)
#'
#' @returns List of items that have been processed with said function.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' l.output <- f_lapply(
#'
#'      l.input  = list(1, 2, 3),
#'      f.input  = function(x) {x+2},
#'      mc.cores = NULL
#' )


        #################################################################################
        # FUNCTION.
        #################################################################################

        f_lapply <- function(

                l.input,

                f.input,

                mc.cores = NULL
        ) {


        ######################################################################################
        # TEST
        ######################################################################################

        # f_test <- function(df.temp) {
        #
        #         df.temp %>%
        #
        #                 slice(-1, -n()) %>%
        #
        #                 rbind(df.temp)
        # }

        # l.input <- df.test
        # f.input <- f_test
        # mc.cores <- 4

        ######################################################################################
        # ERROR CHECKS
        ######################################################################################

        if(!any(c("list", "numeric", "integer", "character") %in% class(l.input))) {

                stop("Note, input to f_lapply - l.input - must be a list or vector of numeric or character values!")
        }


        if(class(f.input) != "function") {

                stop("Note, input to f_lapply - f.input - must be a function!")
        }


        if(!is.null(mc.cores)) {

                if(mc.cores > detectCores()) {

                        stop(paste0(

                                "Note, mc.cores (", mc.cores,

                                ") exceeds the number of cores in your system (",

                                detectCores(), ")!"
                        ))
                }
        }


        ######################################################################################
        # INITIALIZATION
        ######################################################################################

        ######################################################################################
        # PROCESS
        ######################################################################################

        if (!is.null(mc.cores) & (

                f_who_am_i() %in% c(

                        # MAcBook Pro
                        "Pieters-MacBook-Pro.local", "Pieters-MBP.home",

                        # Mac Studio
                        "Pieters-Mac-Studio.local"
                        )
                )
        ) {

                l.output <- mclapply(

                        X        = l.input,

                        FUN      = f.input,

                        mc.cores = mc.cores
                )


        } else {

                l.output <- lapply(

                        X        = l.input,

                        FUN      = f.input
                )
        }


        ######################################################################################
        # ERROR CHECK
        ######################################################################################


        ######################################################################################
        # RETURN
        ######################################################################################

        return(l.output)

        }

