#' @title  Apply function to list of items
#'
#' @description Applies function to list of items in parallel or sequentially.
#'
#' @author Pieter Overdevest
#'
#' @param l.input  List of items.
#' @param f.input  Function to apply to each item.
#' @param ...      Additional arguments passed to f.input.
#' @param mc.cores Number of cores (default: NULL). If NULL or 1, runs sequentially.
#'
#' @returns List of items that have been processed with said function.
#'
#' @details Uses future.apply for cross-platform parallel execution (compatible with Positron, RStudio, macOS, and Windows).
#'
#' @export
#'
#' @examples
#' l.output <- f_lapply(
#'      l.input  = list(1, 2, 3),
#'      f.input  = function(x) {x + 2},
#'      mc.cores = NULL
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_lapply <- function(
                l.input,
                f.input,
                ...,
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

        if (!is.list(l.input) && !is.numeric(l.input) && !is.character(l.input)) {
                stop(
                        "Note, input to f_lapply - l.input - must be a list or vector of ",
                        "numeric or character values!"
                )
        }

        if (!is.function(f.input)) {
                stop("Note, input to f_lapply - f.input - must be a function!")
        }

        if (!is.null(mc.cores)) {
                n_cores_available <- parallel::detectCores(logical = TRUE)

                if (mc.cores > n_cores_available) {
                        stop(paste0(
                                "Note, mc.cores (", mc.cores,
                                ") exceeds the number of cores in your system (",
                                n_cores_available, ")!"
                        ))
                }
        }

        ######################################################################################
        # PROCESS
        ######################################################################################

        if (!is.null(mc.cores) && mc.cores > 1) {

                # Set up background process workers
                future::plan(future::multisession, workers = mc.cores)
                
                # Ensure plan resets back to sequential processing on exit/error
                on.exit(future::plan(future::sequential), add = TRUE)

                l.output <- future.apply::future_lapply(
                        X               = l.input,
                        FUN             = f.input,
                        ...,
                        future.seed     = TRUE,
                        future.packages = rev(.packages())
                )

        } else {

                l.output <- lapply(
                        X   = l.input,
                        FUN = f.input,
                        ...
                )
        }

        ######################################################################################
        # RETURN
        ######################################################################################

        return(l.output)
}