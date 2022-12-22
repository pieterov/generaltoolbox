#' @title Welcome!
#'
#' @description Gives welcome.
#'
#' @author Pieter Overdevest
#'
#' @param x Name to welcome (default: "Pieter").
#'
#' @returns String of welcome!
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_welcome(
#'
#'     x = "Pieter"
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_welcome <- function(x = "Pieter") {

                return(paste0("Welcome ", x, "!"))
        }
