#' @title Calculate object size
#'
#' @description Calculate object size.
#'
#' @author Pieter Overdevest
#'
#' @param df.input Object to get size of.
#'
#' @returns Size of object in KB, MB, GB, depending of size.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_object_size(
#'
#'     df.input = mtcars
#' )


        #################################################################################
        # FUNCTION.
        #################################################################################

        f_object_size <- function(df.input) {


        return(

                format(object.size(df.input), unit = "auto")

        )

        }
