#' @title Calculate distances
#'
#' @description Calculates distances.
#'
#' @author Pieter Overdevest
#'
#' @param x1,y1 Coordinates of first point.
#' @param x2,y2 Coordinates of second point.
#'
#' @returns Vector with distances.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' v.distance <- f_distance(
#'
#'     x1 = c(1 ,1),
#'     y1 = c(1, 2),
#'     x2 = c(3, 3),
#'     y2 = c(4, 5)
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_distance <- function(

                x1, y1, x2, y2
        ) {

                return(
                        sqrt((x1 - x2)^2 + (y1 - y2)^2)
                )

        }
