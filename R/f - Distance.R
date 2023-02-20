#' @title Calculate distances
#'
#' @description Calculates distances.
#'
#' @author Pieter Overdevest
#'
#' @param x1,y1 Coordinates of first point (default: y1 = NULL).
#' @param x2,y2 Coordinates of second point (default: x2, y2 = NULL).
#'
#' @returns Vector with distances.
#'
#' @details In case a single vector of 4 elements is provided, then x1, x2, x2, and y2 are
#' reassigned using x1.
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

                x1, y1 = NULL, x2 = NULL, y2 = NULL
        ) {
                # In case x1, y1, x2, y2 are supplied in single vector.
                if(is.null(y1) & is.null(x2) & is.null(y2)) {

                        if(length(x1) != 4) {
                                stop('If x1, y1, x2, and y2 are provided in single vector, its length must be 4.')
                        }

                        y1 = x1[2]
                        x2 = x1[3]
                        y2 = x1[4]
                        x1 = x1[1]
                }

                return(
                        sqrt((x1 - x2)^2 + (y1 - y2)^2)
                )

        }
