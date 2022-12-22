#' @title Interpolate between points
#'
#' @description Interpolate between points when their distance exceeds threshold.
#'
#' @author Pieter Overdevest
#'
#' @param df.input -----
#' @param v.coord.point Vector with names of x- and y-coordinate (default: c("point.x", "point.y")).
#' @param n.threshold.distance Distance between interpolated points will not exceed n.threshold.distance.
#'
#' @returns Data frame with interpolated coordinates.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' df.output <- f_interpolate(
#'
#'    df.input             = tibble(
#'
#'         point.x = c(110000, 130000, 150000, 130000, 110000),
#'
#'         point.y = c(400000, 430000, 400000, 370000, 400000)
#'    ),
#'
#'    v.coord.point        = c("point.x", "point.y"),
#'
#'    n.threshold.distance = 500
#' )

#################################################################################
# FUNCTION.
#################################################################################

        f_interpolate <- function(

                df.input,
                v.coord.point = c("point.x", "point.y"),
                n.threshold.distance
        ) {


#########################################################################
# TEST ONLY
#########################################################################

        # df.input <- tibble(
        #
        #         polygon.x = c(110000, 130000, 150000, 130000, 110000),
        #
        #         polygon.y = c(400000, 430000, 400000, 370000, 400000)
        # )

        # f_plot_leaflet(
        #
        #         df.polygon      = df.input %>% mutate(polygon.id = 1),
        #         c.leaflet.title = "TEST",
        #         n.zoom          = 9,
        #         b.save.leaflet  = FALSE
        # )

        # df.input             <- c
        # v.coord.point        <- c("polygon.x", "polygon.y")
        # n.threshold.distance <- 500


#########################################################################
# ERROR CHECK
#########################################################################

        if(!all(v.coord.point %in% names(df.input))) {

                stop("Note, the point coordinates, ", f_paste(v.coord.point, b.quotation = TRUE), " do not occur in df.input.")
        }

        if(ncol(df.input) > 2) {

                stop("Note, the input data frame can only contain the point coordinates, ", f_paste(v.coord.point, b.quotation = TRUE), ".")
        }


#########################################################################
# INITIALIZATION
#########################################################################

        # Determine features containing x,y coordinates.
        c.coord.x <- v.coord.point[1]
        c.coord.y <- v.coord.point[2]

        # Is polygon?
        b.is.polygon <- identical( head(df.input,1), tail(df.input, 1) )


#########################################################################
# INTERPOLATE
#########################################################################

        df.output <- lapply(seq(nrow(df.input)-1), function(i) { # i = 1

                x1 <- df.input[[c.coord.x]][i]
                y1 <- df.input[[c.coord.y]][i]
                x2 <- df.input[[c.coord.x]][i+1]
                y2 <- df.input[[c.coord.y]][i+1]

                # Calculate distance between two points.
                d <- f_distance(x1, y1, x2, y2)

                #print(d)

                if(d > n.threshold.distance) {

                        df.temp <- tibble(

                                # We want the distance between interpolated points not to exceed n.threshold.distance.
                                x = seq(x1, x2, length.out = 1 + (d / n.threshold.distance)),
                                y = seq(y1, y2, length.out = 1 + (d / n.threshold.distance))
                        )

                        names(df.temp) <- v.coord.point

                        #print(df.temp)

                } else {

                        df.temp <- df.input[i:(i+1),]
                }

                return(df.temp)

        }) %>% rbindlist()

        #print(df.output)

        # Clean up doubles
        if(b.is.polygon) {

                df.output <- df.output %>%

                        # Remove last row
                        head(-1) %>%

                        # Remove doubles
                        distinct() %>%

                        rbind(head(., 1))

        } else {

                df.output <- df.output %>%

                        distinct()
        }


#########################################################################
# Return df.output.
#########################################################################

        return(df.output)

}
