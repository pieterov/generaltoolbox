#' @title Plot geom point
#'
#' @description Plot geom point
#'
#' @author Pieter Overdevest
#'
#' @param df.plot Data frame with data to plot.
#' @param c.x Column name in said data frame to plot on the x-axis.
#' @param c.y Column name in said data frame to plot on the y-axis.
#' @param n.alpha Transparancy of points (default: 0.2)
#' @param b.log.x Should x-scale be log10? (default: FALSE)
#' @param b.log.y Should y-scale be log10? (default: FALSE)
#' @param v.abline Slope and intercept in a vector (default: NULL)
#' @param c.labs.x Label for x-axis (default: NULL)
#' @param c.labs.y Label for y-axis (default: NULL)
#' @param c.labs.title Main title (default: waiver())
#' @param c.labs.subtitle Subtitle (default: waiver())
#' @param c.labs.caption Caption under the plot (default: waiver())
#'
#' @returns A ggplot object that can be printed.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_geom_point(
#'
#'        df.plot         df.plot,
#'        c.x             = 'columnx',
#'        c.y             = 'columny',
#'        n.alpha         = 0.2,
#'        b.log.x         = FALSE,
#'        b.log.y         = FALSE,
#'        v.abline        = NULL,
#'        c.labs.x        = NULL,
#'        c.labs.y        = NULL,
#'        c.labs.title    = waiver(),
#'        c.labs.subtitle = waiver(),
#'        c.labs.caption  = waiver()
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_geom_point <- function(

                df.plot,
                c.x,
                c.y,
                n.alpha         = 0.2,
                b.log.x         = FALSE,
                b.log.y         = FALSE,
                v.abline        = NULL,
                c.labs.x        = NULL,
                c.labs.y        = NULL,
                c.labs.title    = waiver(),
                c.labs.subtitle = waiver(),
                c.labs.caption  = waiver()
        ) {


        ######################################################################################
        # TEST
        ######################################################################################

        ######################################################################################
        # ERROR CHECK
        ######################################################################################

        # Error check.
        if(!x %in% names(df.plot)) {
                stop(glue("Column name {x} does not occur in the data frame to plot."))
        }

        if(!y %in% names(df.plot)) {
                stop(glue("Column name {y} does not occur in the data frame to plot."))
        }

        if(!is.null(v.abline)) {
                if(length(v.abline) != 2) {
                        stop(
                                "Object 'v.abline' must have two numbers, the first is the slope ",
                                "and the second one is the intercept of the abline to be plotted."
                        )
                }
        }


        ######################################################################################
        # INITIALIZE
        ######################################################################################

        ######################################################################################
        # MAIN
        ######################################################################################

        # Create base plot.
        gg.output <- ggplot(data = df.plot) +

                geom_point(
                        mapping = aes(

                                x = get(x),
                                y = get(y)
                        ),

                        alpha = n.alpha
                ) +

                labs(
                        x        = ifelse(is.null(c.labs.x), c.x, c.labs.x),
                        y        = ifelse(is.null(c.labs.y), c.y, c.labs.y),
                        title    = c.labs.title,
                        subtitle = c.labs.subtitle,
                        caption  = c.labs.caption,
                ) +

                theme.figure

        # Set axis to log scale if requested.
        if(b.log.x) {

                gg.output <- gg.output + scale_x_log10(

                        breaks = trans_breaks("log10", function(x) 10^x),
                        labels = trans_format("log10", math_format(10^.x))
                )
        }

        if(b.log.y) {

                gg.output <- gg.output + scale_y_log10(

                        breaks = trans_breaks("log10", function(x) 10^x),
                        labels = trans_format("log10", math_format(10^.x))
                )
        }

        if(!is.null(v.abline)) {

                gg.output <- gg.output +

                        geom_abline(

                                slope     = v.abline[1],
                                intercept = v.abline[2],
                                linetype  = 2
                        )
        }

        ######################################################################################
        # RETURN
        ######################################################################################

        return(gg.output)

        }
