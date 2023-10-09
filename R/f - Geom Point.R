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
#' @param b.grid.major.x Should grid added on x-axis (default: FALSE)
#' @param b.grid.major.y Should grid added on y-axis (default: FALSE)
#' @param b.coord.fixed Should x-axis and y-axis have fixed ratio of 1 (default: FALSE)
#' @param v.scale.x.continuous Breaks along x-axis (default: NULL)
#' @param v.scale.y.continuous Breaks along y-axis (default: NULL)
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
                c.facet              = NULL,
                n.alpha              = 0.2,
                b.log.x              = FALSE,
                b.log.y              = FALSE,
                v.abline             = NULL,
                c.labs.x             = NULL,
                c.labs.y             = NULL,
                b.grid.major.x       = FALSE,
                b.grid.major.y       = FALSE,
                b.coord.fixed        = FALSE,
                v.scale.x.continuous = NULL,
                v.scale.y.continuous = NULL,
                c.labs.title         = waiver(),
                c.labs.subtitle      = waiver(),
                c.labs.caption       = waiver()
        ) {


        ######################################################################################
        # TEST
        ######################################################################################

        # ALWAYS
        # c.facet         = NULL
        # n.alpha         = 0.2
        # b.log.x         = FALSE
        # b.log.y         = FALSE
        # v.abline        = NULL
        # c.labs.x        = NULL
        # c.labs.y        = NULL
        # b.grid.major.x  = FALSE
        # b.grid.major.y  = FALSE
        # b.coord.fixed   = FALSE
        # v.scale.x.continuous = NULL
        # v.scale.y.continuous = NULL
        # c.labs.title    = waiver()
        # c.labs.subtitle = waiver()
        # c.labs.caption  = waiver()

        # SET 1:
        # df.plot  = df.ttemp.error.nn.temp.dist.cog %>%
        #         select(dist.thd, name.tp, ttemp, error.ttemp, error.ttemp.both) %>% na.omit()
        # c.x             = 'error.ttemp'
        # c.y             = 'error.ttemp.both'
        # b.grid.major.x  = TRUE
        # b.grid.major.y  = TRUE
        # b.log.x         = FALSE
        # b.log.y         = FALSE
        # b.coord.fixed   = TRUE
        # c.facet         = 'dist.thd'
        # v.abline        = c(1, 1e-15)
        # n.alpha         = 0.3
        # c.labs.x        = paste('Error using', c.use,  'source')
        # c.labs.y        = 'Error using both sources'
        # v.scale.x.continuous = seq(0, 45, by = 5)
        # v.scale.y.continuous = seq(0, 45, by = 5)

        # SET 2:
        # df.plot              = df %>% filter(final.name.nn == x)
        # c.x                  = paste0(c.axis, '.ttemp')
        # c.y                  = paste0(c.axis, '.ttemp.both')
        # b.grid.major.x       = TRUE
        # b.grid.major.y       = TRUE
        # b.log.x              = FALSE
        # b.log.y              = FALSE
        # b.coord.fixed        = TRUE
        # v.scale.x.continuous = seq(0, 45, by = 5)
        # v.scale.y.continuous = seq(0, 45, by = 5)
        # c.facet              = 'dist.thd'
        # v.abline             = c(1, 1e-15)
        # n.alpha              = 0.3
        # c.labs.x             = paste(c.lab, 'using', c.use, 'source')
        # c.labs.y             = paste(c.lab, 'using both sources')



        ######################################################################################
        # ERROR CHECK
        ######################################################################################

        # Error check.
        if(!c.x %in% names(df.plot)) {
                stop(glue("Column name {x} does not occur in the data frame to plot."))
        }

        if(!c.y %in% names(df.plot)) {
                stop(glue("Column name {y} does not occur in the data frame to plot."))
        }

        if(!is.null(c.facet)) {
                if(!c.facet %in% names(df.plot)) {
                        stop(glue("Column name {c.facet} does not occur in the data frame to plot."))
                }
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

                                x = get(c.x),
                                y = get(c.y)
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


        if(!is.null(c.facet)) {

                gg.output <- gg.output + facet_wrap(~get(c.facet))
        }


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


        if(b.coord.fixed) {
                gg.output <- gg.output + coord_fixed(ratio=1)
        }


        if(!is.null(v.abline)) {

                gg.output <- gg.output +

                        geom_abline(

                                slope     = v.abline[1],
                                intercept = v.abline[2],
                                linetype  = 2
                        )
        }


        if(b.grid.major.x) {

                gg.output <- gg.output + theme(

                        panel.grid.major.x = element_line(

                                color     = "grey60",
                                linewidth = 0.5,
                                linetype  = 2)
                        )
        }

        if(b.grid.major.y) {

                gg.output <- gg.output + theme(

                        panel.grid.major.y = element_line(

                                color     = "grey60",
                                linewidth = 0.5,
                                linetype  = 2)
                )
        }


        # Breaks along x-axis.
        if(!is.null(v.scale.x.continuous)) {

                gg.output <- gg.output +

                        scale_x_continuous(
                                breaks = v.scale.x.continuous,
                                limits = c(min(v.scale.x.continuous), max(v.scale.x.continuous))
                        )

        }


        # Breaks along y-axis.
        if(!is.null(v.scale.y.continuous)) {

                gg.output <- gg.output +

                        scale_y_continuous(
                                breaks = v.scale.y.continuous,
                                limits = c(min(v.scale.y.continuous), max(v.scale.y.continuous))
                        )
        }


        ######################################################################################
        # RETURN
        ######################################################################################

        return(gg.output)

        }
