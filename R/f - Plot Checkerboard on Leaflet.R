##############################################################################
# FUNCTION - Plot Checkerboard on Leaflet
#
# Name:         Pieter Overdevest.
# Date:         Oct 18, 2021.
#
##############################################################################

        f_plot_checkerboard_on_leaflet = function(

                df.input,
                v.coord.point,
                c.value,
                c.leaflet.title,
                n.layer,
                n.round,
                n.dig.lab,
                v.info.tag.polygon.label,
                v.info.veld.polygon.label,
                v.info.tag.polygon.popup,
                v.info.veld.polygon.popup,
                b.save.leaflet
        ) {


##############################################################################
# TEST ONLY!!
##############################################################################

        # Testing
        # df.input                  = df.temp
        # v.coord.point             = c("bord.x.rnd", "bord.y.rnd")
        # c.value                   = "polygon.result"
        # c.leaflet.title           = "TEST"
        # n.layer                   = 8
        # n.round                   = -3
        # n.dig.lab                 = 5
        # v.info.tag.polygon.label  = c("Count")
        # v.info.veld.polygon.label = c("polygon.result")
        # v.info.tag.polygon.popup  = c("Count")
        # v.info.veld.polygon.popup = c("polygon.result")
        # b.save.leaflet            = TRUE


##############################################################################
# ERROR CHECK.
##############################################################################

##############################################################################
# INITIALIZE.
##############################################################################

##############################################################################
# PROCESS DATA.
##############################################################################

        ######################################################################
        # Enrich df.input.
        ######################################################################

        # Bereken n.step (lokaal).
        n.step.local <- median(diff(f_unique(df.input[[v.coord.point[1]]])))

        # Hernoem kolommen.
        df.input <- df.input %>%

                mutate(
                        polygon.x.mid = get(v.coord.point[1]),
                        polygon.y.mid = get(v.coord.point[2]),
                        polygon.value = get(c.value),
                        polygon.col   = polygon.value^(n.step.local/30000)
                )


        # Error check
        if(n.step.local != median(diff(f_unique(df.input$polygon.x.mid)))) {

                stop(paste0("Let op, n.step zijn verschillend in x (", n.step.local,
                     ") en y (", median(diff(f_unique(df.input$polygon.x.mid))), ") richting!"))
        }


        # Bereken breaks.
        v.seq <- seq(
                from       = min(df.input$polygon.value),
                to         = max(df.input$polygon.value),
                length.out = n.layer
        )

        v.seq[2:(length(v.seq)-1)] <- round(v.seq[2:(length(v.seq)-1)], n.round)


        # Voeg factorisatie toe van continue variabele.
        df.input <- df.input %>%

                mutate(

                        polygon.layer = cut(

                                x              = polygon.value,
                                breaks         = v.seq,
                                include.lowest = TRUE,
                                dig.lab        = n.dig.lab
                        )
                )


        ######################################################################
        # Create cell polygons.
        ######################################################################

        # Initialization.
        df.polygon.plot <- NULL
        df.polygon.temp <- NULL

        min.x <- min(df.input$polygon.x.mid) - n.step.local/2
        max.x <- max(df.input$polygon.x.mid) + n.step.local/2
        min.y <- min(df.input$polygon.y.mid) - n.step.local/2
        max.y <- max(df.input$polygon.y.mid) + n.step.local/2


        # Bouw onderste polygon rij.
        for ( x in seq(from = min.x, to = max.x - n.step.local, by = n.step.local) ) {

                # Build polygons
                df.polygon.temp <- df.polygon.temp %>%

                        rbind(

                                tibble(

                                        polygon.x     = c(x,     x + n.step.local,  x + n.step.local,     x,                    x),
                                        polygon.y     = c(min.y, min.y,             min.y + n.step.local, min.y + n.step.local, min.y),
                                        polygon.x.mid = as.integer(x + n.step.local/2),
                                        polygon.y.mid = as.integer(min.y + n.step.local/2),
                                )
                        )
        }


        # Stapel polygon rijen.
        for ( i in seq((max.y-min.y)/n.step.local) ) {

                df.polygon.plot               <- df.polygon.plot %>% rbind(df.polygon.temp)
                df.polygon.temp$polygon.y     <- df.polygon.temp$polygon.y     + n.step.local
                df.polygon.temp$polygon.y.mid <- df.polygon.temp$polygon.y.mid + n.step.local
        }


        # Voeg ID toe.
        df.polygon.plot <- df.polygon.plot %>%

                mutate(
                        polygon.id = rep(seq(nrow(.)/5), each = 5)
                ) %>%

                inner_join(
                        y  = df.input,
                        by = c("polygon.x.mid", "polygon.y.mid")
                )



##############################################################################
# PLOT LEAFLET.
##############################################################################

        df.dummy <- f_plot_leaflet(

                c.leaflet.title           = c.leaflet.title,
                c.legend.title            = "Legend",

                c.layer                   = "polygon.layer",

                n.zoom                    = 8,
                b.save.leaflet            = b.save.leaflet,

                df.polygon                = df.polygon.plot,
                v.coord.polygon           = c("polygon.x", "polygon.y"),
                c.id.polygon              = "polygon.id",

                c.fill.numeric.polygon    = "polygon.col",
                n.fill.opacity.polygon    = 0.6,

                b.show.polygon.label      = TRUE,
                v.info.tag.polygon.label  = v.info.tag.polygon.label,
                v.info.veld.polygon.label = v.info.veld.polygon.label,

                b.show.polygon.popup      = TRUE,
                v.info.tag.polygon.popup  = v.info.tag.polygon.popup,
                v.info.veld.polygon.popup = v.info.veld.polygon.popup
                )



##############################################################################
# Return data.
##############################################################################

        return()

      }
