#################################################################################
# NAME:         FUNCTION - Plot Checkerboard on Leaflet.
# AUTHOR:       Pieter Overdevest.
# DESCRIPTION:  Plot Checkerboard on Leaflet.
#################################################################################

        f_plot_checkerboard_on_leaflet = function(

                df.input,
                c.id.polygon,
                v.coord.point,
                c.value,
                c.leaflet.title           = "Title",
                n.layer                   = NULL,
                v.layer                   = NULL,
                n.round                   = 0,
                n.dig.lab                 = 3,
                v.info.tag.polygon.label  = NULL,
                v.info.veld.polygon.label = NULL,
                v.info.tag.polygon.popup  = NULL,
                v.info.veld.polygon.popup = NULL,
                b.save.leaflet            = FALSE
        ) {


##############################################################################
# TEST ONLY!
##############################################################################

        # M7 - Delivery
        # df.input                  = df.del.act.by.cell
        # c.id.polygon              = "cell.id"
        # v.coord.point             = c("bord.x.cell.mid", "bord.y.cell.mid")
        # c.value                   = "perc.delivered"
        # c.leaflet.title           = paste0("Sign delivery percentage in ", n.step/1000, "km-by-", n.step/1000, "km cells")
        # n.layer                   = NULL
        # v.layer                   = c(0, 0.001, 1, 50, 75, 90, 95, 99.999, 100)
        # n.round                   = 1
        # n.dig.lab                 = 3
        # v.info.tag.polygon.label  = v.info.tag.polygon.label.
        # v.info.veld.polygon.label = v.info.veld.polygon.label.
        # v.info.tag.polygon.popup  = v.info.tag.polygon.popup.
        # v.info.veld.polygon.popup = v.info.veld.polygon.popup.
        # b.save.leaflet            = TRUE


        # M7 - Actualisatie
        # df.input                  = df.del.act.by.cell
        # c.id.polygon              = "cell.id"
        # v.coord.point             = c("bord.x.cell.mid", "bord.y.cell.mid")
        # c.value                   = "n.bord.not.delivered.gemeente.is.delivered"
        # c.leaflet.title           = paste0("Lost sign FIDs - i.e., sign FID not delivered while municipality is - in ", n.step/1000, "km-by-", n.step/1000, "km cells")
        # n.layer                   = NULL
        # v.layer                   = c(0, 0.5, 25, 50, 100, 300, 800, 3000)
        # n.round                   = 1
        # n.dig.lab                 = 4
        # v.info.tag.polygon.label  = v.info.tag.polygon.label.
        # v.info.veld.polygon.label = v.info.veld.polygon.label.
        # v.info.tag.polygon.popup  = v.info.tag.polygon.popup.
        # v.info.veld.polygon.popup = v.info.veld.polygon.popup.
        # b.save.leaflet            = TRUE



##############################################################################
# INITIALIZE.
##############################################################################

        if(!is.null(v.layer)) {

                v.layer[1]               <- min(df.input[[c.value]], na.rm = TRUE)
                v.layer[length(v.layer)] <- max(df.input[[c.value]], na.rm = TRUE)

                # Error check
                if(!all(v.layer == sort(v.layer))) {

                        stop("Let op, v.layer moet oplopend zijn!")
                }
        }


##############################################################################
# ERROR CHECK.
##############################################################################

        if((!is.null(n.layer) & !is.null(v.layer)) | (is.null(n.layer) & is.null(v.layer))) {

                stop("Let op, alleen n.layer of v.layer moet gelijk zijn aan NULL, niet beide!")
        }


##############################################################################
# PROCESS DATA.
##############################################################################

        ######################################################################
        # Enrich df.input.
        ######################################################################

        # Bereken n.step (lokaal).
        n.step.local     <- df.input %>% pull(get(v.coord.point[1])) %>% unique() %>% sort() %>% diff() %>% median()
        n.step.local.alt <- df.input %>% pull(get(v.coord.point[2])) %>% unique() %>% sort() %>% diff() %>% median()

        # Hernoem kolommen.
        df.input <- df.input %>%

                mutate(
                        polygon.x.mid = get(v.coord.point[1]),
                        polygon.y.mid = get(v.coord.point[2]),
                        polygon.value = get(c.value),
                        polygon.col   = polygon.value^(n.step.local/30000)
                )

        # Error check
        if(n.step.local != n.step.local.alt) {

                stop(paste0("Let op, n.step zijn verschillend in x (", n.step.local,
                     ") en y (", n.step.local.alt, ") richting!"))
        }


        # Bereken breaks.
        if(!is.null(n.layer)) {

                v.seq <- seq(
                        from       = min(df.input$polygon.value, na.rm = TRUE),
                        to         = max(df.input$polygon.value, na.rm = TRUE),
                        length.out = n.layer
                )

                v.seq[2:(length(v.seq)-1)] <- round(v.seq[2:(length(v.seq)-1)], n.round)

        } else {

                v.seq <- v.layer
        }


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
                c.id.polygon              = c.id.polygon,

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

        return(df.polygon.plot)

      }
