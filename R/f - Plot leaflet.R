#' @title Plot leaflet
#'
#' @description Plots leaflet. Ik moet de beschrijving van de input parameters nog verder uitwerken
#'
#' @author Pieter Overdevest
#'
#' @param c.leaflet.title Leaflet title; also used for filename.
#' @param c.legend.title Legend title.
#' @param c.layer What feature should be used for layering? (default: NULL).
#' @param v.coord.point.midpoint Option to set the midpoint, else the center of the data will chosen (default: NULL).
#' @param n.zoom At what zoom level should leaflet be opened? (default: 13).
#' @param b.zoom.control Should we include zoom control (default: TRUE).
#' @param b.save.leaflet Should we save the leaflet? (default: FALSE).
#' @param b.save.pdf Should we save the leaflet also as pdf? (default: FALSE).
#' @param v.add.date,v.add.time Vector of booleans to specify whether date and/or time should be added.
#' @param b.add.streetsmart Should we add StreetSmart link to pop-up? (default: TRUE).
#' @param df.point POINT - (default: NULL).
#' @param v.coord.point POINT -  (default: c("bord.lon", "bord.lat")).
#' @param c.id.point POINT - (default: "bord.id").
#' @param b.show.point.label POINT - (default: TRUE).
#' @param v.info.tag.point.label POINT - (default: NULL).
#' @param v.info.veld.point.label POINT -  (default: NULL).
#' @param b.show.point.popup  POINT - (default: TRUE).
#' @param v.info.tag.point.popup POINT - (default: NULL).
#' @param v.info.veld.point.popup  POINT - (default: NULL).
#' @param df.fill.point POINT -  (default: NULL).
#' @param c.fill.factor.point POINT -  (default: NULL).
#' @param c.fill.numeric.point POINT - (default: NULL).
#' @param df.weight.point POINT -  (default: NULL).
#' @param c.weight.point POINT -  (default: NULL).
#' @param n.weight.point Point marker size, in case you want to tune alle markers - (default: NULL).
#' @param df.stroke.color.point  POINT -   (default: NULL).
#' @param c.stroke.factor.point POINT -    (default: NULL).
#' @param c.stroke.numeric.point POINT -   (default: NULL
#' @param df.stroke.weight.point POINT -   (default: NULL).
#' @param c.stroke.weight.point POINT -    (default: NULL).
#' @param n.stroke.weight.point Stroke marker size, in case you want to tune alle markers - (default: NULL).
#' @param n.opacity.fill POINT - opacity of fill (default: 0.9).
#' @param n.opacity.stroke POINT - Opacity of stroke (default: 0.9).
#' @param df.line LINE - (default: NULL).
#' @param v.coord.line LINE - (default: c("line.lon", "line.lat")).
#' @param c.id.line LINE - (default: "line.id").
#' @param b.show.line.label LINE - (default: FALSE).
#' @param v.info.tag.line.label LINE - (default: NULL).
#' @param v.info.veld.line.label LINE - (default: NULL).
#' @param b.show.line.popup LINE - (default: FALSE).
#' @param v.info.tag.line.popup LINE (default: NULL).
#' @param v.info.veld.line.popup LINE - (default: NULL).
#' @param df.color.line LINE - (default: NULL).
#' @param c.color.line LINE - (default: NULL).
#' @param df.weight.line LINE - (default: NULL).
#' @param c.weight.line LINE - (default: NULL).
#' @param n.opacity.line LINE -  (default: 1).
#' @param df.polygon POLYGON - (default: NULL).
#' @param v.coord.polygon POLYGON - (default: c("polygon.lon", "polygon.lat")).
#' @param c.id.polygon POLYGON - (default: "polygon.id").
#' @param b.show.polygon.label POLYGON - (default: TRUE).
#' @param v.info.tag.polygon.label POLYGON - (default: NULL).
#' @param v.info.veld.polygon.label POLYGON - (default: NULL).
#' @param b.show.polygon.popup POLYGON - (default: TRUE).
#' @param v.info.tag.polygon.popup POLYGON - (default: NULL).
#' @param v.info.veld.polygon.popup POLYGON - (default: NULL).
#' @param c.fill.numeric.polygon POLYGON -  (default: NULL).
#' @param n.fill.opacity.polygon POLYGON - (default: 0.3).
#' @param c.stroke.color.polygon POLYGON - (default: "black").
#' @param n.stroke.weight.polygon POLYGON - (default: 1).
#' @param b.show.stroke.polygon POLYGON - (default: TRUE).
#' @param df.text TEXT - data frame with text data (default: NULL).
#' @param v.coord.text TEXT - features containing coordinates (default: c("text.lon", "text.lat")).
#' @param c.text.label TEXT - feature containing text label (default: "text.label").
#' @param c.text.direction TEXT - Text direction (default: 'left').
#' @param c.text.color TEXT - Test color (default: 'black').
#' @param c.text.font.size TEXT - Text size (default: '25px').
#' @param n.text.opacity TEXT - Text and box opacity (default: 1).
#'
#' @returns A beautiful leaflet!
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_plot_leaflet(
#'
#'      c.leaflet.title           = "Great Leaflet",
#'      c.legend.title            = "Legend:",
#'      c.layer                   = NULL,
#'      v.coord.point.midpoint    = NULL,
#'      n.zoom                    = 13,
#'      b.save.leaflet            = FALSE,
#'      b.add.date                = TRUE,
#'      b.add.time                = TRUE,
#'      b.add.streetsmart         = TRUE,
#'      df.point                  = NULL,
#'      v.coord.point             = c("bord.lon", "bord.lat"),
#'      c.id.point                = "bord.id",
#'      b.show.point.label        = TRUE,
#'      v.info.tag.point.label    = NULL,
#'      v.info.veld.point.label   = NULL,
#'      b.show.point.popup        = TRUE,
#'      v.info.tag.point.popup    = NULL,
#'      v.info.veld.point.popup   = NULL,
#'      df.fill.point             = NULL,
#'      c.fill.factor.point       = NULL,
#'      c.fill.numeric.point      = NULL,
#'      df.weight.point           = NULL,
#'      c.weight.point            = NULL,
#'      n.weight.point            = NULL,
#'      df.stroke.color.point     = NULL,
#'      c.stroke.factor.point     = NULL,
#'      c.stroke.numeric.point    = NULL,
#'      df.stroke.weight.point    = NULL,
#'      c.stroke.weight.point     = NULL,
#'      n.stroke.weight.point     = NULL,
#'      n.opacity.fill            = 0.9,
#'      n.opacity.stroke          = 0.9,
#'      df.line                   = NULL,
#'      v.coord.line              = c("line.lon", "line.lat"),
#'      c.id.line                 = "line.id",
#'      b.show.line.label         = FALSE,
#'      v.info.tag.line.label     = NULL,
#'      v.info.veld.line.label    = NULL,
#'      b.show.line.popup         = FALSE,
#'      v.info.tag.line.popup     = NULL,
#'      v.info.veld.line.popup    = NULL,
#'      df.color.line             = NULL,
#'      c.color.line              = NULL,
#'      df.weight.line            = NULL,
#'      c.weight.line             = NULL,
#'      n.opacity.line            = 1,
#'      df.polygon                = NULL,
#'      v.coord.polygon           = c("polygon.lon", "polygon.lat"),
#'      c.id.polygon              = "polygon.id",
#'      b.show.polygon.label      = TRUE,
#'      v.info.tag.polygon.label  = NULL,
#'      v.info.veld.polygon.label = NULL,
#'      b.show.polygon.popup      = TRUE,
#'      v.info.tag.polygon.popup  = NULL,
#'      v.info.veld.polygon.popup = NULL,
#'      c.fill.numeric.polygon    = NULL,
#'      n.fill.opacity.polygon    = 0.3,
#'      c.stroke.color.polygon    = "black",
#'      n.stroke.weight.polygon   = 1,
#'      b.show.stroke.polygon     = TRUE,
#'      df.text                   = NULL,
#'      v.coord.text              = c("text.lon", "text.lat"),
#'      c.text.label              = "text.label"
#' )



        #################################################################################
        # FUNCTION.
        #################################################################################

        f_plot_leaflet <- function(

                ##############################################
                # GENERAL
                ##############################################

                c.leaflet.title         = "",
                c.legend.title          = "",
                c.layer                 = NULL,

                v.coord.point.midpoint  = NULL,
                n.zoom                  = 13,
                b.zoom.control          = TRUE,
                b.save.leaflet          = FALSE,
                b.save.pdf              = FALSE,
                n.zoom.pdf              = 4,
                b.add.date              = TRUE,
                b.add.time              = TRUE,
                b.add.streetsmart       = TRUE,


                ##############################################
                # POINTS
                ##############################################

                df.point                = NULL,
                v.coord.point           = c("bord.lon", "bord.lat"),
                c.id.point              = "bord.id",

                b.show.point.label      = TRUE,
                v.info.tag.point.label  = NULL,
                v.info.veld.point.label = NULL,

                b.show.point.popup      = TRUE,
                v.info.tag.point.popup  = NULL,
                v.info.veld.point.popup = NULL,

                df.fill.point           = NULL,
                c.fill.factor.point     = NULL,
                c.fill.numeric.point    = NULL,

                df.weight.point         = NULL,
                c.weight.point          = NULL,
                n.weight.point          = NULL,

                df.stroke.color.point   = NULL,
                c.stroke.factor.point   = NULL,
                c.stroke.numeric.point  = NULL,

                df.stroke.weight.point  = NULL,
                c.stroke.weight.point   = NULL,
                n.stroke.weight.point   = NULL,

                n.opacity.fill          = 0.9,
                n.opacity.stroke        = 0.9,


                ##############################################
                # LINES
                ##############################################

                df.line                  = NULL,
                v.coord.line             = c("line.lon", "line.lat"),
                c.id.line                = "line.id",

                b.show.line.label        = FALSE,
                v.info.tag.line.label    = NULL,
                v.info.veld.line.label   = NULL,

                b.show.line.popup        = FALSE,
                v.info.tag.line.popup    = NULL,
                v.info.veld.line.popup   = NULL,

                df.color.line            = NULL,
                c.color.line             = NULL,

                df.weight.line           = NULL,
                c.weight.line            = NULL,

                n.opacity.line           = 1,


                ##############################################
                # POLYGONS
                ##############################################

                df.polygon                = NULL,
                v.coord.polygon           = c("polygon.lon", "polygon.lat"),
                c.id.polygon              = "polygon.id",

                b.show.polygon.label      = TRUE,
                v.info.tag.polygon.label  = NULL,
                v.info.veld.polygon.label = NULL,

                b.show.polygon.popup      = TRUE,
                v.info.tag.polygon.popup  = NULL,
                v.info.veld.polygon.popup = NULL,

                c.fill.numeric.polygon    = NULL,

                n.fill.opacity.polygon    = 0.3,

                c.stroke.color.polygon    = "black",
                n.stroke.weight.polygon   = 1,

                b.show.stroke.polygon     = TRUE,


                ##############################################
                # PERMANENT TEXT LABEL.
                ##############################################

                df.text                   = NULL,
                v.coord.text              = c("text.lon", "text.lat"),
                c.text.label              = "text.label",
                c.text.direction          = 'left',
                c.text.color              = 'black',
                c.text.font.size          = '25px',
                n.text.opacity            = 1
        ) {


###############################################################################
# TESTING ONLY!!
###############################################################################

        # ##############################################
        # # GENERAL
        # ##############################################
        #
        # c.leaflet.title         = ""
        # c.legend.title          = ""
        # c.layer                 = NULL
        #
        # v.coord.point.midpoint  = NULL
        # n.zoom                  = 13
        # b.zoom.control          = TRUE
        # b.save.leaflet          = FALSE
        # b.add.date              = TRUE
        # b.add.time              = TRUE
        # b.add.streetsmart       = TRUE
        #
        #
        # ##############################################
        # # POINTS
        # ##############################################
        #
        # df.point                = NULL
        # v.coord.point           = c("bord.lon", "bord.lat")
        # c.id.point              = "bord.id"
        #
        # b.show.point.label      = TRUE
        # v.info.tag.point.label  = NULL
        # v.info.veld.point.label = NULL
        #
        # b.show.point.popup      = TRUE
        # v.info.tag.point.popup  = NULL
        # v.info.veld.point.popup = NULL
        #
        # df.fill.point           = NULL
        # c.fill.factor.point     = NULL
        # c.fill.numeric.point    = NULL
        #
        # df.weight.point         = NULL
        # c.weight.point          = NULL
        # n.weight.point          = NULL
        #
        # df.stroke.color.point   = NULL
        # c.stroke.factor.point   = NULL
        # c.stroke.numeric.point  = NULL
        #
        # df.stroke.weight.point  = NULL
        # c.stroke.weight.point   = NULL
        # n.stroke.weight.point   = NULL
        #
        # n.opacity.fill          = 0.9
        # n.opacity.stroke        = 0.9
        #
        # ##############################################
        # # LINES
        # ##############################################
        #
        # df.line                  = NULL
        # v.coord.line             = c("line.lon", "line.lat")
        # c.id.line                = "line.id"
        #
        # b.show.line.label        = FALSE
        # v.info.tag.line.label    = NULL
        # v.info.veld.line.label   = NULL
        #
        # b.show.line.popup        = FALSE
        # v.info.tag.line.popup    = NULL
        # v.info.veld.line.popup   = NULL
        #
        # df.color.line            = NULL
        # c.color.line             = NULL
        #
        # df.weight.line           = NULL
        # c.weight.line            = NULL
        #
        # n.opacity.line           = 1
        #
        #
        # ##############################################
        # # POLYGONS
        # ##############################################
        #
        # df.polygon                = NULL
        # v.coord.polygon           = c("polygon.lon", "polygon.lat")
        # c.id.polygon              = "polygon.id"
        #
        # b.show.polygon.label      = TRUE
        # v.info.tag.polygon.label  = NULL
        # v.info.veld.polygon.label = NULL
        #
        # b.show.polygon.popup      = TRUE
        # v.info.tag.polygon.popup  = NULL
        # v.info.veld.polygon.popup = NULL
        #
        # c.fill.numeric.polygon    = NULL
        #
        # n.fill.opacity.polygon    = 0.3
        #
        # c.stroke.color.polygon    = "black"
        # n.stroke.weight.polygon   = 1
        #
        # b.show.stroke.polygon     = TRUE
        #
        #
        # ##############################################
        # # PERMANENT TEXT LABEL.
        # ##############################################
        #
        # df.text                   = NULL
        # v.coord.text              = c("text.lon", "text.lat")
        # c.text.label              = "text.label"
        # c.text.direction          = 'left'
        # c.text.color              = 'black'
        # c.text.font.size          = '25px'
        # n.text.opacity            = 1



        #######################################################################
        # DEPENDING ON SCRIPT REQUESTING USE OF FUNCTION:
        #######################################################################

        # f_find_points_within_polygons
        # df.point                = df.point.plot
        # c.fill.factor.point     = "point.polygon"
        # df.polygon              = df.polygon.plot

        # ZONERING
        # c.leaflet.title         = "Analyse ISA proofing"
        # c.legend.title          = "Type"
        #
        # v.coord.point.midpoint  = c(df.boundary$lon[1], df.boundary$lat[1])
        # n.zoom                  = 18
        # b.save.leaflet          = FALSE
        #
        # df.point                = df.point.plot
        # v.coord.point           = c("point.lon", "point.lat")
        # c.id.point              = "point.id"
        #
        # df.fill.point           = df.fill.point.
        # c.fill.factor.point     = "point.type"
        #
        # df.weight.point         = df.weight.point.
        # c.weight.point          = "point.type"
        #
        #
        # v.info.tag.point.label  = v.tag.label
        # v.info.veld.point.label = v.veld.label
        # v.info.tag.point.popup  = v.tag.popup
        # v.info.veld.point.popup = v.veld.popup
        #
        # df.line                 = df.line.plot
        # v.coord.line            = c("lon", "lat")
        # c.id.line               = "id"
        #
        # df.color.line           = df.color.line.
        # c.color.line            = "line.type"
        #
        # df.weight.line          = df.weight.line.
        # c.weight.line           = "line.type"
        #
        # df.polygon              = df.buffer
        # v.coord.polygon         = c("buffer.lon", "buffer.lat")
        # c.id.polygon            = "wegsegment.id"


        # Plot Checkerboard
        # c.leaflet.title           = c.leaflet.title
        # c.legend.title            = "Legend"
        # c.layer                   = "polygon.layer"
        # n.zoom                    = 8
        # b.save.leaflet            = b.save.leaflet
        # df.polygon                = df.polygon.plot
        # v.coord.polygon           = c("polygon.x", "polygon.y")
        # c.id.polygon              = "polygon.id"
        # c.fill.numeric.polygon    = "polygon.col"
        # n.fill.opacity.polygon    = 0.6
        # b.show.polygon.label      = TRUE
        # v.info.tag.polygon.label  = v.info.tag.polygon.label
        # v.info.veld.polygon.label = v.info.veld.polygon.label
        # b.show.polygon.popup      = TRUE
        # v.info.tag.polygon.popup  = v.info.tag.polygon.popup
        # v.info.veld.polygon.popup = v.info.veld.polygon.popup


        # ISA
        # c.leaflet.title         = "Analyse ISA proofing"
        # c.legend.title          = "Type"
        # v.coord.point.midpoint  = c(df.boundary$lon[1], df.boundary$lat[1])
        # n.zoom                  = 17
        # b.save.leaflet          = TRUE
        # b.add.date              = FALSE
        # b.add.time              = FALSE
        # df.point                = df.point.plot
        # v.coord.point           = c("point.lon", "point.lat")
        # c.id.point              = "point.id"
        # df.fill.point           = df.fill.point.
        # c.fill.factor.point     = "point.type"
        # df.weight.point         = df.weight.point.
        # c.weight.point          = "point.type"
        # df.stroke.color.point   = df.stroke.color.point.
        # c.stroke.factor.point   = "stroke.type"
        # df.stroke.weight.point  = df.stroke.weight.point.
        # c.stroke.weight.point   = "stroke.type"
        # n.opacity.fill          = 1
        # n.opacity.stroke        = 1
        # v.info.tag.point.label  = v.tag.label
        # v.info.veld.point.label = v.veld.label
        # v.info.tag.point.popup  = v.tag.popup
        # v.info.veld.point.popup = v.veld.popup
        # df.line                 = df.line.plot
        # v.coord.line            = c("lon", "lat")
        # c.id.line               = "line.id"
        # df.color.line           = df.color.line.
        # c.color.line            = "line.type"
        # df.weight.line          = df.weight.line.
        # c.weight.line           = "line.type"
        # n.opacity.line          = 1
        # df.text                 = df.leak.cluster.id
        # v.coord.text            = c("center.x", "center.y")
        # c.text.label            = "leak.cluster.id"

        # SCHOOLZONE
        # c.leaflet.title         = "Voorbeelden Schoolzones Rotterdam"
        # c.legend.title          = "Categorie"
        # c.layer                 = "point.type"
        # v.coord.point.midpoint  = c(mean(df.point.plot$point.lon), mean(df.point.plot$point.lat))
        # n.zoom                  = 16
        # b.save.leaflet          = FALSE
        # df.point                = df.point.plot
        # v.coord.point           = c("point.lon", "point.lat")
        # c.id.point              = "point.id"
        # c.fill.factor.point     = "point.type"
        # c.weight.point          = "point.type"
        # df.fill.point           = df.fill.point.
        # df.weight.point         = df.weight.point.
        # v.info.tag.point.popup  = v.info.tag
        # v.info.veld.point.popup = v.info.veld
        # df.line                 = df.line.plot
        # c.color.line            = "line.type"
        # c.weight.line           = "line.type"
        # df.color.line           = df.color.line.
        # df.weight.line          = df.weight.line.
        # n.opacity.line          = 0.7
        # df.polygon              = df.polygon.plot
        # n.fill.opacity.polygon  = 0.15
        # b.show.polygon.label    = FALSE
        # b.show.polygon.popup    = FALSE


##############################################################################
# ERROR CHECKS
##############################################################################

        ######################################################################
        # POINTS
        ######################################################################

        # Check in het geval df.point ongelijk is aan NULL.
        if(!is.null(df.point)) {

                # Check aanwezigheid van kolommen.
                if(!all(v.coord.point %in% names(df.point))) {

                        stop("De velden - ", f_paste(v.coord.point), " (v.coord.point) - moeten aanwezig zijn in df.point!")
                }


                # Check appendix v.coord.point.
                if(!any(grepl("x$|lon$", v.coord.point[1]))) {

                        stop("Het veld - '", v.coord.point[1], "' (v.coord.point[1]) - moeten eindigen met '.x' of '.lon'!")
                }

                if(!any(grepl("y$|lat$", v.coord.point[2]))) {

                        stop("Het veld - '", v.coord.point[2], "' (v.coord.point[2]) - moeten eindigen met '.y' of '.lat'!")
                }


                # Check that c.id.point is in names df.point.
                if(!c.id.point %in% names(df.point)) {

                        stop("Het veld - ", c.id.point, " (c.id.point) - moeten aanwezig zijn in df.point!")
                }

                if(!is.null(c.fill.factor.point)) {

                        if(!c.fill.factor.point %in% names(df.point)) {

                                stop("Het veld - ", c.fill.factor.point, " (c.fill.factor.point) - moeten aanwezig zijn in df.point!")
                        }
                }

                if(!is.null(c.fill.numeric.point)) {

                        if(!c.fill.numeric.point %in% names(df.point)) {

                                stop("Het veld - ", c.fill.numeric.point, " (c.fill.numeric.point) - moeten aanwezig zijn in df.point!")
                        }
                }

                if(!is.null(c.weight.point)) {

                        if(!c.weight.point %in% names(df.point)) {

                                stop("Het veld - ", c.weight.point, " (c.weight.point) - moeten aanwezig zijn in df.point!")
                        }
                }

                if(!is.null(c.stroke.factor.point)) {

                        if(!c.stroke.factor.point %in% names(df.point)) {

                                stop("Het veld - ", c.stroke.factor.point, " (c.stroke.factor.point) - moeten aanwezig zijn in df.point!")
                        }
                }

                if(!is.null(c.stroke.numeric.point)) {

                        if(!c.stroke.numeric.point %in% names(df.point)) {

                                stop("Het veld - ", c.stroke.numeric.point, " (c.stroke.numeric.point) - moeten aanwezig zijn in df.point!")
                        }
                }

                if(!is.null(c.stroke.weight.point)) {

                        if(!c.stroke.weight.point %in% names(df.point)) {

                                stop("Het veld - ", c.stroke.weight.point, " (c.stroke.weight.point) - moeten aanwezig zijn in df.point!")
                        }
                }


                # Check v.info popup.
                if(!is.null(v.info.tag.point.popup) & !is.null(v.info.veld.point.popup) & length(v.info.tag.point.popup) != length(v.info.veld.point.popup)) {

                        stop("Let op, v.info.tag.point.popup en v.info.veld.point.popup moeten dezelfde lengte hebben!")
                }

                # Check v.info label.
                if(!is.null(v.info.tag.point.label) & !is.null(v.info.veld.point.label) & length(v.info.tag.point.label) != length(v.info.veld.point.label)) {

                        stop("Let op, v.info.tag.point.label en v.info.veld.point.label moeten dezelfde lengte hebben!")
                }

                # Check c.fill.factor.point en c.fill.numeric.point
                if(!is.null(c.fill.factor.point) & !is.null(c.fill.numeric.point)) {

                        stop("Let op, c.fill.factor.point en c.fill.numeric.point mogen niet beide een waarde hebben!")
                }

                # Check c.stroke.factor.point en c.stroke.numeric.point
                if(!is.null(c.stroke.factor.point) & !is.null(c.stroke.numeric.point)) {

                        stop("Let op, c.stroke.factor.point en c.stroke.numeric.point mogen niet beide een waarde hebben!")
                }



                # Check number of columns
                if(!is.null(df.fill.point)) {

                        if(ncol(df.fill.point) != 2)
                                stop("Let op, df.fill.point moet twee kolommen bevatten!")
                }

                if(!is.null(df.weight.point)) {

                        if(ncol(df.weight.point) != 2)
                                stop("Let op, df.weight.point moet twee kolommen bevatten!")
                }

                if(!is.null(df.stroke.color.point)) {

                        if(ncol(df.stroke.color.point) != 2)
                                stop("Let op, df.stroke.color.point moet twee kolommen bevatten!")
                }

                if(!is.null(df.stroke.weight.point)) {

                        if(ncol(df.stroke.weight.point) != 2)
                                stop("Let op, df.stroke.weight.point moet twee kolommen bevatten!")
                }


                # Check df.fill.point.
                if(!is.null(c.fill.factor.point)) {

                        # Aantal levels van fill factor in df.point.
                        n.level.fill.factor <- length(unique(pull(df.point, c.fill.factor.point)))

                        if(n.level.fill.factor == 2) {

                                warning(
                                        "Als er maar 2 levels zijn in c.fill.color, dan komt er een warning van color brewer. Deze kun je negeren."
                                )
                        }

                        # Check df.fill.point.
                        if(!is.null(df.fill.point)) {

                                if(nrow(df.fill.point) != n.level.fill.factor) {

                                        stop(
                                                "Let op, df.fill.point moet net zo veel rijen hebben als er unieke waarden zijn in ",
                                                c.fill.factor.point, ", nl. ", n.level.fill.factor, "!"
                                        )
                                }
                        }
                }


                # Check df.weight.point.
                if(!is.null(c.weight.point)) {

                        # Aantal levels van stroke weight in df.point.
                        n.level.weight <- length(unique(pull(df.point, c.weight.point)))

                        # Check df.weight.point.
                        if(!is.null(df.weight.point)) {

                                if(nrow(df.weight.point) != n.level.weight) {

                                        stop(
                                                "Let op, df.weight.point moet net zo veel rijen hebben als er unieke waarden zijn in ",
                                                c.weight.point, ", nl. ", n.level.weight, "!")
                                }

                        } else {

                                stop("Let op, als c.weight.point is gegeven, moet ook df.weight.point zijn gegeven!")
                        }

                } else if(!is.null(df.weight.point)) {

                        stop("Let op, als df.weight.point is gegeven, moet ook c.weight.point zijn gegeven!")
                }


                # Check n.weight.point.
                if(!is.null(df.weight.point) & !is.null(c.weight.point) & !is.null(n.weight.point)) {

                        stop(
                                "Let op, als df.weight.point en c.weight.point zijn gegeven, moet n.weight.point NULL zijn!")
                }



                # Check df.stroke.color.point.
                if(!is.null(c.stroke.factor.point)) {

                        # Aantal levels van stroke factor in df.point.
                        n.level.stroke.factor <- length(unique(pull(df.point, c.stroke.factor.point)))

                        if(n.level.stroke.factor == 2) {

                                warning(
                                        "Als er maar 2 levels zijn in c.stroke.color, dan komt er een warning van color brewer. Deze kun je negeren."
                                )
                        }

                        # Check df.stroke.color.point.
                        if(!is.null(df.stroke.color.point)) {

                                if(nrow(df.stroke.color.point) != n.level.stroke.factor) {

                                        stop(
                                                "Let op, df.stroke.color.point moet net zo veel rijen hebben als er unieke waarden zijn in ",
                                                c.stroke.factor.point, ", nl. ", n.level.stroke.factor, "!")
                                }
                        }
                }



                # Check df.stroke.weight.point.
                if(!is.null(c.stroke.weight.point)) {

                        # Aantal levels van stroke weight in df.point.
                        n.level.stroke.weight <- length(unique(pull(df.point, c.stroke.weight.point)))

                        # Check df.stroke.weight.point.
                        if(!is.null(df.stroke.weight.point)) {

                                if(nrow(df.stroke.weight.point) != n.level.stroke.weight) {

                                        stop(
                                                "Let op, df.stroke.weight.point moet net zo veel rijen hebben als er unieke waarden zijn in ",
                                                c.stroke.weight.point, ", nl. ", n.level.stroke.weight, "!")
                                }

                        } else {

                                stop("Let op, als c.stroke.weight.point is gegeven, moet ook df.stroke.weight.point zijn gegeven!")

                        }

                } else if(!is.null(df.stroke.weight.point)) {

                        stop("Let op, als df.stroke.weight.point is gegeven, moet ook c.stroke.weight.point zijn gegeven!")
                }


                # Check n.stroke.weight.point.
                if(!is.null(df.stroke.weight.point) & !is.null(c.stroke.weight.point) & !is.null(n.stroke.weight.point)) {

                        stop(
                                "Let op, als df.stroke.weight.point en c.stroke.weight.point zijn gegeven, moet n.stroke.weight.point NULL zijn!")
                }
        }


        ######################################################################
        # LINES
        ######################################################################

        # Check in het geval df.line ongelijk is aan NULL.
        if(!is.null(df.line)) {

                # Check appendix v.coord.line.
                if(!any(grepl("x$|lon$", v.coord.line[1]))) {

                        stop("Het veld - '", v.coord.line[1], "' (v.coord.line[1]) - moeten eindigen met '.x' of '.lon'!")
                }

                if(!any(grepl("y$|lat$", v.coord.line[2]))) {

                        stop("Het veld - '", v.coord.line[2], "' (v.coord.line[2]) - moeten eindigen met '.y' of '.lat'!")
                }


                # Check number of columns
                if(!is.null(df.color.line)) {

                        if(ncol(df.color.line) != 2)
                                stop("Let op, df.color.line moet twee kolommen bevatten!")
                }

                if(!is.null(df.weight.line)) {

                        if(ncol(df.weight.line) != 2)
                                stop("Let op, df.weight.line moet twee kolommen bevatten!")
                }



                # Check df.color.line.
                if(!is.null(c.color.line)) {

                        # Aantal levels van fill factor in df.line.
                        n.level.stroke.color.line <- length(unique(pull(df.line, c.color.line)))

                        if(n.level.stroke.color.line == 2) {

                                warning(
                                        "Als er maar 2 levels zijn in c.color.line, dan komt er een warning van color brewer. Deze kun je negeren."
                                )
                        }

                        # Check df.color.line.
                        if(!is.null(df.color.line)) {

                                if(nrow(df.color.line) != n.level.stroke.color.line) {

                                        stop(
                                                "Let op, v.color.line moet net zo veel waarden hebben als er unieke waarden zijn in ",
                                                c.color.line, ", nl. ", n.level.stroke.color.line, "!"
                                        )
                                }
                        }
                }

                # Check df.weight.line.
                if(!is.null(c.weight.line)) {

                        # Aantal levels van fill factor in df.line.
                        n.level.stroke.weight.line <- length(unique(pull(df.line, c.weight.line)))

                        # Check df.weight.line.
                        if(!is.null(df.weight.line)) {

                                if(nrow(df.weight.line) != n.level.stroke.weight.line) {

                                        stop(
                                                "Let op, v.weight.line moet net zo veel waarden hebben als er unieke waarden zijn in ",
                                                c.weight.line, ", nl. ", n.level.stroke.weight.line, "!"
                                        )
                                }
                        }
                }
        }


        ######################################################################
        # POLYGONS
        ######################################################################

        if(!is.null(df.polygon)) {

                # Check appendix v.coord.polygon.
                if(!any(grepl("x$|lon$", v.coord.polygon[1]))) {

                        stop("Het veld - '", v.coord.polygon[1], "' (v.coord.polygon[1]) - moeten eindigen met '.x' of '.lon'!")
                }

                if(!any(grepl("y$|lat$", v.coord.polygon[2]))) {

                        stop("Het veld - '", v.coord.polygon[2], "' (v.coord.polygon[2]) - moeten eindigen met '.y' of '.lat'!")
                }
        }


        ######################################################################
        # LAYERS
        ######################################################################

        # Check c.layer in df.point, df.line, en df.polygon.
        if(!is.null(c.layer)) {

                if(c.layer %in% names(df.point) + c.layer %in% names(df.line) + c.layer %in% names(df.polygon) == 0) {

                        stop("Let op, '", c.layer, "' is niet aanwezig in df.point, df.line, of df.polygon!")
                }

                if(c.layer %in% names(df.point) + c.layer %in% names(df.line) + c.layer %in% names(df.polygon) > 1) {

                        stop("Let op, '", c.layer, "' is aanwezig in meer dan 1 van df.point, df.line, of df.polygon. ",
                             "Dit attribuut mag maar in 1 van de drie data frames aanwezig zijn!")
                }
        }



        ######################################################################
        # PERMANENT TEXT LABEL
        ######################################################################

        if(!is.null(df.text)) {

                # Check appendix v.coord.text.
                if(!any(grepl("x$|lon$", v.coord.text[1]))) {

                        stop("Het veld - '", v.coord.text[1], "' (v.coord.text[1]) - moeten eindigen met '.x' of '.lon'!")
                }

                if(!any(grepl("y$|lat$", v.coord.text[2]))) {

                        stop("Het veld - '", v.coord.text[2], "' (v.coord.text[2]) - moeten eindigen met '.y' of '.lat'!")
                }


                if(!all(v.coord.text %in% names(df.text))) {

                        stop("Let op, ", f_paste(v.coord.text, c.and = "en", b.quotation = TRUE), " zijn niet aanwezig in df.text!")
                }

                if(!c.text.label %in% names(df.text)) {

                        stop("Let op, '", c.text.label, "' is niet aanwezig in df.text!")
                }
        }


##############################################################################
# INITIALIZATION.
##############################################################################

        ######################################################################
        # POINTS
        ######################################################################

        if(!is.null(df.point)) {

                # Rename headers of formatting data frames.
                if(!is.null(df.fill.point)) {

                        names(df.fill.point)         <- c("fill.color.point.label", "fill.color.point.value")
                }

                if(!is.null(df.weight.point)) {

                        names(df.weight.point)       <- c("weight.point.label", "weight.point.value")
                }

                if(!is.null(df.stroke.color.point)) {

                        names(df.stroke.color.point) <- c("stroke.color.point.label", "stroke.color.point.value")
                }

                if(!is.null(df.stroke.weight.point)) {

                        names(df.stroke.weight.point) <- c("stroke.weight.point.label", "stroke.weight.point.value")
                }


                # Bepaal POINT FILL COLOR indien deze niet gegeven is.
                if(is.null(df.fill.point) & !is.null(c.fill.factor.point)) {

                        df.fill.point <- data.frame(

                                fill.color.point.label = unique(pull(df.point, c.fill.factor.point)),
                                fill.color.point.value = brewer.pal(n.level.fill.factor, "Dark2")[seq(n.level.fill.factor)]
                        )
                }

                # # Bepaal POINT WEIGHT indien deze niet gegeven is.
                # if(is.null(df.weight.point) & !is.null(c.weight.point)) {
                #
                #         df.weight.point <- data.frame(
                #
                #                 weight.point.label = unique(pull(df.point, c.weight.point)),
                #                 weight.point.value = 4
                #         )
                # }

                # Bepaal POINT STROKE COLOR indien deze niet gegeven is.
                if(is.null(df.stroke.color.point) & !is.null(c.stroke.factor.point)) {

                        df.stroke.color.point <- data.frame(

                                stroke.color.point.label = unique(pull(df.point, c.stroke.factor.point)),
                                stroke.color.point.value = brewer.pal(n.level.stroke.factor, "Dark2")[seq(n.level.stroke.factor)]
                        )
                }

                # # Bepaal POINT STROKE WEIGHT indien deze niet gegeven is. Beide zijn NULL of niet NULL, dus 1 checken is voldoende.
                # # Maar beide checken doet geen pijn.
                # if(!is.null(df.stroke.weight.point) & !is.null(c.stroke.weight.point)) {
                #
                #         df.stroke.weight.point <- data.frame(
                #
                #                 stroke.weight.point.label = unique(pull(df.point, c.stroke.weight.point)),
                #                 stroke.weight.point.value = 4
                #         )
                # }


                # Verwijder dubbele uit df.point. Met name relevant als we de punten in een polygoon willen plotten
                # en dan plotten we begin en uitpunt (zelfde).
                df.point <- df.point %>% distinct()


                # Define POINT FILL field.
                if(!is.null(c.fill.factor.point)) {

                        df.point <- df.point %>% mutate(fill.point.label = as.factor(get(c.fill.factor.point)))

                } else if(!is.null(c.fill.numeric.point)) {

                        df.point <- df.point %>% mutate(fill.point.label = as.numeric(get(c.fill.numeric.point)))

                } else {

                        df.point$fill.point.label <- NA
                }

                # Define POINT WEIGHT field.
                if(!is.null(c.weight.point)) {

                        df.point <- df.point %>% mutate(weight.point.label = as.factor(get(c.weight.point)))

                } else {

                        df.point$weight.point.label <- NA
                }

                # Define STROKE COLOR field.
                if(!is.null(c.stroke.factor.point)) {

                        df.point <- df.point %>% mutate(stroke.color.point.label = as.factor(get(c.stroke.factor.point)))

                } else if(!is.null(c.stroke.numeric.point)) {

                        df.point <- df.point %>% mutate(stroke.color.point.label = as.numeric(get(c.stroke.numeric.point)))

                } else {

                        df.point$stroke.color.point.label <- NA
                }

                # Define STROKE WEIGHT field.
                if(!is.null(c.stroke.weight.point)) {

                        df.point <- df.point %>% mutate(stroke.weight.point.label = as.factor(get(c.stroke.weight.point)))

                } else {

                        df.point$stroke.weight.point.label <- NA
                }

        }


        ######################################################################
        # LINES
        ######################################################################

        if(!is.null(df.line)) {

                # Rename headers of formatting data frames.
                if(!is.null(df.color.line)) {

                        names(df.color.line)  <- c("color.line.label", "color.line.value")
                }

                if(!is.null(df.weight.line)) {

                        names(df.weight.line) <- c("weight.line.label", "weight.line.value")
                }



                # Bepaal LINE COLOR indien deze niet gegeven is.
                if(is.null(df.color.line) & !is.null(c.color.line)) {

                        df.color.line <- data.frame(

                                color.line.label = unique(pull(df.line, c.color.line)),
                                color.line.value = brewer.pal(n.level.stroke.color.line, "Dark2")[seq(n.level.stroke.color.line)]
                        )
                }

                # Bepaal LINE WEIGHT indien deze niet gegeven is.
                # if(is.null(df.weight.line) & !is.null(c.weight.line)) {
                #
                #         df.weight.line <- data.frame(
                #
                #                 weight.line.label = unique(pull(df.line, c.weight.line)),
                #                 weight.line.value = 4
                #         )
                # }
                #

                # Define LINE color field.
                if(!is.null(c.color.line)) {

                        df.line <- df.line %>% mutate(color.line.label = as.factor(get(c.color.line)))
                }

                # Define LINE weight field.
                if(!is.null(c.weight.line)) {

                        df.line <- df.line %>% mutate(weight.line.label = as.factor(get(c.weight.line)))
                }
        }


        ######################################################################
        # POLYGONS
        ######################################################################

        if(!is.null(df.polygon)) {

                # Define POLYGON color field.
                if(!is.null(c.fill.numeric.polygon)) {

                        df.polygon <- df.polygon %>% mutate(polygon.fill = as.numeric(get(c.fill.numeric.polygon)))

                } else {

                        df.polygon$polygon.fill <- NA
                }
        }


##############################################################################
# PREPROCESSING
##############################################################################

        ######################################################################
        # POINTS
        ######################################################################

        if(!is.null(df.point)) {

                # CONVERSIE COORDINAAT LABELS.
                c.coord.point.x        <- gsub("lon$", "x", v.coord.point[1])
                c.coord.point.y        <- gsub("lat$", "y", v.coord.point[2])
                c.coord.point.lon      <- gsub("x$", "lon", v.coord.point[1])
                c.coord.point.lat      <- gsub("y$", "lat", v.coord.point[2])
                v.coord.point.xy       <- c(c.coord.point.x,   c.coord.point.y)
                v.coord.point.lonlat   <- c(c.coord.point.lon, c.coord.point.lat)


                # Voeg point.x en point.y toe.
                if( all(v.coord.point.xy %in% names(df.point)) ) {

                        df.point$point.x <- as.numeric(df.point[[c.coord.point.x]])
                        df.point$point.y <- as.numeric(df.point[[c.coord.point.y]])

                        # Voeg point.lon en point.lat toe.
                        if( all(v.coord.point.lonlat %in% names(df.point)) ) {

                                df.point$point.lat <- as.numeric(df.point[[c.coord.point.lat]])
                                df.point$point.lon <- as.numeric(df.point[[c.coord.point.lon]])

                        } else {

                                df.point <- df.point %>%

                                        mutate(point.lon = f_rd_to_wgs84_longitude(point.x, point.y),
                                               point.lat = f_rd_to_wgs84_latitude(point.x, point.y)
                                        )
                        }

                } else {

                        # Voeg point.x en point.y toe.
                        if( all(v.coord.point.lonlat %in% names(df.point)) ) {

                                df.point$point.lat <- as.numeric(df.point[[c.coord.point.lat]])
                                df.point$point.lon <- as.numeric(df.point[[c.coord.point.lon]])

                                df.point <- df.point %>%

                                        mutate(point.x = f_wgs84_to_rd_x(point.lat, point.lon),
                                               point.y = f_wgs84_to_rd_y(point.lat, point.lon)
                                        )

                        } else {

                                stop("No coordinate features available in df.point!")
                        }
                }



                # Hernoem SCHOUWRICHTING kolom.
                if("schouwrichting" %in% names(df.point)) {

                        warning("Het veld schouwrichting is hernoemd naar bord.schouw.")

                        df.point <- rename(df.point, bord.schouw = schouwrichting) }


                # Voeg ghost.x en ghost.y toe als ze niet in de data zitten.
                if ( !all(c("ghost.x", "ghost.y") %in% names(df.point)) ) {

                        if( "bord.schouw" %in% names(df.point) ) {

                                warning("De velden ghost.x en ghost.y zijn niet aanwezig in de point data, en worden toegevoegd.")

                                df.point <- f_add_ghost_xy(

                                        df       = df.point,
                                        c.x      = "point.x",
                                        c.y      = "point.y",
                                        n.r      = 5,
                                        c.schouw = "bord.schouw")
                        } else {

                                df.point <- df.point %>%

                                        mutate(
                                                ghost.x = point.x,
                                                ghost.y = point.y
                                        )
                        }
                }



                # Hernoem URL kolom en vul lege velden. Voeg kolom toe als bord.url niet aanwezig is.
                if("bord.url" %in% names(df.point)) {

                        df.point <- df.point %>%

                                rename(url = bord.url) %>%

                                mutate(
                                        url = ifelse(

                                                is.na(url),

                                                paste0(
                                                        "https://streetsmart.cyclomedia.com/streetsmart?q=",
                                                        df.point$ghost.x, ";", df.point$ghost.y,
                                                        ";EPSG:28992"
                                                ),

                                                url
                                        )
                                )
                } else {

                        df.point <- df.point %>%

                                mutate(
                                        url = paste0(
                                                        "https://streetsmart.cyclomedia.com/streetsmart?q=",
                                                        df.point$ghost.x, ";", df.point$ghost.y,
                                                        ";EPSG:28992"
                                                        )
                                        )
                }


                # Define POINT weight.
                if(!is.null(df.weight.point)) {

                        df.point <- df.point %>%

                            left_join(
                                    y  = df.weight.point,
                                    by = "weight.point.label"
                            )

                        # Error check:
                        if(sum(is.na(df.point$weight.point.value)) > 0) {

                                stop("Niet voor alle punten in df.point is een weight.point.value gevonden:")

                                print(df.point %>% filter(is.na(weight.point.value)))
                        }

                } else {

                        df.point <- df.point %>%

                                mutate(
                                        weight.point.value = ifelse(
                                                is.null(n.weight.point),
                                                4,
                                                n.weight.point
                                        )
                                )
                }


                # Define STROKE weight.
                if(!is.null(df.stroke.weight.point)) {

                        df.point <- df.point %>%

                                left_join(
                                        y  = df.stroke.weight.point,
                                        by = "stroke.weight.point.label"
                                )

                        # Error check:
                        if(sum(is.na(df.point$stroke.weight.point.value)) > 0) {

                                stop("Niet voor alle punten in df.point is een stroke.weight.point.value gevonden:")

                                print(df.point %>% filter(is.na(stroke.weight.point.value)))
                        }

                } else {

                        df.point <- df.point %>%

                                mutate(
                                        stroke.weight.point.value = ifelse(
                                                is.null(n.stroke.weight.point),
                                                2,
                                                n.stroke.weight.point
                                        )
                                )
                }
        }


        ######################################################################
        # LINES
        ######################################################################

        if(!is.null(df.line)) {

                # CONVERSIE COORDINAAT LABELS.
                c.coord.line.x        <- gsub("lon$", "x", v.coord.line[1])
                c.coord.line.y        <- gsub("lat$", "y", v.coord.line[2])
                c.coord.line.lon      <- gsub("x$", "lon", v.coord.line[1])
                c.coord.line.lat      <- gsub("y$", "lat", v.coord.line[2])
                v.coord.line.xy       <- c(c.coord.line.x,   c.coord.line.y)
                v.coord.line.lonlat   <- c(c.coord.line.lon, c.coord.line.lat)


                # Voeg line.x en line.y toe.
                if( all(v.coord.line.xy %in% names(df.line)) ) {

                        df.line$line.x <- as.numeric(df.line[[c.coord.line.x]])
                        df.line$line.y <- as.numeric(df.line[[c.coord.line.y]])

                        # Voeg line.lon en line.lat toe.
                        if( all(v.coord.line.lonlat %in% names(df.line)) ) {

                                df.line$line.lat <- as.numeric(df.line[[c.coord.line.lat]])
                                df.line$line.lon <- as.numeric(df.line[[c.coord.line.lon]])

                        } else {

                                df.line <- df.line %>%

                                        mutate(line.lon = f_rd_to_wgs84_longitude(line.x, line.y),
                                               line.lat = f_rd_to_wgs84_latitude(line.x, line.y)
                                        )
                        }

                } else {

                        # Voeg line.x en line.y toe.
                        if( all(v.coord.line.lonlat %in% names(df.line)) ) {

                                df.line$line.lat <- as.numeric(df.line[[c.coord.line.lat]])
                                df.line$line.lon <- as.numeric(df.line[[c.coord.line.lon]])

                                df.line <- df.line %>%

                                        mutate(line.x = f_wgs84_to_rd_x(line.lat, line.lon),
                                               line.y = f_wgs84_to_rd_y(line.lat, line.lon)
                                        )

                        } else {

                                stop("NNo coordinate features available in df.line!")
                        }
                }



                ######################################################################
                # LABELS & POP-UPS
                ######################################################################

                # SEED:
                v.line.label <- NULL
                v.line.popup <- "<b><u>OBJECT-INFORMATIE:</b></u>"


                # LABEL:
                if(b.show.line.label) {

                        v.line.label <- paste0(v.line.label, "Line ID ", df.line %>% pull(c.id.line))

                        # Voeg additionele informatie toe indien gegeven.
                        if(!is.null(v.info.tag.line.label)) {

                                # Voeg additionele informatie toe:
                                for (i in seq_along(v.info.tag.line.label)) { # i = 1

                                        v.line.label <- paste0(

                                                v.line.label, " / ",
                                                v.info.tag.line.label[i], " ",
                                                pull(df.line, v.info.veld.line.label[i])
                                        )
                                }
                        }

                } else {

                        v.line.label <- NA

                }


                # POP-UP:
                if(b.show.line.popup) {

                        # Voeg ID toe:
                        v.line.popup <- paste0(v.line.popup,
                                                "<br><br><b><u>Line ID:</b></u><br>",
                                                df.line %>% pull(c.id.line))


                        # Voeg additionele informatie toe.
                        if(!is.null(v.info.tag.line.popup)) {

                                v.line.popup <- paste0(v.line.popup,
                                                          "<br><br><b><u>Additionele informatie:</b></u>")

                                # Voeg additionele informatie toe:
                                for (i in seq_along(v.info.tag.line.popup)) { # i = 1

                                        v.line.popup <- paste0(

                                                v.line.popup, "<br>",
                                                v.info.tag.line.popup[i], ": ",
                                                pull(df.line, v.info.veld.line.popup[i])
                                        )
                                }
                        }


                } else {

                        v.line.popup <- NA

                }



                # Define LINE color.
                if(!is.null(df.color.line)) {

                    df.line <- df.line %>%

                            left_join(

                                    y  = df.color.line,
                                    by = "color.line.label"
                            )


                    # Error check:
                    if(sum(is.na(df.line$color.line.value))>0) {

                            stop("Niet voor alle punten in df.line is een color.line.value gevonden:")

                            print(df.line %>% filter(is.na(color.line.value)))
                    }

                } else {

                    df.line <- df.line %>%

                            mutate(color.line.value = "blue")
                }



                # Define LINE weight.
                if(!is.null(df.weight.line)) {

                    df.line <- df.line %>%

                            left_join(
                                    y  = df.weight.line,
                                    by = "weight.line.label"
                             )

                    # Error check:
                    if(sum(is.na(df.line$weight.line.value)) > 0) {

                            stop("Niet voor alle punten in df.line is een weight.line.value gevonden:")

                            print(df.line %>% filter(is.na(weight.line.value)))
                    }

                } else {

                    df.line <- df.line %>%

                            mutate(weight.line.value = 2)
                }
        }


        ######################################################################
        # POLYGONS
        ######################################################################

        if(!is.null(df.polygon)) {

                # CONVERSIE COORDINAAT LABELS.
                c.coord.polygon.x      <- gsub("lon$", "x", v.coord.polygon[1])
                c.coord.polygon.y      <- gsub("lat$", "y", v.coord.polygon[2])
                c.coord.polygon.lon    <- gsub("x$", "lon", v.coord.polygon[1])
                c.coord.polygon.lat    <- gsub("y$", "lat", v.coord.polygon[2])
                v.coord.polygon.xy     <- c(c.coord.polygon.x,   c.coord.polygon.y)
                v.coord.polygon.lonlat <- c(c.coord.polygon.lon, c.coord.polygon.lat)


                # Hernoem x,y naar polygon.x en polygon.y, en voeg polygon.lon en polygon.lat toe.
                if( all(v.coord.polygon.xy %in% names(df.polygon)) ) {

                        df.polygon$polygon.x <- as.numeric(df.polygon[[c.coord.polygon.x]])
                        df.polygon$polygon.y <- as.numeric(df.polygon[[c.coord.polygon.y]])

                        # Voeg lon en lat toe.
                        df.polygon <- df.polygon %>%

                                mutate(polygon.lon = f_rd_to_wgs84_longitude(polygon.x, polygon.y),
                                       polygon.lat = f_rd_to_wgs84_latitude(polygon.x, polygon.y)
                                )
                }

                # Hernoem lon,lat naar polygon.lon en polygon.lat, en voeg polygon.x en polygon.y toe.
                if( all(v.coord.polygon.lonlat %in% names(df.polygon)) ) {

                        df.polygon$polygon.lat <- as.numeric(df.polygon[[c.coord.polygon.lat]])
                        df.polygon$polygon.lon <- as.numeric(df.polygon[[c.coord.polygon.lon]])

                        # Voeg x en y toe. Let op f_wgs84_to_rd_x heeft 'lat' als eerste input en 'lon' als tweede input.
                        df.polygon <- df.polygon %>%

                                mutate(polygon.x = f_wgs84_to_rd_x(polygon.lat, polygon.lon),
                                       polygon.y = f_wgs84_to_rd_y(polygon.lat, polygon.lon)
                                )
                }


                ##########################################################
                # LABELS & POP-UPS
                ##########################################################

                # SEED:
                v.polygon.label <- NULL
                v.polygon.popup <- "<b><u>OBJECT-INFORMATIE:</b></u>"


                # LABEL:
                if(b.show.polygon.label) {

                        v.polygon.label <- paste0(v.polygon.label, "Polygon ID ", df.polygon %>% pull(c.id.polygon))

                        #print(v.info.tag.polygon.label)
                        #print(v.info.veld.polygon.label)

                        # Voeg additionele informatie toe indien gegeven.
                        if(!is.null(v.info.tag.polygon.label)) {

                                # Voeg additionele informatie toe:
                                for (i in seq_along(v.info.tag.polygon.label)) { # i = 1

                                        v.polygon.label <- paste0(

                                                v.polygon.label, " / ",
                                                v.info.tag.polygon.label[i], " ",
                                                pull(df.polygon, v.info.veld.polygon.label[i])
                                        )
                                }
                        }

                } else {

                        v.polygon.label <- NA

                }


                # POP-UP:
                if(b.show.polygon.popup) {

                        # Voeg ID toe:
                        v.polygon.popup <- paste0(v.polygon.popup,
                                                "<br><br><b><u>Polygon ID:</b></u><br>",
                                                df.polygon %>% pull(c.id.polygon))


                        # Voeg additionele informatie toe.
                        if(!is.null(v.info.tag.polygon.popup)) {

                                v.polygon.popup <- paste0(v.polygon.popup,
                                                          "<br><br><b><u>Additionele informatie:</b></u>")

                                # Voeg additionele informatie toe:
                                for (i in seq_along(v.info.tag.polygon.popup)) { # i = 1

                                        v.polygon.popup <- paste0(

                                                v.polygon.popup, "<br>",
                                                v.info.tag.polygon.popup[i], ": ",
                                                pull(df.polygon, v.info.veld.polygon.popup[i])
                                        )
                                }
                        }


                } else {

                        v.polygon.popup <- NA

                }
        }


        ######################################################################
        # PERMANENT TEXT LABEL
        ######################################################################

        if(!is.null(df.text)) {

                # CONVERSIE COORDINAAT LABELS.
                c.coord.text.x      <- gsub("lon$", "x", v.coord.text[1])
                c.coord.text.y      <- gsub("lat$", "y", v.coord.text[2])
                c.coord.text.lon    <- gsub("x$", "lon", v.coord.text[1])
                c.coord.text.lat    <- gsub("y$", "lat", v.coord.text[2])
                v.coord.text.xy     <- c(c.coord.text.x,   c.coord.text.y)
                v.coord.text.lonlat <- c(c.coord.text.lon, c.coord.text.lat)


                # Hernoem x,y naar text.x en text.y, en voeg text.lon en text.lat toe.
                if( all(v.coord.text.xy %in% names(df.text)) ) {

                        df.text$text.x <- as.numeric(df.text[[c.coord.text.x]])
                        df.text$text.y <- as.numeric(df.text[[c.coord.text.y]])

                        # Voeg lon en lat toe.
                        df.text <- df.text %>%

                                mutate(text.lon = f_rd_to_wgs84_longitude(text.x, text.y),
                                       text.lat = f_rd_to_wgs84_latitude(text.x, text.y)
                                )
                }

                # Hernoem lon,lat naar text.lon en text.lat, en voeg text.x en text.y toe.
                if( all(v.coord.text.lonlat %in% names(df.text)) ) {

                        df.text$text.lat <- as.numeric(df.text[[c.coord.text.lat]])
                        df.text$text.lon <- as.numeric(df.text[[c.coord.text.lon]])

                        # Voeg x en y toe. Let op f_wgs84_to_rd_x heeft 'lat' als eerste input en 'lon' als tweede input.
                        df.text <- df.text %>%

                                mutate(text.x = f_wgs84_to_rd_x(text.lat, text.lon),
                                       text.y = f_wgs84_to_rd_y(text.lat, text.lon)
                                )
                }
        }


##############################################################################
# POINTS - BOUW LABEL TEXT
##############################################################################

        if(!is.null(df.point) & b.show.point.label) {

                # Initialize
                v.point.label    <- NULL
                v.names.df.point <- names(df.point)


                # Label bij punten.
                if("gemeentenaam" %in% v.names.df.point)
                        v.point.label <- paste0(v.point.label, df.point$gemeentenaam)

                if("photo.number" %in% v.names.df.point)
                        v.point.label <- paste0(v.point.label, " / photo.number ", df.point$photo.number)

                if("id" %in% v.names.df.point)
                        v.point.label <- paste0(v.point.label, " / ID ",  df.point$id)

                if("point.id" %in% v.names.df.point)
                        v.point.label <- paste0(v.point.label, " / point ID ",  df.point$point.id)

                if("bord.id" %in% v.names.df.point)
                        v.point.label <- paste0(v.point.label, " / bord.id ",  df.point$bord.id)

                if("drager.id" %in% v.names.df.point)
                        v.point.label <- paste0(v.point.label, " / drager.id ",  df.point$drager.id)

                if("bord.type" %in% v.names.df.point)
                        v.point.label <- paste0(v.point.label, " / bord.type ",  df.point$bord.type)

                if("bord.schouw" %in% v.names.df.point)
                        v.point.label <- paste0(v.point.label, " / bord.schouw ",  df.point$bord.schouw)

                if("wegsegment.id" %in% v.names.df.point & !"wegsegment.subid" %in% v.names.df.point)
                        v.point.label <- paste0(v.point.label, " / wegsegment ",  df.point$wegsegment.id)

                if("wegsegment.subid" %in% v.names.df.point)
                        v.point.label <- paste0(v.point.label, " / wegsegment ",  df.point$wegsegment.subid)

                if("center.seq" %in% v.names.df.point)
                        v.point.label <- paste0(v.point.label, " / center seq ",  df.point$center.seq)

                if("wegnummer.hmp" %in% v.names.df.point)
                        v.point.label <- paste0(v.point.label, " / wegnummer ",  df.point$wegnummer.hmp)

                if("bord.kompas" %in% v.names.df.point)
                        v.point.label <- paste0(v.point.label, " / bord.kompas ",  df.point$bord.kompas)

                # point.x en point.y zitten altijd in de data. Zo niet in de input df, dan worden ze hierboven toegevoegd.
                v.point.label <- paste0(v.point.label, " / x ",  round(df.point$point.x))
                v.point.label <- paste0(v.point.label, " / y ",  round(df.point$point.y))

                if("fill.point.label" %in% v.names.df.point & !all(is.na(df.point$fill.point.label)))
                        v.point.label <- paste0(v.point.label, " / ", c.legend.title, " ",  df.point$fill.point.label)

                if("stroke.color.point.label" %in% v.names.df.point & !all(is.na(df.point$stroke.color.point.label)))
                    v.point.label <- paste0(v.point.label, " / ", c.legend.title, " ",  df.point$stroke.color.point.label)

                # Voeg additionele informatie toe indien gegeven.
                if(!is.null(v.info.tag.point.label)) {

                        # Voeg additionele informatie toe:
                        for (i in seq_along(v.info.tag.point.label)) { # i = 2

                                v.point.label <- paste0(

                                        v.point.label, " / ",
                                        v.info.tag.point.label[i], " ",
                                        pull(df.point, v.info.veld.point.label[i])
                                )
                        }
                }

                # Verwijder eventuele " / " aan het begin.
                v.point.label <- gsub("^ / ", "", v.point.label)

        } else {

                v.point.label <- NA
        }



##############################################################################
# POINTS BOUW POP-UP TEXT
##############################################################################

        if(!is.null(df.point) & b.show.point.popup) {

                # Initialize.
                v.point.popup <- "<b><u>OBJECT-INFORMATIE:</b></u>"

                # Pop-up bij punten.
                if("photo.number" %in% v.names.df.point)

                        v.point.popup <- paste0(v.point.popup,
                                             "<br><br><b><u>Photo number:</b></u><br>",
                                             df.point$photo.number)

                if("drager.type" %in% v.names.df.point)

                        v.point.popup <- paste0(v.point.popup,
                                               "<br><br><b><u>Drager Type:</b></u><br>",
                                               df.point$drager.type)

                if("id" %in% v.names.df.point)

                        v.point.popup <- paste0(v.point.popup,
                                               "<br><br><b><u>ID:</b></u><br>",
                                               df.point$id)

                if("point.id" %in% v.names.df.point)

                        v.point.popup <- paste0(v.point.popup,
                                                "<br><br><b><u>Point ID:</b></u><br>",
                                                df.point$point.id)

                if("bord.id" %in% v.names.df.point)

                        v.point.popup <- paste0(v.point.popup,
                                             "<br><br><b><u>Bord ID:</b></u><br>",
                                             df.point$bord.id)

                if("bord.fid" %in% v.names.df.point)

                    v.point.popup <- paste0(v.point.popup,
                                           "<br><br><b><u>Bord FID:</b></u><br>",
                                           df.point$bord.fid)

                if("drager.id" %in% v.names.df.point)

                        v.point.popup <- paste0(v.point.popup,
                                             "<br><br><b><u>Drager ID:</b></u><br>",
                                             df.point$drager.id)

                if("drager.fid" %in% v.names.df.point)

                        v.point.popup <- paste0(v.point.popup,
                                               "<br><br><b><u>Drager FID:</b></u><br>",
                                               df.point$drager.fid)

                if("bord.type" %in% v.names.df.point)

                        v.point.popup <- paste0(v.point.popup,
                                             "<br><br><b><u>Bord type:</b></u><br>",
                                             df.point$bord.type)

                if("point.type" %in% v.names.df.point)

                        v.point.popup <- paste0(v.point.popup,
                                                "<br><br><b><u>Point type:</b></u><br>",
                                                df.point$point.type)

                # Bord tekst.
                if("bord.tekst" %in% v.names.df.point)

                        v.point.popup <- paste0(v.point.popup,
                                                "<br><br><b><u>Bordtekst:</b></u><br>",
                                                df.point$bord.tekst)

                # Schouwrichting.
                if("bord.schouw" %in% v.names.df.point)

                        v.point.popup <- paste0(v.point.popup,
                                               "<br><br><b><u>Schouwrichting (graden):</b></u><br>",
                                               round(df.point$bord.schouw, 0))

                # Locatie
                v.point.popup <- paste0(

                        v.point.popup,
                        "<br><br><b><u>Locatie:</b></u>",
                        "<br>RD X,Y (m):  ",      round(df.point$point.x,   1), ", ",
                                                  round(df.point$point.y,   1),
                        "<br>Lon,Lat (WGS84):  ", round(df.point$point.lon, 6), ", ",
                                                  round(df.point$point.lat, 6)
                )


                if("bord.kompas" %in% v.names.df.point)

                        v.point.popup <- paste0(v.point.popup,
                                             " (kompas: ", df.point$bord.kompas, ")")

                # Maximum snelheid.
                if("bord.max.snelheid" %in% v.names.df.point)

                        v.point.popup = ifelse(!is.na(df.point$bord.max.snelheid),

                                            paste0(v.point.popup,
                                                   "<br><br><b><u>Max snelheid:</b></u><br>",
                                                   df.point$bord.max.snelheid),

                                            v.point.popup)

                # Wegvak
                if(
                         "wegvak.id" %in% v.names.df.point &
                        !"wegsegment.id" %in% v.names.df.point &
                        !"wegsegment.subid" %in% v.names.df.point
                   )

                        v.point.popup <- paste0(v.point.popup,
                                             "<br><br><b><u>Wegvak:</b></u><br>",
                                             df.point$wegvak.id)

                if("wegsegment.id" %in% v.names.df.point & !"wegsegment.subid" %in% v.names.df.point)

                        v.point.popup <- paste0(v.point.popup,
                                                "<br><br><b><u>Wegsegment:</b></u><br>",
                                                df.point$wegsegment.id)

                if("wegsegment.subid" %in% v.names.df.point)

                        v.point.popup <- paste0(v.point.popup,
                                                "<br><br><b><u>Wegsegment:</b></u><br>",
                                                df.point$wegsegment.subid)

                if(all(c("wegsegment.order", "wegsegment.suborder") %in% v.names.df.point))

                        v.point.popup <- paste0(

                                v.point.popup,
                                " (segment: ", df.point$wegsegment.order,
                                ",sub ",       df.point$wegsegment.suborder, ")")

                # Wegnummer.
                if("wegnummer.hmp" %in% v.names.df.point)

                        v.point.popup <- paste0(v.point.popup,
                                             "<br><br><b><u>Wegnummer:</b></u><br>",
                                             df.point$wegnummer.hmp)

                if("rel.positie.code" %in% v.names.df.point)

                        v.point.popup <- paste0(v.point.popup,
                                             " (relatieve positie: ", df.point$rel.positie.code, ")")

                # Wegbeheerder.
                if(any(c("wegbeheerder.type", "wegbeheerder.naam", "wegbeheerder.code", "wegbeheerder.picklist") %in% v.names.df.point)) {


                        v.point.popup <- paste0(v.point.popup,
                                             "<br><br><b><u>Wegbeheerder:</b></u>")


                        if("wegbeheerder.type" %in% v.names.df.point)

                                v.point.popup <- paste0(v.point.popup, "<br>Type:       ", df.point$wegbeheerder.type)

                        if("wegbeheerder.naam" %in% v.names.df.point)

                                v.point.popup <- paste0(v.point.popup, "<br>Naam:      ", df.point$wegbeheerder.naam)

                        if("wegbeheerder.code" %in% v.names.df.point)

                                v.point.popup <- paste0(v.point.popup, "<br>Code:      ", df.point$wegbeheerder.code)

                        if("wegbeheerder.picklist" %in% v.names.df.point)

                                v.point.popup <- paste0(v.point.popup, "<br>Picklist:  ", df.point$wegbeheerder.picklist)
                        }



                # Adres.
                if(any(c("straatnaam.nwb", "woonplaatsnaam.nwb",
                         "gemeentenaam.nwb", "gemeentenaam", "gemeentecode.nwb", "gemeentecode",
                         "provincienaam", "provinciecode") %in% v.names.df.point)) {


                        v.point.popup <- paste0(v.point.popup,
                                                "<br><br><b><u>Adres:</b></u>")


                        if("straatnaam.nwb" %in% v.names.df.point)

                                v.point.popup <- paste0(v.point.popup, "<br>Straatnaam:              ", df.point$straatnaam.nwb)

                        if("woonplaatsnaam.nwb" %in% v.names.df.point)

                                v.point.popup <- paste0(v.point.popup, "<br>Woonplaatsnaam:      ", df.point$woonplaatsnaam.nwb)

                        if("gemeentenaam.nwb" %in% v.names.df.point)

                                v.point.popup <- paste0(v.point.popup, "<br>Gemeentenaam (NWB):  ", df.point$gemeentenaam.nwb)

                        if("gemeentenaam" %in% v.names.df.point)

                                v.point.popup <- paste0(v.point.popup, "<br>Gemeentenaam:        ", df.point$gemeentenaam)

                        if("gemeentecode.nwb" %in% v.names.df.point)

                                v.point.popup <- paste0(v.point.popup, "<br>Gemeentencode (NWB): ", df.point$gemeentecode.nwb)

                        if("gemeentecode" %in% v.names.df.point)

                                v.point.popup <- paste0(v.point.popup, "<br>Gemeentecode:        ", df.point$gemeentecode)

                        if("provincienaam" %in% v.names.df.point)

                                v.point.popup <- paste0(v.point.popup, "<br>Provincienaam:       ", df.point$provincienaam)

                        if("provinciecode" %in% v.names.df.point)

                                v.point.popup <- paste0(v.point.popup, "<br>Provinciecode:       ", df.point$provinciecode)
                }


                # Voeg additionele informatie toe.
                if(!is.null(v.info.tag.point.popup)) {

                    v.point.popup <- paste0(v.point.popup,
                                           "<br><br><b><u>Additionele informatie:</b></u>")

                    # Voeg additionele informatie toe:
                    for (i in seq_along(v.info.tag.point.popup)) { # i = 2

                        v.point.popup <- paste0(v.point.popup, "<br>",
                                               v.info.tag.point.popup[i], ": ",
                                               pull(df.point, v.info.veld.point.popup[i])
                                               )
                    }
                }

                # Legend informatie.
                if(!is.null(c.fill.factor.point) | !is.null(c.fill.numeric.point)) {

                        v.point.popup <- paste0(
                                                v.point.popup,
                                                "<br><br><b><u>", c.legend.title, " (legend)</b></u><br>",
                                                df.point$fill.point.label
                                                )
                }

                if(!is.null(c.stroke.factor.point) | !is.null(c.stroke.numeric.point)) {

                        v.point.popup <- paste0(
                                                v.point.popup,
                                                "<br><br><b><u>", c.legend.title, " (legend)</b></u><br>",
                                                df.point$stroke.color.point.label
                                                )
                }




                # Add hyperlinks.
                v.point.popup <- paste0(v.point.popup, "<br><br><b><u>Bekijk objecten in:</b></u><br>")


                if(b.add.streetsmart) {

                        v.point.popup <- paste0(

                                v.point.popup,

                                paste0(
                                        "<a href = '",
                                        df.point$url,
                                        "' target='_blank' >Street Smart ('perfecte plaatje')</a><br>"
                                )
                        )

                }

                # Voeg StreetView link toe
                if("bord.schouw" %in% v.names.df.point) {

                        v.point.popup <- paste0(

                                v.point.popup,

                                "<a href = 'http://maps.google.com/maps?q=&layer=c&cbll=",
                                df.point$point.lat, ",", df.point$point.lon, "&cbp=11,",
                                df.point$bord.schouw, ",0,0,0' target='_blank' >Street View</a>"
                        )

                } else {

                        v.point.popup <- paste0(

                                v.point.popup,

                                "<a href = 'http://maps.google.com/maps?q=&layer=c&cbll=",
                                df.point$point.lat, ",", df.point$point.lon, "&cbp=11,",
                                0, ",0,0,0' target='_blank' >Street View</a>")
                        }
        } else {

                v.point.popup <- NA
        }


##############################################################################
# PREPARE LEAFLET.
##############################################################################

        ######################################################################
        # TITLE
        ######################################################################

        # Definieer 'c.title.append' (placeholder).
        c.title.append <- ""

        if(!is.null(df.point)) {

                if(!is.null(c.fill.factor.point)) {

                        df.count <- df.point %>%

                                # Remove dummy poins, lines, polygons from count in title. Dummies are
                                # placed around 0,0.
                                filter(point.y > 100000) %>%

                                count(fill.factor = get(c.fill.factor.point), name = "n.fill.factor") %>%

                                arrange(desc(n.fill.factor)) %>%

                                mutate(label = paste0(fill.factor, " (", n.fill.factor, ")"))

                        c.title.append <- f_paste(df.count$label)
                }

                if("gemeentenaam" %in% v.names.df.point) {

                        if(length(unique(df.point$gemeentenaam)) < 4) {

                                c.title.append <- paste0(

                                        c.title.append, " in ",
                                        f_paste(unique(df.point$gemeentenaam))
                                )
                        }
                }
        }


        # Formuleer title.
        html.title <- tags$div(tag.map.title, HTML(paste0(c.leaflet.title, "<br>", c.title.append)))


        ######################################################################
        # POINTS
        ######################################################################

        if(!is.null(df.point)) {

                # Add label and pop-up
                df.point <- df.point %>%

                        mutate(
                                # Original
                                #point.label = v.point.label,
                                #point.popup = v.point.popup

                                # Clean non-UTF-8 or otherwise malformed text.
                                point.label = iconv(
                                        v.point.label,
                                        from = "",
                                        to   = "UTF-8",
                                        sub  = "byte"
                                ),

                                # Clean non-UTF-8 or otherwise malformed text.
                                point.popup = iconv(
                                        v.point.popup,
                                        from = "",
                                        to   = "UTF-8",
                                        sub  = "byte"
                                )
                        )

                # Define POINT fill colors.
                if(!is.null(c.fill.factor.point)) {

                        f.color.point.fill     <- colorFactor(

                                palette = df.fill.point$fill.color.point.value,
                                levels  = df.fill.point$fill.color.point.label,
                                ordered = TRUE
                        )
                }

                if(!is.null(c.fill.numeric.point)) {

                        f.color.point.fill     <- colorNumeric(

                                palette = "Blues",
                                domain  = df.point[[c.fill.numeric.point]]
                        )
                }


                # Define POINT stroke colors.
                if(!is.null(c.stroke.factor.point)) {

                        f.color.point.stroke   <- colorFactor(

                                palette = df.stroke.color.point$stroke.color.point.value,
                                levels  = df.stroke.color.point$stroke.color.point.label,
                                ordered = TRUE
                        )
                }

                if(!is.null(c.stroke.numeric.point)) {

                        f.color.point.stroke   <- colorNumeric(

                                palette = "Blues",
                                domain  = df.point[[c.stroke.numeric.point]]
                        )
                }
        }



        ######################################################################
        # LINES
        ######################################################################

        if(!is.null(df.line)) {

                # Add label and pop-up.
                df.line <- df.line %>%

                        mutate(
                                line.label = v.line.label,
                                line.popup = v.line.popup
                        )
        }



        ######################################################################
        # POLYGONS
        ######################################################################

        if(!is.null(df.polygon)) {

                # Add label and pop-up.
                df.polygon <- df.polygon %>%

                        mutate(
                                polygon.label = v.polygon.label,
                                polygon.popup = v.polygon.popup
                        )


                # Define POLYGON fill colors.
                if(!is.null(c.fill.numeric.polygon)) {

                        f.color.polygon.fill     <- colorNumeric(

                                palette = "Blues",
                                domain  = df.polygon[[c.fill.numeric.polygon]]
                        )
                }
        }


        ######################################################################
        # PERMANENT TEXT LABEL.
        ######################################################################

        if(!is.null(df.text)) {

                df.text <- df.text %>%

                        rename(
                                text.label := !!c.text.label
                        )

        }


##############################################################################
# BUILD LEAFLET.
##############################################################################


        plot.leaflet <- leaflet(

                options = leafletOptions(zoomControl = b.zoom.control)
        ) %>%

                # Set base map - Default OSM.
                addTiles(

                        options = providerTileOptions(

                                                minZoom = 1,
                                                maxZoom = 19,
                                                opacity = 0.5),


                        group = "OSM (default)"
                ) %>%


                # Set base map - Toner Light.
                addProviderTiles(

                        # https://maps.stamen.com/stadia-partnership/
                        # I replaced Stamen maps by OSM, because we are getting warnings that the
                        # service will stop operating as it did and that we need to make an account.
                        # Let's keep things simple.
                        #provider = providers$Stamen.TonerLite,
                        #group    = "Toner Lite",

                        # Same map as OSM
                        #provider = "Esri.WorldStreetMap",
                        #group    = "Esri Maps",

                        #provider = "CartoDB.Positron",
                        #group    = "CartoDB Maps",

                        provider  = "OpenStreetMap",

                        options = providerTileOptions(

                                                minZoom = 1,
                                                maxZoom = 19,
                                                opacity = 0.5)
                ) %>%


                # Set distance measurement tool.
                addMeasure(
                        position          = "bottomleft",
                        primaryLengthUnit = "meters",
                        primaryAreaUnit   = "sqmeters",
                        activeColor       = "#3D535D",
                        completedColor    = "#7D4479") %>%

                # Show Zoom level.
                addControl(html="<h2 id='zoom'>Zoom</h2>") %>%

                onRender("function(el,x,data)
                                        {var map = this;
                                         var evt = function(e){$('#zoom').html(map.getZoom())};
                                         map.on('zoom', evt);}") %>%

                addControl(html.title, position = "topleft", className="map-title")


        # Set midpoint.
        if(is.null(v.coord.point.midpoint)) {

                if(!is.null(df.point)) {

                        plot.leaflet <- plot.leaflet %>%

                                # Set midpoint.
                                setView(
                                        lng  = mean(df.point$point.lon, na.rm = TRUE),
                                        lat  = mean(df.point$point.lat, na.rm = TRUE),
                                        zoom = n.zoom)

                } else if(!is.null(df.line)) {

                        plot.leaflet <- plot.leaflet %>%

                                # Set midpoint.
                                setView(
                                        lng  = mean(df.line$line.lon, na.rm = TRUE),
                                        lat  = mean(df.line$line.lat, na.rm = TRUE),
                                        zoom = n.zoom)

                } else if(!is.null(df.polygon)) {

                        plot.leaflet <- plot.leaflet %>%

                                # Set midpoint.
                                setView(
                                        lng  = mean(df.polygon$polygon.lon, na.rm = TRUE),
                                        lat  = mean(df.polygon$polygon.lat, na.rm = TRUE),
                                        zoom = n.zoom)
                } else {

                        stop("Er is onvoldoende data om het midpoint en de zoom te bepalen!")
                }

        } else {

                plot.leaflet <- plot.leaflet %>%

                        # Set midpoint.
                        setView(
                                lng  = v.coord.point.midpoint[1],
                                lat  = v.coord.point.midpoint[2],
                                zoom = n.zoom)
        }


        ######################################################################
        # POLYGONS
        ######################################################################

        # https://stackoverflow.com/questions/37820196/r-leaflet-passing-popupoptions-when-adding-polygons

        if(!is.null(df.polygon)) {

                # Als c.layer geen waarde heeft --> fill with dummy:
                if(is.null(c.layer)) {

                        df.polygon       <- df.polygon %>% mutate(polygon = "polygon")
                        c.layer.polygon  <- "polygon"

                # Als c.layer wel een waarde heeft...
                } else {

                        # maar niet in df.polgon voorkomt --> fill with dummy:
                        if(!c.layer %in% names(df.polygon)) {

                                df.polygon       <- df.polygon %>% mutate(polygon = "polygon")
                                c.layer.polygon  <- "polygon"

                        # en als wel in df.polygon voorkomt --> c.lager.polygon gelijk gesteld aan c.layer.
                        } else {

                                c.layer.polygon  <- c.layer
                        }
                }


                # https://rstudio.github.io/leaflet/showhide.html
                df.polygon.grouped <- df.polygon %>% group_by(get(c.layer.polygon))
                l.polygon          <- group_split(df.polygon.grouped)
                v.keys             <- group_keys(df.polygon.grouped) %>% unlist() %>% unname()

                if(length(v.keys) > 15) {

                        warning("Let op, er zijn ", length(v.keys),
                                " lagen in df.polygon, en dat is misschien wel beetje veel van het goede. ",
                                "Kies andere c.layer in df.polygon dan '" , c.layer.polygon, "'?")
                }


                # Plot de polygonen per 'c.layer'
                for (i in seq_along(v.keys)) { # i = 1


                        ##########################################################
                        # SPATIAL POLYGONS
                        ##########################################################

                        # Convert to SpatialPolygons object, voor gebruik in de leaflet.
                        sp.polygon <- f_create_SpatialPolygons(

                                df.polygon = l.polygon[[i]],
                                v.coord    = v.coord.polygon.lonlat,
                                c.group    = c.id.polygon
                        )


                        # FILL variable.
                        if(!is.null(c.fill.numeric.polygon)) {


                                # Create SpatialPolygonsDataFrame
                                spdf.polygon <- SpatialPolygonsDataFrame(

                                        Sr   = sp.polygon,

                                        # Rownames is deprecated for tibbles. Therefore
                                        # dataframe set to as.data.frame.
                                        data = as.data.frame(l.polygon[[i]]) %>%

                                                select(all_of(c.id.polygon), polygon.label, polygon.popup, polygon.fill) %>%

                                                distinct() %>%

                                                # Add rownames.
                                                tibble::column_to_rownames(c.id.polygon)
                                )


                                plot.leaflet <- plot.leaflet %>%

                                        # Toon polygonen.
                                        addPolygons(

                                                data             = spdf.polygon,

                                                color            = c.stroke.color.polygon,
                                                weight           = n.stroke.weight.polygon,
                                                stroke           = b.show.stroke.polygon,

                                                fillColor        = ~f.color.polygon.fill(polygon.fill),
                                                fillOpacity      = n.fill.opacity.polygon,

                                                label            = ~polygon.label,
                                                popup            = ~polygon.popup,

                                                group            = v.keys[i],

                                                highlightOptions = highlightOptions(
                                                                        color        = "white",
                                                                        weight       = 2,
                                                                        bringToFront = TRUE)
                                        )

                        # FILL fixed.
                        } else {


                                # Create SpatialPolygonsDataFrame
                                spdf.polygon <- SpatialPolygonsDataFrame(

                                        Sr   = sp.polygon,

                                        # Rownames is deprecated for tibbles. Therefore
                                        # dataframe set to as.data.frame.
                                        data = as.data.frame(l.polygon[[i]]) %>%

                                                select(all_of(c.id.polygon), polygon.label, polygon.popup) %>%

                                                distinct() %>%

                                                # Add rownames.
                                                tibble::column_to_rownames(c.id.polygon)
                                )


                                plot.leaflet <- plot.leaflet %>%

                                        # Toon polygonen.
                                        addPolygons(

                                                data             = spdf.polygon,

                                                color            = c.stroke.color.polygon,
                                                weight           = n.stroke.weight.polygon,
                                                stroke           = b.show.stroke.polygon,

                                                fillColor        = "#708090",
                                                fillOpacity      = n.fill.opacity.polygon,

                                                label            = ~polygon.label,
                                                popup            = ~polygon.popup,

                                                group            = v.keys[i]
                                        )
                        }

                }

                # Layers control.
                if(c.layer.polygon != "polygon") {

                        plot.leaflet <- plot.leaflet %>%

                                addLayersControl(

                                        baseGroups    = c("OSM (default)", "Toner Lite"),
                                        overlayGroups = v.keys,
                                        options       = layersControlOptions(collapsed = FALSE)
                                )
                }

        } else {

                c.layer.polygon  <- "polygon"
        }


        ######################################################################
        # LINES
        ######################################################################

        if(!is.null(df.line)) {

                # Als c.layer geen waarde heeft --> fill with dummy:
                if(is.null(c.layer)) {

                        df.line       <- df.line %>% mutate(line = "line")
                        c.layer.line  <- "line"

                # Als c.layer wel een waarde heeft...
                } else {

                        # maar niet in df.polgon voorkomt --> fill with dummy:
                        if(!c.layer %in% names(df.line)) {

                                df.line       <- df.line %>% mutate(line = "line")
                                c.layer.line  <- "line"

                        # en als wel in df.line voorkomt --> c.lager.line gelijk gesteld aan c.layer.
                        } else {

                                c.layer.line  <- c.layer
                        }
                }


                # https://rstudio.github.io/leaflet/showhide.html
                df.line.grouped <- df.line %>% group_by(get(c.layer.line))
                l.line          <- group_split(df.line.grouped)
                v.keys          <- group_keys(df.line.grouped) %>% unlist() %>% unname()

                if(length(v.keys) > 15) {

                        warning("Let op, er zijn ", length(v.keys),
                                " lagen in df.line, en dat is misschien wel beetje veel van het goede. ",
                                "Kies andere c.layer in df.line dan '" , c.layer.line, "'?")
                }


                # Plot de lijnen per 'c.layer'
                for (i in seq_along(v.keys)) { # i = 1


                        ##########################################################
                        # SPATIAL LINES
                        ##########################################################

                        # Convert to SpatialLines object, voor gebruik in de leaflet.
                        sl.line <- f_create_SpatialLines(

                                df.line = l.line[[i]],
                                v.coord = v.coord.line,
                                c.group = c.id.line)


                        sldf.line <- SpatialLinesDataFrame(

                                sl   = sl.line,

                                # Rownames is deprecated for tibbles. Therefore
                                # dataframe set to as.data.frame.
                                data = as.data.frame(df.line) %>%

                                        select(all_of(c.id.line), color.line.value, weight.line.value) %>%

                                        distinct() %>%

                                        # Add rownames.
                                        tibble::column_to_rownames(c.id.line)
                        )


                        plot.leaflet <- plot.leaflet %>%

                                # Toon lineen.
                                addPolylines(

                                        data     = sldf.line,

                                        color    = ~color.line.value,
                                        weight   = ~weight.line.value,
                                        opacity  = n.opacity.line
                                        )
                }

                # Layers control.
                if(c.layer.line != "line") {

                        plot.leaflet <- plot.leaflet %>%

                                addLayersControl(

                                        baseGroups    = c("OSM (default)", "Toner Lite"),
                                        overlayGroups = v.keys,
                                        options       = layersControlOptions(collapsed = FALSE)
                                )
                }

        } else {

                c.layer.line  <- "line"
        }


        ######################################################################
        # POINTS
        ######################################################################

        if(!is.null(df.point)) {

                # Als c.layer geen waarde heeft --> fill with dummy:
                if(is.null(c.layer)) {

                        df.point       <- df.point %>% mutate(point = "point")
                        c.layer.point  <- "point"

                # Als c.layer wel een waarde heeft...
                } else {

                        # maar niet in df.polgon voorkomt --> fill with dummy:
                        if(!c.layer %in% names(df.point)) {

                                df.point       <- df.point %>% mutate(point = "point")
                                c.layer.point  <- "point"

                        # en als wel in df.point voorkomt --> c.lager.point gelijk gesteld aan c.layer.
                        } else {

                                c.layer.point  <- c.layer
                        }
                }

                # https://rstudio.github.io/leaflet/showhide.html
                df.point.grouped <- df.point %>% group_by(get(c.layer.point))
                l.point          <- group_split(df.point.grouped)
                v.keys           <- group_keys(df.point.grouped) %>% unlist() %>% unname()

                if(length(v.keys) > 15) {

                        warning("Let op, er zijn ", length(v.keys),
                                " lagen in df.point, en dat is misschien wel beetje veel van het goede. ",
                                "Kies andere c.layer in df.point dan '" , c.layer.point, "'?")
                }



                # Add circle markers: FILL variable, STROKE variable.
                if((!is.null(c.fill.factor.point) |   !is.null(c.fill.numeric.point)) &
                   (!is.null(c.stroke.factor.point) | !is.null(c.stroke.numeric.point))) {

                        for (i in seq_along(v.keys)) {

                                plot.leaflet <- plot.leaflet %>%

                                        # Voeg borden en titel toe.
                                        addCircleMarkers(

                                                data        = l.point[[i]],

                                                lat         = ~ point.lat,
                                                lng         = ~ point.lon,
                                                fillColor   = ~ f.color.point.fill(fill.point.label),
                                                fillOpacity = n.opacity.fill,
                                                radius      = ~ weight.point.value,
                                                color       = ~ f.color.point.stroke(stroke.color.point.label),
                                                weight      = ~ stroke.weight.point.value,
                                                opacity     = n.opacity.stroke,
                                                popup       = ~ point.popup,
                                                label       = ~ point.label,
                                                group       = v.keys[i]
                                                )
                                }


                        plot.leaflet <- plot.leaflet %>%

                                addLegend(

                                        data      = df.point,
                                        position  = "bottomright",
                                        pal       = f.color.point.fill,
                                        values    = ~ fill.point.label,
                                        title     = c.legend.title,
                                        #labFormat = labelFormat(prefix = "$"),
                                        opacity   = 1
                                        )


                # Add circle markers: FILL variable, STROKE fixed.
                } else if((!is.null(c.fill.factor.point) |  !is.null(c.fill.numeric.point)) &
                           (is.null(c.stroke.factor.point)  & is.null(c.stroke.numeric.point))) {

                    for (i in seq_along(v.keys)) { # i = 1

                            plot.leaflet <- plot.leaflet %>%

                                    # Voeg borden en titel toe.
                                    addCircleMarkers(

                                                data        = l.point[[i]],
                                                lat         = ~ point.lat,
                                                lng         = ~ point.lon,
                                                fillColor   = ~ f.color.point.fill(fill.point.label),
                                                fillOpacity = n.opacity.fill,
                                                radius      = ~ weight.point.value,
                                                color       = "darkblue",
                                                weight      = ~ stroke.weight.point.value,
                                                opacity     = n.opacity.stroke,
                                                popup       = ~ point.popup,
                                                label       = ~ point.label,
                                                group       = v.keys[i]
                                        )
                            }


                    plot.leaflet <- plot.leaflet %>%

                          addLegend(

                                data      = df.point,
                                position  = "bottomright",
                                pal       = f.color.point.fill,
                                values    = ~ fill.point.label,
                                title     = c.legend.title,
                                #labFormat = labelFormat(prefix = "$"),
                                opacity   = 1
                                )


                # Add circle markers: FILL fixed, STROKE variable.
                } else if(( is.null(c.fill.factor.point) &    is.null(c.fill.numeric.point)) &
                          (!is.null(c.stroke.factor.point) | !is.null(c.stroke.numeric.point))) {

                        for (i in seq_along(v.keys)) {

                                plot.leaflet <- plot.leaflet %>%

                                        # Voeg borden en titel toe.
                                        addCircleMarkers(

                                                data        = l.point[[i]],
                                                lat         = ~ point.lat,
                                                lng         = ~ point.lon,
                                                fillColor   = "blue",
                                                fillOpacity = n.opacity.fill,
                                                radius      = ~ weight.point.value,
                                                color       = ~ f.color.point.stroke(stroke.color.point.label),
                                                weight      = ~ stroke.weight.point.value,
                                                opacity     = n.opacity.stroke,
                                                popup       = ~ point.popup,
                                                label       = ~ point.label,
                                                group       = v.keys[i]
                                        )
                                }

                        plot.leaflet <- plot.leaflet %>%

                                addLegend(

                                        data      = df.point,
                                        position  = "bottomright",
                                        pal       = f.color.point.stroke,
                                        values    = ~ stroke.color.point.label,
                                        title     = c.legend.title,
                                        #labFormat = labelFormat(prefix = "$"),
                                        opacity   = 1
                                        )


                # Add circle markers: FILL fixed, STROKE fixed.
                } else {

                        for (i in seq_along(v.keys)) {

                                plot.leaflet <- plot.leaflet %>%

                                        # Voeg borden en titel toe.
                                        addCircleMarkers(

                                                data        = l.point[[i]],

                                                lat         = ~ point.lat,
                                                lng         = ~ point.lon,
                                                fillColor   = "blue",
                                                fillOpacity = n.opacity.fill,
                                                radius      = ~ weight.point.value,
                                                color       = "darkblue",
                                                weight      = ~ stroke.weight.point.value,
                                                opacity     = n.opacity.stroke,
                                                popup       = ~ point.popup,
                                                label       = ~ point.label,
                                                group       = v.keys[i]
                                                )
                                }
                }


                # Layers control.
                if(c.layer.point != "point") {

                        # Layers control.
                        plot.leaflet <- plot.leaflet %>%

                                addLayersControl(

                                        baseGroups    = c("OSM (default)", "Toner Lite"),
                                        overlayGroups = v.keys,
                                        options       = layersControlOptions(collapsed = FALSE)
                                )
                }

        } else {

                c.layer.point  <- "point"
        }


        # Layers control.
        if(c.layer.point != "point" & c.layer.line != "line" & c.layer.polygon != "polygon") {

                # Layers control.
                plot.leaflet <- plot.leaflet %>%

                        addLayersControl(

                                baseGroups    = c("OSM (default)", "Toner Lite"),
                                options       = layersControlOptions(collapsed = FALSE)
                        )
        }


        ######################################################################
        # TEXT
        ######################################################################

        if(!is.null(df.text)) {

                # Add permanent text on the map.
                plot.leaflet <- plot.leaflet %>%

                        addLabelOnlyMarkers(

                                data         = df.text,
                                lng          = ~text.lon,
                                lat          = ~text.lat,
                                label        = ~text.label,
                                labelOptions = labelOptions(

                                        noHide    = T,

                                        direction = c.text.direction,

                                        style     = list(

                                                "color"        = c.text.color,
                                                "font-size"    = c.text.font.size,
                                                "font-style"   = "bold",
                                                "box-shadow"   = "5px 5px rgba(0,0,0,0.25)",
                                                "border-color" = "rgba(0,0,0,0)"
                                        ),

                                        opacity    = n.text.opacity
                                )
                        )
        }


##############################################################################
# PRINT LEAFLET.
##############################################################################

        # Print leaflet in Viewer.
        print(plot.leaflet)


##############################################################################
# SAVE LEAFLET.
##############################################################################

        # Save kaart as HTML.
        if(b.save.leaflet) {

                # File name.
                c.file.html <- paste0(

                        path.leaflets,

                        ifelse(b.add.date, paste0(format(Sys.time(), "%Y %m %d"), " - "), ""),

                        ifelse(b.add.time, paste0(format(Sys.time(), "%H %M %S"), " - "), ""),

                        c.leaflet.title,

                        ".html"
                )


                htmlwidgets::saveWidget(

                        widget        = plot.leaflet,
                        file          = c.file.html,
                        selfcontained = TRUE
                )


                if(b.save.pdf) {

                        c.file.pdf <- paste0(

                                path.deliverables,

                                ifelse(b.add.date, paste0(format(Sys.time(), "%Y %m %d"), " - "), ""),

                                ifelse(b.add.time, paste0(format(Sys.time(), "%H %M %S"), " - "), ""),

                                c.leaflet.title,

                                ".pdf"
                        )


                        webshot(c.file.html, c.file.pdf, vwidth = 297*n.zoom.pdf, vheight = 210*n.zoom.pdf)

                        # Alternative: Wrapper around webshot
                        #mapview::mapshot(plot.leaflet, file = "mapshot.pdf")
                }


                cat("Leaflet saved.\n\n")
        }



##############################################################################
# RETURN.
##############################################################################

        # return(
        #         plot.leaflet
        #         #ifelse(!is.null(df.point), df.point, ifelse(!is.null(df.polygon), df.polygon, ifelse(!is.null(df.line), df.line, NULL)))
        # )

        }
