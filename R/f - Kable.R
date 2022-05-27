#################################################################################
# NAME:         FUNCTION - Kable.
# AUTHOR:       Pieter Overdevest.
# DESCRIPTION:  Print Kable in R-Markdown.
#################################################################################

        f_kable <- function(

                df.input,
                c.caption           = "Add nice caption through 'c.caption'",
                c.position          = "center",
                v.align             = NULL,     # Vector containing "l", "c", "r" indicating how to align each column
                c.width             = "2cm",
                c.width.col.1       = NULL,
                n.angle             = NULL,
                n.font.size         = 8,
                c.latex_options     = "basic",
                v.grey.col          = NULL,
                n.top               = 35        # Optional, number of rows to print. Default print all.
        ) {

#########################################################################
# Test Only!
#########################################################################

        # Altijd!
        # c.caption           = "Add nice caption through 'c.caption'"
        # c.position          = "center"
        # c.width             = "2cm"
        # n.angle             = NULL
        # n.font.size         = 8
        # c.latex_options     = "basic"
        # v.grey.col          = NULL
        # n.top               = 35


        # Set 1
        # df.input  = df.temp
        # c.caption = "Test"

        # Set 2
        # df.input  = df.analyse.1 %>% filter(!in.imagelist, image.type == "streetsmart") %>% f_table(c.ver = "bord.type.rood") %>% head(15)
        # df.input  = df.input %>% tail(-1)
        # c.caption = "Borden zonder image op picture server en met link naar StreetSmart (bord type rood):"
        # n.max     = 10

        # Set 3
        # n.angle     = 45
        # df.input    = df.dummy
        # c.caption   = "test"

        # Set 4
        # df.input  = df.temp
        # c.caption = "Aantal borden per project (incl. historie):"
        # c.width   = "3cm"

        # #28 in M6
        # df.input  = df.dubbel.bord %>% f_table(c.ver = "bord.type")
        # c.caption = "Aantal borden die mogelijk dubbele borden betreffen, per bord type:"
        # c.width   = "2cm"
        # n.top     = 20

        # c.caption   = NULL
        # c.position  = "left"
        # c.width     = "2.5cm"
        # n.angle     = NULL
        # n.font.size = 10
        # v.grey.col  = NULL

#########################################################################
# Error Checks.
#########################################################################

        if(!is.null(v.align)) {

                if(length(v.align) != ncol(df.input)) {

                        stop(paste0(

                                "Note, v.align must have the same number of values (like: 'l', 'c', 'r') as columns in df.input!"
                        ))
                }

                if(!all(v.align %in% c("l", "c", "r"))) {

                        stop(paste0(

                                "Note, v.align can only contain values like,  'l', 'c', 'r'!"
                        ))
                }
        }

#########################################################################
# Initialization.
#########################################################################

        # Indien het dataframe leeg is, voeg dan een lege rij toe, om error te voorkomen.
        if(nrow(df.input) == 0) {

                df.input[nrow(df.input) + 1, ] <- NA
        }


        # Get df.total, if total is present in last row, else df.total is NULL.
        df.input <- df.input %>% mutate_all(as.character)
        df.total <- df.input %>% tail(1)


        # Remove total from df.output if present.
        if(
                any(
                        grepl(
                                pattern     = "^tot[a]{1,2}l",
                                x           = df.total %>% unlist(),
                                ignore.case = TRUE
                        )
                )
        ) {

                # Verwijder total row from df.input (last row).
                df.output <- df.input %>% head(-1)

        } else {

                df.total  <- NULL
                df.output <- df.input
        }



        # Let op, als het dataframe meer dan n.top rijen bevat.
        # Op 28 feb 2022 de '...' vervangen door '---' (zie hieronder),
        # dit voorkomt de '...llcolor[HTML]E8E8E831' error in tabel,
        # zie Slack (QC / 25 feb 2022 / Mary).
        if(
                nrow(df.output) > n.top
        ) {

                df.output <- rbind(

                        df.output %>% head(n.top),

                        "---",

                        df.total
                        )
        } else {

                df.output <- rbind(

                        df.output %>% head(n.top),

                        df.total
                )
        }


#########################################################################
# Processing
#########################################################################

        kable.output <- df.output %>%

                # Do not show row names.
                kbl(
                        row.names = FALSE,
                        caption   = c.caption,
                        align     = v.align
                ) %>%

                kable_styling(

                        full_width        = F,
                        position          = c.position,
                        font_size         = n.font.size,
                        latex_options     = c("HOLD_position", c.latex_options)
                ) %>%

                row_spec(

                        row        = 0,
                        bold       = TRUE,
                        background = "#D3D3D3"
                ) %>%

                row_spec(

                        row        = grep("tot[a]{1,2}l", df.output %>% pull(1), ignore.case = TRUE),
                        bold       = TRUE,
                        background = "#E8E8E8"
                ) %>%

                column_spec(

                        column     = grep("tot[a]{1,2}l", names(df.output),  ignore.case = TRUE),
                        bold       = TRUE,
                        background = "#E8E8E8"
                )


        # Add grey color to left column.
        if(!is.null(v.grey.col)) {

                kable.output <- kable.output %>%

                        # Deze actie eerder hidden omdat we latex error kregen. Echter, op 28 feb 2022 weer 'aan' gezet
                        # omdat de error veroorzaakt lijkt te worden door de '...' in regel 100. Als we deze vervangen
                        # door '---' gaat het goed.
                        column_spec(

                                column     = v.grey.col,
                                bold       = TRUE,
                                background = "#E8E8E8"
                        )


        }

        # Pas kolombreedte aan van kolom 1.
        if(!is.null(c.width.col.1)) {

                kable.output <- kable.output %>%

                        column_spec(

                                column = 1,
                                width  = c.width.col.1
                        )
        }


        # Pas kolombreedte aan van kolom 2 t/m n, indien df.output meer dan 1 kolom bevat.
        if(ncol(df.output) > 1 & !is.null(c.width)) {

                kable.output <- kable.output %>%

                        column_spec(

                                column = seq(from = 2, to = ncol(df.output)),
                                width  = c.width
                        )
                }


        # Rotate headers.
        if(!is.null(n.angle)) {

                kable.output <- kable.output %>%

                        row_spec(0, angle = n.angle)

                }


#########################################################################
# Return
#########################################################################

        return(kable.output)

        }

