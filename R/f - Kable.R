#################################################################################
# NAME:         FUNCTION - Kable.
# AUTHOR:       Pieter Overdevest.
# DESCRIPTION:  Print Kable in R-Markdown.
#################################################################################

        f_kable <- function(df.input,
                            c.caption,
                            c.position  = "center",
                            c.width     = "2cm",
                            n.angle     = NULL,
                            n.font.size = 8,
                            n.top       = 35        # Optional, number of rows to print. Default print all.
                            ) {

#########################################################################
# Test Only!
#########################################################################

        # Altijd!
        # c.position  = "center"
        # c.width     = "2cm"
        # n.angle     = NULL
        # n.font.size = 8
        # n.top       = 35

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
        if(
                nrow(df.output) > n.top
        ) {

                df.output <- rbind(

                        df.output %>% head(n.top),

                        "...",

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
                        caption   = c.caption) %>%

                kable_styling(

                        full_width    = F,
                        position      = c.position,
                        font_size     = n.font.size,
                        latex_options = "hold_position"
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

                        column     = 1,
                        bold       = TRUE,
                        background = "#E8E8E8"
                        ) %>%

                column_spec(

                        column     = grep("tot[a]{1,2}l", names(df.output),  ignore.case = TRUE),
                        bold       = TRUE,
                        background = "#E8E8E8"
                        )


        # Pas kolombreedte aan van kolom 2 t/m n, indien df.output meer dan 1 kolom bevat.
        if(ncol(df.output) > 1) {

                kable.output <- kable.output %>%

                        column_spec(

                                column = seq_along(names(df.output))[2:ncol(df.output)],
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

