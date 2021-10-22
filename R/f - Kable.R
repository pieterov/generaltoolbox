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
                            n.font.size = NULL,
                            n.max       = 15        # Optional, number of rows to print. Default print all.
                            ) {

#########################################################################
# Test Only!
#########################################################################

        # Altijd!
        # c.position = "center"
        # c.width    = "2cm"
        # n.angle    = NULL
        # n.max      = Inf

        # Set 1
        # df.input  <-  df.temp
        # c.caption <- "Test"

        # Set 2
        # df.input  = df.analyse.1 %>% filter(!in.imagelist, image.type == "streetsmart") %>% f_table(c.ver = "bord.type.rood") %>% head(15)
        # df.input  = df.input %>% tail(-1)
        # c.caption = "Borden zonder image op picture server en met link naar StreetSmart (bord type rood):"
        # n.max     = 10

#########################################################################
# Initialization.
#########################################################################

        # Indien het dataframe leeg is, voeg dan een lege rij toe, om error te voorkomen.
        if(nrow(df.input) == 0) {

                df.input[nrow(df.input) + 1, ] <- NA
        }

        # Indien het dataframe meer dan n.max regels bevat, toon dan alleen een
        # deel van boven- en van onderkant.
        if(nrow(df.input) > n.max) {

                df.input <- rbind(

                        a = df.input %>% head(ceiling(n.max/2)+1),
                        b = df.input %>% tail(floor(n.max/2))
                        ) %>%

                        mutate_all(as.character)

                df.input[floor(n.max/2)+1,] <- "..."

        }

#########################################################################
# Processing
#########################################################################

        df.output <- df.input %>%

                kbl(caption = c.caption) %>%

                kable_styling(full_width    = F,
                              position      = c.position,
                              font_size     = n.font.size,
                              latex_options = "hold_position") %>%

                row_spec(

                        row        = 0,
                        bold       = TRUE,
                        background = "#D3D3D3"
                        ) %>%

                row_spec(

                        row        = grep("total|totaal", tolower(df.input %>% pull(1))),
                        bold       = TRUE,
                        background = "#E8E8E8"
                        ) %>%

                column_spec(

                        column     = 1,
                        bold       = TRUE,
                        background = "#E8E8E8"
                        ) %>%

                column_spec(

                        column     = grep("total|totaal", tolower(names(df.input))),
                        bold       = TRUE,
                        background = "#E8E8E8"
                        )


        # Alleen als df.input meer dan 1 kolom bevat.
        if(ncol(df.input) > 1) {

                df.output <- df.output %>%

                        column_spec(

                                column = seq_along(names(df.input))[2:ncol(df.input)],
                                width  = c.width
                        )
                }


        # Rotate headers.
        if(!is.null(n.angle)) {

                df.output <- df.output %>%

                        row_spec(0, angle = n.angle)

                }


#########################################################################
# Return
#########################################################################

        return(df.output)

        }

