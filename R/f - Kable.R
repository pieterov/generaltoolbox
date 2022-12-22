#' @title Print Kable in R-Markdown
#'
#' @description Prints Kable in R-Markdown (PDF only, not HTML).
#'
#' @author Pieter Overdevest
#'
#' @param df.input Data frame to put in kable table.
#' @param c.caption Add nice caption.
#' @param c.position Options: "center", "left" (default: "center")
#' @param v.align Vector containing "l", "c", "r" indicating how to align each column (default: NULL)
#' @param v.width.cm Width of column (default: 2)
#' @param n.angle Angle of text in header (default: NULL).
#' @param n.font.size Font size (default: 8).
#' @param c.latex_options String of latex options (default: "basic").
#' @param v.grey.col Column numbers to give grey background (default: NULL).
#' @param b.grey.col Add grey column to columns with total in header? (default: TRUE).
#' @param n.top Number of rows to print (default: "all").
#'
#' @returns Kable output.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' kable.output <- f_kable(
#'
#'        df.input            = mtcars,
#'        c.caption           = "Add nice caption",
#'        c.position          = "center",
#'        v.align             = NULL,
#'        v.width.cm          = 2,
#'        n.angle             = NULL,
#'        n.font.size         = 8,
#'        c.latex_options     = "basic",
#'        v.grey.col          = NULL,
#'        b.grey.col          = TRUE,
#'        n.top               = "all"
#' )


        #################################################################################
        # FUNCTION.
        #################################################################################

        f_kable <- function(

                df.input,
                c.caption           = "Add nice caption through 'c.caption'",
                c.position          = "center",
                v.align             = NULL,     # Vector containing "l", "c", "r" indicating how to align each column
                v.width.cm          = 2,
                n.angle             = NULL,
                n.font.size         = 8,
                c.latex_options     = "basic",
                v.grey.col          = NULL,
                b.grey.col          = TRUE,     # Add grey column to columns with total in header?
                n.top               = "all"     # Optional, number of rows to print. Default print all.
        ) {

#########################################################################
# Test Only!
#########################################################################

        # Altijd!
        # c.caption           = "Add nice caption through 'c.caption'"
        # c.position          = "center"
        # v.width.cm          = 2
        # n.angle             = NULL
        # n.font.size         = 8
        # c.latex_options     = "basic"
        # v.grey.col          = NULL
        # b.grey.col          = TRUE
        # n.top               = 35

        # c.caption   = NULL
        # c.position  = "left"
        # v.width.cm  = 2
        # n.angle     = NULL
        # n.font.size = 10
        # v.grey.col  = NULL

        # df.input        = df.input.for.kable %>%
        #
        #         mutate(
        #                 across(c(5, 6, 8, 9, 10), format, nsmall = 2, big.mark = ".", decimal.mark = ","),
        #                 across(c(7),              format, nsmall = 1, big.mark = ".", decimal.mark = ",")
        #         )
        # c.caption       = NULL
        # c.position      = "left"
        # v.align         = c(rep("c", 4), rep("r", 2), rep("c", 1), rep("r", 3))
        # v.width.cm      = c(rep(2, 4), NA, rep(5, 5))
        # n.font.size     = 8
        # c.latex_options = "striped"

#########################################################################
# Error Checks.
#########################################################################

        if(!is.null(v.align)) {

                if(length(v.align) != ncol(df.input)) {

                        f_stop(
                                "Note, v.align must have the same number of values (like: 'l', 'c', 'r') as columns",
                                "in df.input!"
                        )
                }

                if(!all(v.align %in% c("l", "c", "r"))) {

                        f_stop(
                                "Note, v.align can only contain values like,  'l', 'c', 'r'!"
                        )
                }
        }


        if(!is.null(v.width.cm)) {

                if(length(v.width.cm) > ncol(df.input)) {

                        f_stop(
                                "Note, v.width.cm cannot have more values ({length(v.width.cm)}), than columns ",
                                "in df.input ({ncol(df.input)})!"
                        )
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


        # Remove total from df.input if present.
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
        if(n.top != "all") {

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

        } else {

                df.output <- rbind(

                        df.output,

                        df.total
                )
        }


        # Add 'cm' to each non-NA value in v.width.cm.
        if(!is.null(v.width.cm)) {

                v.width <- sapply(v.width.cm, function(x) {ifelse(!is.na(x), paste0(x, "cm"), NA)})

        }

#########################################################################
# Processing
#########################################################################

        kable.output <- df.output %>%

                # Do not show row names.
                kbl(
                        row.names = FALSE,
                        caption   = c.caption,
                        align     = v.align,
                        longtable = TRUE
                ) %>%

                kable_styling(

                        full_width        = F,
                        position          = c.position,
                        font_size         = n.font.size,
                        latex_options     = c("HOLD_position", "repeat_header", c.latex_options)
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
                )


        # Add grey color to columns containing total in the header, if b.grey.col is TRUE.
        if(b.grey.col) {


                kable.output <- kable.output %>%

                        column_spec(

                                column     = grep("tot[a]{1,2}l", names(df.output),  ignore.case = TRUE),
                                bold       = TRUE,
                                background = "#E8E8E8"
                        )
        }


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


        # Pas kolombreedte aan.
        if(!is.null(v.width.cm)) {

                if(length(v.width.cm) == 1) {

                        kable.output <- kable.output %>%

                                column_spec(

                                        column = seq(from = 1, to = ncol(df.output)),
                                        width  = v.width
                                )
                } else {

                        for(i in seq_along(v.width)) { # i = 1

                                if(!is.na(v.width.cm[i])) {

                                        kable.output <- kable.output %>%

                                                column_spec(

                                                        column = i,
                                                        width  = v.width[i]
                                                )
                                }
                        }
                }
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

