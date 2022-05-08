#################################################################################
# NAME:         FUNCTION - Give basic info of vector.
# AUTHOR:       Pieter Overdevest
# DESCRIPTION:  Provide informartion on vector.
#################################################################################

    f_vector_info <- function(v,
                              name,
                              n.top,
                              show.freq) {

##############################################################################
# Error check.
##############################################################################

        # v         = df.discount.too.low$discount
        # name      = "df.discount.too.low$discount"
        # n.top     = 10
        # show.freq = TRUE

        # v <- c(0, 2, 2, NA, NA, NA, 0/0, -0/0, -0/0, 0/0, 6/0, -7/0, -8/0, 9/0, 10/0)
        # v <- c(NA, NA, NA)
        # v <- c(0/0, 0/0, 0/0, 0/0)
        # v <- c(6/0, 7/0, 8/0, 9/0, 10/0)


##############################################################################
# Error check.
##############################################################################

        if(!is.numeric(n.top) & n.top != "all") {

                stop("n.top moet valide waarde bevatten: integer of 'all'")
        }


##############################################################################
# Analyse data.
##############################################################################

        # Calculate basic info.
        df.basic.info <- data.frame(

                x  = c(
                        "Total elements:",
                        "Unique elements:",
                        "0:",
                        "Empty:",
                        "NA:",
                        "NaN:",
                        "Inf(-):",
                        "Inf(+):"
                ),

                y = c(
                        length(v),
                        length(unique(v)),
                        sum(v == 0, na.rm = TRUE),
                        sum(v == "", na.rm = TRUE),
                        sum(v %in% NA),
                        sum(v %in% NaN),
                        sum(v %in% -Inf),
                        sum(v %in% Inf)
                ),

                z = c(
                        "",
                        "",

                        paste(
                                round(
                                        sum(v == 0, na.rm = TRUE) / length(v) * 100,
                                        digits = 1
                                ),
                                "%"
                        ),

                        paste(
                                round(
                                        sum(v == "", na.rm = TRUE) / length(v) * 100,
                                        digits = 1
                                ),
                                "%"
                        ),

                        paste(
                                round(
                                        sum(v %in% NA) / length(v) * 100,
                                        digits = 1
                                ),
                                "%"
                        ),

                        paste(
                                round(
                                        sum(v %in% NaN) / length(v) * 100,
                                        digits = 1
                                ),
                                "%"
                        ),

                        paste(
                                round(
                                        sum(v %in% -Inf) / length(v) * 100,
                                        digits = 1
                                ),
                                "%"
                        ),

                        paste(
                                round(
                                        sum(v %in% Inf) / length(v) * 100,
                                        digits = 1
                                ),
                                "%"
                        )
                )
        )


        names(df.basic.info) <- c("============================", "======", "======")

        # Print header.
        cat(paste0("\n ", name, "\n"))

        cat(paste0(rep(" ", 29), collapse = ""), "n      perc\n")

        # Show in console, left align.
        print(
                x         = df.basic.info,
                row.names = FALSE,
                right     = FALSE
        )


        # Show frequency table.
        if (show.freq) {

                # Replace any NA by "NA", and NaN by "NaN"
                v[v %in% NA]  <- "NA "
                v[v %in% NaN] <- "NaN "
                v[v %in% Inf] <- "Inf "

                # Calculate frequency of levels in vector.
                df.freq.source <- as.data.frame(table(v)) %>%

                        arrange(desc(Freq), v) %>%

                        mutate(perc = Freq / sum(Freq) * 100)


                df.dots <- data.frame(

                        v    = "...",
                        Freq = "...",
                        perc = "..."
                )


                df.total <- data.frame(

                        v    = c("----------------", "TOTAL"),
                        Freq = c("------", sum(df.freq.source$Freq)),
                        perc = c("------", paste(round(sum(df.freq.source$perc), digits = 1), "%"))
                )


                df.freq <- df.freq.source %>%

                        mutate(
                                perc = paste(round(df.freq.source$perc, digits = 1), "%")
                        ) %>%

                        head(n.top)


                # Puntjes toevoegen als n.top een getal is.
                if(is.numeric(n.top)) {

                        df.freq <- rbind(df.freq, df.dots)
                }

                # Total toevoegen.
                df.freq <- rbind(df.freq, df.total)

                names(df.freq) <- c("============================", "======", "======")


                # Header frequency table.
                cat(
                        paste0(

                                "\n Frequency table ",

                                if(is.numeric(n.top)) {

                                        if(nrow(df.freq.source) < n.top) {

                                                "(all items): "

                                        } else {

                                                paste0("(Top-" , n.top, "):    ")
                                        }

                                } else {

                                        "(all items): "
                                },

                                "n      perc\n"
                        )
                )



                print(
                        x         = df.freq,
                        row.names = FALSE,
                        right     = FALSE
                )

        }
}
