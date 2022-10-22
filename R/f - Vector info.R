#################################################################################
# NAME:         FUNCTION - Give basic info of vector.
# AUTHOR:       Pieter Overdevest
# DESCRIPTION:  Provide informartion on vector.
#################################################################################

    f_vector_info <- function(v,
                              name,
                              n.top,
                              show.freq,
                              n.width) {

##############################################################################
# Error check.
##############################################################################

        # v         = df.datachamp.baseline$id.sku.vendor[1:3]
        # name      = "df.datachamp.baseline$id.sku.vendor"
        # n.top     = 10
        # show.freq = TRUE

        # v         = l.result$bord.type
        # name      = "l.result$bord.type"
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

        # Initialization. We take max of nchar and 3 to prevent count errors below. Width is at least 3.
        n.count.true <- nchar(format(length(v), big.mark = ","))
        n.count      <- max(3, n.count.true)

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

                y = format(

                        c(
                                length(v),
                                length(unique(v)),
                                sum(v == 0, na.rm = TRUE),
                                sum(v == "", na.rm = TRUE),
                                sum(v %in% NA),
                                sum(v %in% NaN),
                                sum(v %in% -Inf),
                                sum(v %in% Inf)
                        ),

                        width    = n.count-1,

                        big.mark = ","
                ),

                z = c(
                        "", "",

                        paste0(
                                format(

                                c(

                                        round(
                                                sum(v == 0, na.rm = TRUE) / length(v) * 100,
                                                digits = 1
                                        ),

                                        round(
                                                sum(v == "", na.rm = TRUE) / length(v) * 100,
                                                digits = 1
                                        ),

                                        round(
                                                sum(v %in% NA) / length(v) * 100,
                                                digits = 1
                                        ),

                                        round(
                                                sum(v %in% NaN) / length(v) * 100,
                                                digits = 1
                                        ),

                                        round(
                                                sum(v %in% -Inf) / length(v) * 100,
                                                digits = 1
                                        ),

                                        round(
                                                sum(v %in% Inf) / length(v) * 100,
                                                digits = 1
                                        )
                                ),

                                width = 4

                                ),

                                "%"
                        )
                )
        )


        # Print header.
        cat(
                paste0(
                        "\n ", name, " (", class(x[[c.column]]), ")\n\n"
                )
        )


        cat(
                paste0(
                        strrep(" ", n.width + n.count),

                        "n",

                        strrep(" ", 2),

                        "perc\n"
                )
        )

        names(df.basic.info) <- c(

                strrep("=", n.width-1),

                strrep("=", n.count),

                strrep("=", 5)
        )

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

                        mutate(
                                v     = as.character(v),
                                v     = ifelse(

                                        nchar(v) >= (n.width - 3),

                                        paste0(substr(v, 1, (n.width - 4)), "..."),

                                        v
                                ),

                                Freq2 = format(Freq, big.mark = ",", width = n.count),
                                perc  = Freq / sum(Freq) * 100,
                                perc2 = paste0(format(round(perc, digits = 1), width = 4), "%")
                        )



                df.dots <- data.frame(

                        v    = "...",
                        Freq = paste0(strrep(" ", n.count - 3), "..."),
                        perc = paste0(strrep(" ", 5       - 3), "...")
                )

                df.total <- data.frame(

                        v    = c(

                                strrep("-", n.width-1),
                                "TOTAL"
                        ),

                        Freq = c(
                                strrep("-", n.count),
                                format(length(v), big.mark = ",")
                        ),

                        perc = c(
                                strrep("-", 5),
                                " 100%"
                        )
                )


                df.freq <- df.freq.source %>%

                        select(-Freq, -perc) %>%

                        rename(Freq = Freq2, perc = perc2) %>%

                        head(n.top)


                # Puntjes toevoegen als n.top een getal is.
                if(is.numeric(n.top)) {

                        df.freq <- rbind(df.freq, df.dots)
                }


                # Total toevoegen.
                df.freq <- rbind(df.freq, df.total)

                names(df.freq) <- names(df.basic.info)


                c.freq.table <- paste0(

                        " Frequency table ",

                        if(is.numeric(n.top)) {

                                if(nrow(df.freq.source) < n.top) {

                                        "(all items):"

                                } else {

                                        paste0("(Top-" , n.top, "):")
                                }

                        } else {

                                "(all items):"
                        }
                )


                # Header frequency table.
                cat(
                        paste0(
                                "\n", c.freq.table,

                                strrep(" ", n.width + n.count - nchar(c.freq.table)),

                                "n",

                                strrep(" ", 2),

                                "perc\n"
                        )
                )


                print(
                        x         = df.freq,
                        row.names = FALSE,
                        right     = FALSE
                )
        }
}
