#' @title Provide information on vector
#'
#' @description Provides informartion on vector. Typically, you will use f_info() and not use f_vector_info().
#' The function f_info() calls f_vector_info() for each feature in the object entered in f_info().
#'
#' @author Pieter Overdevest
#'
#' @param v.input Vector of items to check statistics of.
#' @param name Name to print above the table.
#' @param n.top Max number of items to show in the list.
#' @param show.freq Should frequency be shown?
#' @param c.sort.by How to sort the items in the frequency table, by its 'frequency' or 'value' (default: 'frequency').
#' @param n.width Number of characters of the items to show in the list.
#'
#' @returns Nothing. Only prints to console.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_vector_info(
#'
#'     v.input   = c(0, 2, 2, NA, NA, NA, 0/0, -0/0, -0/0, 0/0, 6/0, -7/0, -8/0, 9/0, 10/0),
#'     name      = "Pieter",
#'     n.top     = 10,
#'     show.freq = TRUE,
#'     c.sort.by = 'frequency',
#'     n.width   = 29
#')

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_vector_info <- function(

                v.input,
                name,
                n.top,
                show.freq,
                n.width,
                c.sort.by
        ) {


        ##############################################################################
        # TESTING.
        ##############################################################################

        # ALTIJD:
        # name      = "pieter"
        # n.top     = 10
        # show.freq = TRUE
        # n.width   = 29
        # c.sort.by = "frequency"

        # v.input   = df.datachamp.baseline$id.sku.vendor[1:3]
        # name      = "df.datachamp.baseline$id.sku.vendor"
        # n.top     = 10
        # show.freq = TRUE

        # v.input         = l.result$bord.type
        # name      = "l.result$bord.type"
        # n.top     = 10
        # show.freq = TRUE

        # v.input <- c(0, 2, 2, NA, NA, NA, 0/0, -0/0, -0/0, 0/0, 6/0, -7/0, -8/0, 9/0, 10/0)
        # v.input <- c(NA, NA, NA)
        # v.input <- c(0/0, 0/0, 0/0, 0/0)
        # v.input <- c(6/0, 7/0, 8/0, 9/0, 10/0)
        # v.input <- as.factor(LETTERS[1:10])
        # v.input <- c(today(), today(), today(), today()+1, today()+1, today()+2, NA, Inf)
        # v.input <- c(now(), now(), now(), today()+1, today()+1, today()+2, NA, Inf)

        # f_info(
        #         c(0, 2, 2, NA, NA, NA, 0/0, -0/0, -0/0, 0/0, 6/0, -7/0, -8/0, 9/0, 10/0)
        # )

        # f_info(
        #         c(as_datetime("2023 11 06"), -6/0, NA, NA, as_date("2023 11 05"), as_date("2023 10 06"))
        # )

        # f_info(
        #         sample(c("A", "B", "C"), 10000, replace = TRUE) %>% as.factor()
        # )

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
        n.count.true <- nchar(format(length(v.input), big.mark = ","))
        n.count      <- max(3, n.count.true)
        c.class      <- paste(class(v.input), collapse = '/')


        # Reformat data to character, since date(time) gives an error in 'v.input[v.input %in% NA]  <- "NA "',
        # see below. To be sure, all are converted to character.
        # Als het weer een issue wordt dan deze conversie hieronder zetten, bijv. in regel 222. Gedaan, ik heb
        # de regel rond regel 222 toegevoegd.
        #v.input = as.character(v.input)


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
                                length(v.input),
                                length(unique(v.input)),
                                sum(v.input == 0, na.rm = TRUE),
                                sum(as.character(v.input) == "", na.rm = TRUE),
                                sum(v.input %in% NA),
                                sum(v.input %in% NaN),
                                sum(v.input %in% -Inf),
                                sum(v.input %in% Inf)
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
                                                sum(v.input == 0, na.rm = TRUE) / length(v.input) * 100,
                                                digits = 1
                                        ),

                                        round(
                                                sum(as.character(v.input) == "", na.rm = TRUE) / length(v.input) * 100,
                                                digits = 1
                                        ),

                                        round(
                                                sum(v.input %in% NA) / length(v.input) * 100,
                                                digits = 1
                                        ),

                                        round(
                                                sum(v.input %in% NaN) / length(v.input) * 100,
                                                digits = 1
                                        ),

                                        round(
                                                sum(v.input %in% -Inf) / length(v.input) * 100,
                                                digits = 1
                                        ),

                                        round(
                                                sum(v.input %in% Inf) / length(v.input) * 100,
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
                        "\n ", name, " (", c.class, ")\n\n"
                )
        )


        cat(
                paste0(
                        strrep(" ", n.width + n.count - 1), # -1

                        "n",

                        strrep(" ", 2),

                        "perc\n"
                )
        )

        names(df.basic.info) <- c(

                strrep("=", n.width - 1),

                strrep("=", n.count),

                strrep("=", 5)
        )

        # Show in console, left align.
        print(
                x         = df.basic.info,
                row.names = FALSE,
                right     = FALSE
        )

        # Reformat data to character, since date(time) and factor give an error in
        # 'v.input[v.input %in% NA]  <- "NA "', see below. To be sure, all are converted to character.
        v.input = as.character(v.input)

        # Show frequency table.
        if(show.freq) {

                # Replace any NA by "NA", and NaN by "NaN"
                v.input[v.input %in% NA]   <- " NA"
                v.input[v.input %in% NaN]  <- " NaN"
                v.input[v.input %in% -Inf] <- " -Inf"
                v.input[v.input %in% Inf]  <- " Inf"

                # Calculate frequency of levels in vector.
                df.freq.source <- as.data.frame(table(v.input)) %>%

                        {
                                if(c.sort.by == "frequency") {

                                        arrange(., desc(Freq), v.input)

                                } else if(c.sort.by == "value") {

                                        arrange(., v.input)

                                } else {
                                        cat(paste(
                                                "You did not provide a valid value for 'c.sort.by', this must be",
                                                "'frequency' or 'value'."
                                        ))
                                }
                        } %>%

                        mutate(
                                v.input     = as.character(v.input),
                                v.input     = ifelse(

                                        nchar(v.input) >= (n.width - 3),

                                        paste0(substr(v.input, 1, (n.width - 4)), "..."),

                                        v.input
                                ),

                                Freq2 = format(Freq, big.mark = ",", width = n.count),
                                perc  = Freq / sum(Freq) * 100,
                                perc2 = paste0(format(round(perc, digits = 1), width = 4), "%")
                        )



                df.dots <- data.frame(

                        v.input    = "...",
                        Freq = paste0(strrep(" ", n.count - 3), "..."),
                        perc = paste0(strrep(" ", 5       - 3), "...")
                )


                df.total <- data.frame(

                        v.input    = c(

                                strrep("-", n.width-1),
                                "TOTAL"
                        ),

                        Freq = c(
                                strrep("-", n.count),
                                format(length(v.input), big.mark = ",")
                        ),

                        perc = c(
                                strrep("-", 5),
                                " 100%"
                        )
                )


                df.freq <- df.freq.source %>%

                        select(-Freq, -perc) %>%

                        rename(Freq = Freq2, perc = perc2) %>%

                        if(is.numeric(n.top)) head(., n.top) else .


                # Puntjes toevoegen als n.top een getal is, en kleiner of gelijk is aan het aantal rijen in df.freq.
                if(is.numeric(n.top)) {

                        if(nrow(df.freq) >= n.top) {

                                df.freq <- rbind(df.freq, df.dots)
                        }
                }


                # Total toevoegen.
                df.freq       <- rbind(df.freq, df.total)

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
                        right     = TRUE
                )
        }
}
