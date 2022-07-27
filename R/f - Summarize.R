#################################################################################
# NAME:         FUNCTION - SUMMARIZE
# AUTHOR:       Pieter Overdevest.
# DESCRIPTION:  Give basic info on columns in data frame.
#################################################################################

        f_summarize <- function(

                df.input,
                b.view   = TRUE,
                b.return = FALSE,
                n.char   = "all",
                n.freq   = 3

                ) {

                # Testing
                # df.input = df.source #%>% select(23) %>% head(10)
                # b.view   = TRUE
                # b.return = FALSE
                # n.char   = "all"
                # n.freq   = 3

                # Error check.
                if(!"data.frame" %in% class(df.input)) {

                        stop("Note, the input must be a data frame!")
                }


                v.temp1 <- sapply(df.input, class)
                v.temp2 <- names(v.temp1[v.temp1 %in% c("list")])

                if(length(v.temp2) > 0) {

                        for (c.temp in v.temp2) { # c.temp <- v.temp2[1]

                                df.input <- df.input %>%

                                        mutate(
                                                !!c.temp := c.temp %>% get() %>% unlist() %>% paste(collapse = ",")
                                        )
                        }



                        warning(glue(

                                "De volgende {length(v.temp2)} kolommen zijn van het type 'list'. De waarden ",
                                "in deze kolommen zijn aan elkaar geplakt om f_summary te voltooien:\n",
                                "{f_paste(v.temp2)}"
                        ))
                }


                # Get info per column.
                df.info.per.column <- tibble(feature = names(df.input)) %>%

                        mutate(
                                class = sapply(

                                        df.input, function(v.temp) {

                                                paste(class(v.temp), collapse = "|")
                                        }),

                                n.na = sapply(

                                        df.input, function(v.temp) {

                                                sum(is.na(v.temp), na.rm = TRUE)

                                        }),

                                n.zero = sapply(

                                        df.input, function(v.temp) { # v.temp <- df.input[1]

                                                sum(v.temp == 0, na.rm = TRUE)

                                        }),

                                n.unique = sapply(

                                        df.input, function(v.temp) {

                                                length(unique(v.temp))

                                        }),

                                n.tot = nrow(df.input),

                                min = sapply(

                                        df.input, function(v.temp) {

                                                ifelse("numeric" %in% class(v.temp), min(v.temp), NA)
                                        }),

                                max = sapply(

                                        df.input, function(v.temp) {

                                                ifelse("numeric" %in% class(v.temp), max(v.temp), NA)
                                        }),

                                example = sapply(

                                        df.input, function(v.temp) { # v.temp = df.input[["approver"]]

                                                n.freq.used <- min(

                                                        n.freq,

                                                        v.temp %>% unique() %>% length()
                                                )

                                                v.temp %>%

                                                        f_unique(
                                                                b.freq = TRUE,
                                                                n.char = n.char
                                                        ) %>%

                                                        .[1:n.freq.used] %>%

                                                        f_paste(c.and = "", b.sort = FALSE)
                                        })
                        ) %>%

                        arrange(class, desc(n.unique))


                if(b.view & !b.return) {View(df.info.per.column)}
                if(b.return)           {return(df.info.per.column)}

        }

