#################################################################################
# NAME:         FUNCTION - INFO PER COLUMN
# AUTHOR:       Pieter Overdevest.
# DESCRIPTION:  Give basic info on columns in data frame.
#################################################################################

        f_info_per_column <- function(

                df.input,
                b.view   = TRUE,
                b.return = FALSE,
                n.char   = "all",
                n.freq   = 3

                ) {

                # Error check.
                if(!"data.frame" %in% class(df.input)) {

                        stop("Note, the input must be a data frame!")
                }

                # Get info per column.
                Info_per_Column <- data.frame(feature = names(df.input)) %>%

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

                                        df.input, function(v.temp) {

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


                if(b.view & !b.return) {View(Info_per_Column)}
                if(b.return)           {return(Info_per_Column)}

        }

