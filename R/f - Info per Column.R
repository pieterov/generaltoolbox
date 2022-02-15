#################################################################################
# NAME:         FUNCTION - INFO PER COLUMN
# AUTHOR:       Pieter Overdevest.
# DESCRIPTION:  Give basic info on columns in data frame.
#################################################################################

        f_info_per_column <- function(df.input) {

                # Error check.
                if(!"data.frame" %in% class(df.input)) {

                        stop("Note, the input must be a data frame!")
                }

                # Get info per column.
                df.output <- tibble(feature = names(df.input)) %>%

                        mutate(
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

                                        })
                        )

                return(df.output)
        }

