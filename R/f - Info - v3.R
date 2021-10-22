#################################################################################
# NAME:         FUNCTION - Info
# AUTHOR:       Pieter Overdevest.
# DESCRIPTION:  Give basic info on vector or data frame.
#################################################################################

        f_info <- function(x,
                           n.top     = 10,
                           show.freq = TRUE) {



                #x <- df.gemeentenaam.na$gemeentenaam.nwb

                #print(x)
                #print(n.top)

                # Als x een dataframe is.
                if (is.data.frame(x)) {

                        for (c.column in colnames(x)) { # c.column <- colnames(x)[1]

                                cat(paste("\n\n\n\nField name: ", c.column, "\n"))

                                f_vector_info(v         = as.character(x[[c.column]]),
                                              name      = c.column,
                                              n.top     = n.top,
                                              show.freq = show.freq) }

                } else {

                        f_vector_info(v         = as.character(x),
                                      name      = deparse(substitute(x)),
                                      n.top     = n.top,
                                      show.freq = show.freq)
                }
        }

