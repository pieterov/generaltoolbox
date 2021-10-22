#################################################################################
# NAME:         FUNCTION - Return sorted unique elements.
# AUTHOR:       Pieter Overdevest
# DESCRIPTION:  Return sorted unique elements.
#################################################################################

        f_unique <- function(v.vector) {

                v.result <- sort(unique(v.vector))

                if(length(v.result) > 10000)
                        stop("Meer dan 10,000 items, bedoelde je 'f_is_unique'?")

                return(v.result)

        }
