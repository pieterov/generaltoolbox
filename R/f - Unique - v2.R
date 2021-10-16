#################################################################################
# FUNCTION - Return sorted unique elements
#
# NAME:         Pieter Overdevest.
# DATE:         Dec 23, 2020.
# VERSION:      2.
#
# VERSIONS:     v1 - start
#               v2 - removed 'cat' from return.
#
#################################################################################

        f_unique <- function(v.vector) {
                
                v.result <- sort(unique(v.vector))
                
                if(length(v.result) > 10000)
                        stop("Meer dan 10,000 items, bedoelde je 'f_is_unique'?")
        
                return(v.result)
                
        }