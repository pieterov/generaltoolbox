#################################################################################
# FUNCTION - Are alle elements unique?
#
# NAME:         Pieter Overdevest.
# DATE:         Dec 23, 2020.
# VERSION:      2.
#
# VERSIONS:     v1 -    Start
#               v2 -    NA toegevoegd. Als hele vector NA of NULL bevat wordt TRUE teruggestuurd.
#
#################################################################################

f_is_unique <- function(v.vector,
                        v.excluding = c(NULL, NA)
                        ) {
        
        #########################################################################
        # TEST
        #########################################################################
        
        #v.vector <- v.temp

        
        #########################################################################
        # TEST
        #########################################################################
        
        # Verwijder elementen die niet meegenomen moeten worden, bijv NA en NULL.
        v.output <- v.vector[!v.vector %in% v.excluding]

        return(length(unique(v.output)) == length(v.output))
        
        }