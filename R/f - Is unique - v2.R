#################################################################################
# NAME:         FUNCTION - Is unique.
# AUTHOR:       Pieter Overdevest.
# DESCRIPTION:  Are all elements in vector unique?
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
