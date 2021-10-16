#################################################################################
# FUNCTION - Retrieve element number from list.
#
# NAME:     Pieter Overdevest.
# DATE:     Dec 21, 2020.
# VERSION:  1.
#
# DESCRIPTION: Retrieve which element in de list of data frames contains grouping information.
#
#################################################################################

f_retrieve_element_number_in_list <- function(l.df,
                                              v.element,
                                              v.search) {
        

#################################################################################
# Test
#################################################################################

        # l.df      <- df.dcs
        # v.element <- c("drager.fid", "groep.id")
        # v.search  <- c("22a1754c-bdae-4578-8128-63522128982b", "1")
        
        
#################################################################################
# Error check
#################################################################################

        if(length(v.element) != length(v.search))
                stop("Let op, v.element moet even lang zijn als v.search!")
        
        
#################################################################################
# Initializatie
#################################################################################

#################################################################################
# Main body
#################################################################################

        v.result <- lapply(l.df, function(df.temp) { # df.temp <- l.df[[1]]
                
                l.result.element <- mapply(function(c.element, c.search) {
                        
                        ifelse(
                                first(df.temp[[c.element]]) == c.search,
                                TRUE, FALSE)
                        },
                        
                        c.element = v.element,  # c.element = v.element[1]
                        c.search  = v.search    # c.search  = v.search[1]
                        )
                
                return(all(l.result.element))
                
        }) %>% unlist()
        
        
#################################################################################
# Return
#################################################################################
        
        return(which(v.result))
        
}
