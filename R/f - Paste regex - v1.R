#################################################################################
# FUNCTION - Put pastes items in regex separared by "|".
#
# NAME:     Pieter Overdevest.
# DATE:     Jan 13, 2019.
# VERSION:  1.
#
#################################################################################

f_paste_regex <- function(v.string,
                          c.pre        = "^",
                          c.collapse   = "|",
                          c.post       = "$",
                          b.sort       = TRUE) {
        
        
#########################################################################
# TEST ONLY
#########################################################################

        # v.bord.type.fietspad <- c("G09", "G10", "G11", "G12", "G13", "G14")
        # v.bord.type.voetpad  <- c("G15", "G07", "G08", "G09", "G10")
        # v.string <- c(v.bord.type.fietspad, v.bord.type.voetpad)
        
        
#########################################################################
# INITIALIZATION
#########################################################################
        
        # Return NULL als input NULL is.
        if(is.null(v.string)) return(NULL)
        
        # Verwijder NA en "" uit lijst.
        v.string <- v.string[!is.na(v.string)]
        v.string <- v.string[v.string != ""]

        # Sort words, if required.
        if(b.sort)
                v.string <- sort(v.string)
        
        # Remove doubles.
        v.string <- unique(v.string)
        
        
#########################################################################
# Concatenate
#########################################################################
        
        c.string <- paste0(c.pre,
                           paste(v.string, collapse = paste0(c.post, "|", c.pre)),
                           c.post)
        
        
#########################################################################
# Return xxxxx.
#########################################################################
        
        return(c.string)
        
}