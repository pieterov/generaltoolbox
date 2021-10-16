#################################################################################
# FUNCTION - Put strings in list.
#
# NAME:         Pieter Overdevest.
# DATE:         Oct 13, 2020.
# VERSION:      2.
#
# COMMENT:      Add option to remove duplicates.
#
#################################################################################

f_paste <- function(v.string,
                    c.collapse   = ",",
                    b.capitalize = FALSE,
                    b.quotation  = FALSE,
                    b.sort       = TRUE,
                    b.unique     = FALSE) {


#########################################################################
# TEST ONLY
#########################################################################

#########################################################################
# INITIALIZATION
#########################################################################

        # Return NULL als input NULL is.
        if(is.null(v.string) | length(v.string) == 0) return(NULL)


        # Verwijder NA en "" uit lijst.
        v.string <- v.string[!is.na(v.string)]
        v.string <- v.string[v.string != ""]


        # Remove duplicates, if required.
        if(b.unique)
                v.string <- unique(v.string)


        # Sort words, if required.
        if(b.sort)
                v.string <- sort(v.string)


        # Capitalize words, if required.
        if(b.capitalize)
                v.string <- stri_trans_totitle(v.string)


        # Add quotation, if required.
        if(b.quotation)

                v.string <- sapply(v.string, function(x) {

                paste0("'", x, "'")

                })


        # Determine c.and.
        if(length(v.string) == 1)
                c.and <- ""

        else if(length(v.string) == 1)
                c.and <- " en "

        else c.and <- ", en "


#########################################################################
# Concatenate
#########################################################################

        c.string <- paste0(

                paste(head(v.string, -1), collapse = paste0(c.collapse, " ")),

                c.and,

                tail(v.string, 1)

                )


#########################################################################
# Return xxxxx.
#########################################################################

        return(c.string)

}
