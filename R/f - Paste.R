#################################################################################
# NAME:         FUNCTION - Paste.
# AUTHOR:       Pieter Overdevest
# DESCRIPTION:  Concatenate items in vector.
#################################################################################

        f_paste <- function(v.string,
                            c.collapse   = ",",
                            c.and        = "and",
                            b.capitalize = FALSE,
                            b.quotation  = FALSE,
                            b.sort       = TRUE,
                            b.unique     = FALSE,
                            n.top        = NULL
                            ) {


#########################################################################
# TEST ONLY
#########################################################################

        # ALWAYS
        # c.collapse   = ","
        # c.and        = "and"
        # b.capitalize = FALSE
        # b.quotation  = FALSE
        # b.sort       = TRUE
        # b.unique     = FALSE
        # n.top        = NULL

        # Set1
        # v.string    = df.temp$ID
        # b.quotation = TRUE

        # v.string = "Unexpected zero value in Variant Price (12)"
        # n.top    = 10


#########################################################################
# INITIALIZATION
#########################################################################

        # Return NULL als input NULL is.
        if(is.null(v.string) | length(v.string) == 0) return(NULL)


        # Verwijder NA en "" uit lijst.
        v.string <- v.string[!is.na(v.string)]
        v.string <- v.string[v.string != ""]


        # Remove duplicates, if required.
        if(b.unique) {

                v.string <- unique(v.string)
        }


        # Sort words, if required.
        if(b.sort) {

                v.string <- sort(v.string)
        }


        # Capitalize words, if required.
        if(b.capitalize) {

                v.string <- stri_trans_totitle(v.string)
        }


        # Add quotation, if required.
        if(b.quotation) {

                v.string <- sapply(v.string, function(x) {

                paste0("'", x, "'")

                })
        }

        # Determine c.and.
        if(length(v.string) == 1) {

                c.and.updated <- ""

        } else if(length(v.string) == 2) {

                if(c.and == "") {

                        c.and.updated <- paste0(", ")

                } else {

                        c.and.updated <- paste0(" ", c.and, " ")
                }

        } else {
                c.and.updated <- paste0(c.collapse, " ", c.and, " ")
        }


#########################################################################
# Concatenate
#########################################################################

        # Create concatenated string based on n.top.
        if(is.null(n.top)) {

                c.string <- paste0(

                        paste(head(v.string, -1), collapse = paste0(c.collapse, " ")),

                        c.and.updated,

                        tail(v.string, 1)
                )

        } else if (length(v.string) <= n.top) {

                c.string <- paste0(

                        paste(head(v.string, -1), collapse = paste0(c.collapse, " ")),

                        c.and.updated,

                        tail(v.string, 1)
                )

        } else {

                c.string <- paste0(

                        paste(v.string[1:n.top], collapse = paste0(c.collapse, " ")),

                        ", ... and ", length(v.string)-n.top, " more ..."

                )
        }


#########################################################################
# Return xxxxx.
#########################################################################

        return(c.string)

}
