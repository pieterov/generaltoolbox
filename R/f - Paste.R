#' @title Concatenate items in vector
#'
#' @description Concatenate items in vector.
#'
#' @author Pieter Overdevest
#'
#' @param v.string     Vector of items to concatenate.
#' @param c.collapse   (default: ",").
#' @param c.and        (default: "and").
#' @param b.capitalize (default: FALSE).
#' @param b.quotation  (default: FALSE).
#' @param b.sort       (default: TRUE).
#' @param b.unique     (default: FALSE).
#' @param n.top        (default: NULL).
#'
#' @returns Concatenated string.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' c.string <- f_paste(
#'
#'        v.string     = c("A", "B", "C"),
#'        c.collapse   = ",",
#'        c.and        = "and",
#'        b.capitalize = FALSE,
#'        b.quotation  = FALSE,
#'        b.sort       = TRUE,
#'        b.unique     = FALSE,
#'        n.top        = NULL
#' )


#################################################################################
# FUNCTION.
#################################################################################

        f_paste <- function(

                v.string,
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

        # v.string = NA
        # n.top    = 10


#########################################################################
# INITIALIZATION
#########################################################################

        # Verwijder NA en "" uit lijst.
        v.string <- v.string[!is.na(v.string)]
        v.string <- v.string[!is.null(v.string)]
        v.string <- v.string[v.string != ""]

        # Return "" als input NULL is.
        if(
                is.null(v.string) |
                is.na(v.string) |
                length(v.string) == 0
        )
                return("")

        # Return "" als input "" is.
        #if(all(v.string == "")) return("")

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

                        c.and.updated <- paste0(c.collapse, " ")

                } else {

                        c.and.updated <- paste0(" ", c.and, " ")
                }

        } else {

                if(c.and == "") {

                        c.and.updated <- paste0(c.collapse, " ")

                } else {

                        c.and.updated <- paste0(c.collapse, " ", c.and, " ")
                }
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
