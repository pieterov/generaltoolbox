#' @title Concatenate regex items in vector separated by "|"
#'
#' @description Concatenate regex items in vector separated by "|".
#'
#' @author Pieter Overdevest
#'
#' @param v.string Vector with items.
#' @param c.pre String to pre-pend (default: "^").
#' @param c.collapse String to collapse items (default: "|").
#' @param c.post String to append (default: "$").
#' @param b.sort Should items be sorted? (default: TRUE).
#'
#' @returns .
#'
#' @details -
#'
#' @export
#'
#' @examples
#' c.string <- f_paste_regex(
#'
#'        v.string     = c("pieter", "bart", "theo", "kees", "aad"),
#'        c.pre        = "^",
#'        c.collapse   = "|",
#'        c.post       = "$",
#'        b.sort       = TRUE
#' )


        #################################################################################
        # FUNCTION.
        #################################################################################

        f_paste_regex <- function(

                v.string,
                c.pre        = "^",
                c.collapse   = "|",
                c.post       = "$",
                b.sort       = TRUE
        ) {


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
        # Return.
        #########################################################################

        return(c.string)

}
