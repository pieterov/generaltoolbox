###############################################################################
# NAME:         FUNCTION - GET INDEX FROM LIST.
# AUTHOR:       Pieter Overdevest.
# DESCRIPTION:  Retrieve which element in list list of data frames contains
#               v.search in fields v.element.
###############################################################################

f_get_index_from_list <- function(

        l.input,
        v.element,
        v.search
) {


###############################################################################
# Test
###############################################################################

# l.input   <- df.bord.source
# v.element <- c("drager.id", "groep.id")
# v.search  <- c(9324728, 1)


###############################################################################
# Error check
###############################################################################

if(length(v.element) != length(v.search)) {

        stop("Let op, v.element moet even lang zijn als v.search!")
}


###############################################################################
# Main body
###############################################################################

v.result <- lapply(l.input, function(df.temp) { # df.temp <- l.input[[1]]

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


###############################################################################
# Return
###############################################################################

        return(which(v.result))

}
