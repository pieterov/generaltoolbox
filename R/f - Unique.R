#' @title  Return sorted unique elements
#'
#' @description  Returns sorted unique elements.
#'
#' @author Pieter Overdevest
#'
#' @param v.vector Vector with elements to get unique values of.
#' @param b.show.freq Should we add the frequency of values occuring? (default: FALSE).
#' @param b.sort.by.val Should results be sorted by values? (default: FALSE).
#' @param b.sort.by.freq Should results be sorted by frequency? (default: FALSE).
#' @param n.char The number of characters to use of each value (default: "all").
#'
#' @returns Concatenated string of items.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' v.result <- f_unique(
#'
#'      v.vector       = c(50, 30, 100, 100, 100, 30),
#'      b.show.freq    = FALSE,
#'      b.sort.by.val  = FALSE,
#'      b.sort.by.freq = FALSE,
#'      n.char         = "all"
#' )


        #################################################################################
        # FUNCTION.
        #################################################################################

        f_unique <- function(

                v.vector,
                b.show.freq    = FALSE,
                b.sort.by.val  = FALSE,
                b.sort.by.freq = FALSE,
                n.char         = "all"
        ) {


        #########################################################################
        # Testing
        #########################################################################

        # ALWAYS
        # b.show.freq    = FALSE
        # b.sort.by.val  = FALSE
        # b.sort.by.freq = FALSE
        # n.char         = "all"


        # Scenario 1
        # v.vector       = c(50, 30, 100, 100, 100, 30)
        #
        # b.show.freq    = FALSE
        # b.sort.by.val  = TRUE
        # b.sort.by.freq = FALSE
        #
        # b.show.freq    = TRUE
        # b.sort.by.val  = TRUE
        # b.sort.by.freq = FALSE
        #
        # b.show.freq    = TRUE
        # b.sort.by.val  = FALSE
        # b.sort.by.freq = TRUE
        #
        # b.show.freq    = FALSE
        # b.sort.by.val  = FALSE
        # b.sort.by.freq = TRUE




        #########################################################################
        # Error check
        #########################################################################

        # n.char must have correct value.
        if(!(is.numeric(n.char) | n.char == "all")) {

                stop(paste0(

                        "Note, input variable 'n.char' must be 'all' (default) or a whole number, not '",
                        n.char, "' what you provided!"
                ))
        }


        # b.sort.by.val and b.sort.by.freq cannot both be TRUE
        if(b.sort.by.val & b.sort.by.freq) {

                stop("Note, b.sort.by.val and b.sort.by.freq cannot both be TRUE!")
        }


        #########################################################################
        # Process
        #########################################################################

        df.result <- tibble(x = unique(v.vector)) %>%

                left_join(

                        y  = tibble(x = v.vector) %>% dplyr::count(x),
                        by = "x"
                ) %>%

                mutate(
                        x = ifelse(is.na(x), "NA", as.character(x)),

                        x = if(n.char != "all") {

                                ifelse(
                                        nchar(x) > n.char,

                                        paste0(substr(x, 1, n.char), ".."),

                                        x
                                )

                                } else {x},

                        #x = as.character(x),

                        y = paste0(x, " (", n, ")"),

                        y = as.character(y)
                )


        # Whether frequency is added determines by what feature is sorted.

        # 1. Handle sorting
        v.result <- df.result

        if (b.sort.by.freq) {
                v.result <- v.result %>% arrange(desc(n))
        } else if (b.sort.by.val) {
                v.result <- v.result %>% arrange(x)
        }

        # 2. Handle pulling the result
        if (b.show.freq) {
                v.result <- v.result %>% pull(y)
        } else {
                v.result <- v.result %>% pull(x)
        }

                
        #########################################################################
        # Return
        #########################################################################

        return(v.result)

        }
