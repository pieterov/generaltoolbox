#################################################################################
# NAME:         FUNCTION - Return sorted unique elements.
# AUTHOR:       Pieter Overdevest
# DESCRIPTION:  Return sorted unique elements.
#################################################################################

        f_unique <- function(

                v.vector,
                b.show.freq    = FALSE,
                b.sort.by.val  = TRUE,
                b.sort.by.freq = FALSE,
                n.char         = "all") {


        #########################################################################
        # Testing
        #########################################################################

        # ALWAYS
        # b.show.freq    = FALSE
        # b.sort.by.val  = TRUE
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
                        x = ifelse(is.na(x), "NA", x),

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
        v.result <- df.result %>%

                purrr::when(

                         b.sort.by.freq ~ arrange(., desc(n)),
                         b.sort.by.val  ~ arrange(., x),
                         TRUE           ~ .
                ) %>%

                purrr::when(

                         b.show.freq ~ pull(., y),
                         TRUE        ~ pull(., x)
                )



        #########################################################################
        # Return
        #########################################################################

        return(v.result)

        }
