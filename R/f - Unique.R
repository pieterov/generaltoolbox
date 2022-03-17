#################################################################################
# NAME:         FUNCTION - Return sorted unique elements.
# AUTHOR:       Pieter Overdevest
# DESCRIPTION:  Return sorted unique elements.
#################################################################################


        f_unique <- function(

                v.vector,
                b.freq = FALSE,
                c.sort = "alphabetical",  # Alternative 'freq'
                n.char = "all") {

        #########################################################################
        # Testing
        #########################################################################

        # v.vector <- df.bord.hl.concept.hl.final %>% filter(project.name.year %in% v.sqlite.name.in.hl.concept.in.hl.final, in.hl.concept.not.in.hl.final) %>% pull(project.name.year)
        # v.vector <- v.temp; n.char = 5

        #########################################################################
        # Error check
        #########################################################################

        if(!(is.numeric(n.char) | n.char == "all")) {

                stop(paste0(

                        "Note, input variable 'n.char' must be 'all' (default) or a whole number, not '",
                        n.char, "' what you provided!"
                ))
        }


        #########################################################################
        # Process
        #########################################################################

        df.result <- tibble(x = v.vector) %>%

                count(x) %>%

                mutate(
                        x = ifelse(is.na(x), "NA", x),

                        x = if(n.char != "all") {

                                ifelse(
                                        nchar(x) > n.char,

                                        paste0(substr(x, 1, n.char), ".."),

                                        x
                                )

                                } else {x},

                        y = paste0(x, " (", n, ")")

                )


        # Sort.
        if(b.freq | c.sort != "alphabetical") {

                df.result <- df.result %>% arrange(desc(n))

        } else {

                df.result <- df.result %>% arrange(x)
        }


        # Add frequency?
        if(b.freq) {

                v.result <- df.result %>% pull(y)

        } else {

                v.result <- df.result %>% pull(x)
        }


        #########################################################################
        # Return
        #########################################################################

        return(v.result)

        }
