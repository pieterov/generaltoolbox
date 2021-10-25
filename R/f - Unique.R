#################################################################################
# NAME:         FUNCTION - Return sorted unique elements.
# AUTHOR:       Pieter Overdevest
# DESCRIPTION:  Return sorted unique elements.
#################################################################################


        f_unique <- function(

                v.vector,
                b.freq = FALSE) {

        #########################################################################
        # Testing
        #########################################################################

        # v.vector <- df.bord.hl.concept.hl.final %>% filter(project.name.year %in% v.sqlite.name.in.hl.concept.in.hl.final, in.hl.concept.not.in.hl.final) %>% pull(project.name.year)


        #########################################################################
        # Testing
        #########################################################################

        if(b.freq) {

                v.result <- tibble(x = v.vector) %>%

                        count(x) %>%

                        mutate(y = paste0(x, " (", n, ")")) %>%

                        arrange(desc(n)) %>%

                        pull(y)

        } else {

                v.result <- sort(unique(v.vector))

                }


        if(length(v.result) > 10000)
                stop("Meer dan 10,000 items, bedoelde je 'f_is_unique'?")


        #########################################################################
        # Return
        #########################################################################

        return(v.result)

        }
