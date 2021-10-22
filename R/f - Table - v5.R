#################################################################################
# NAME:         FUNCTION - Calculate table
# AUTHOR:       Pieter Overdevest.
# DESCRIPTION:  Prepare table
#################################################################################

f_table <- function(df.input,
                    c.ver,
                    c.hor     = NULL,
                    c.useNA   = "ifany",  # options: "no", "ifany", "always"
                    c.type    = "abs",    # options: "abs", "rel"
                    b.warning = TRUE
                    ) {   # Warnings are shown (default).


        #########################################################################
        # Test Only!
        #########################################################################

        # Allways
        # c.hor    <- NULL
        # c.useNA  <- "ifany"
        # c.type   <- "abs"

        # Depending on case:
        # df.input <- df.temp
        # c.ver    <- "in.ipsm"
        # c.hor    <- "scenario"

        # df.input <- df.bord.fid
        # c.ver    <- "project.name"


        #########################################################################
        # Error checking
        #########################################################################

        if(!c.ver %in% names(df.input)) {

                stop("Note,'", c.ver, "' does not occur in df.input!")
        }


        if(!is.null(c.hor)) {

                if(!c.hor %in% names(df.input)) {

                        stop("Note,'", c.hor, "' does not occur in df.input!")
                }
        }


        #########################################################################
        # Initialization.
        #########################################################################

        # Indien het dataframe leeg is, voeg dan een lege rij toe, om error te voorkomen.
        # Return resultaat.
        if(nrow(df.input) == 0) {

                # Only when warning is true.
                if(b.warning) {

                        warning("Input data frame for f_table is empty; an empty table is returned.")
                        }

                df.input[nrow(df.input) + 1, ] <- NA

                return(df.input[c(c.hor, c.ver)])
        }


        #########################################################################
        # Processing
        #########################################################################

        if(is.null(c.hor)) {

                df.output <- count(df.input, get(c.ver)) %>%

                        arrange(desc(n)) %>%

                        mutate(`Total (%)` = round(n / sum(n, na.rm = TRUE) * 100, 1)) %>%

                        bind_rows(summarise(.,
                                            across(where(is.numeric), sum),
                                            across(where(is.character) | where(is.factor), ~"Total")))

                # Set sum of 100% to '100%'. Sometimes, the total is 99.9 because we sum rounded numbers.
                df.output$`Total (%)`[nrow(df.output)] <- 100

                # Update eerste header.
                v.name <- names(df.output)
                v.name[1] <- c.ver
                names(df.output) <- v.name


        } else {


                df.table.source <- table(df.input %>% mutate(Var1 = as.character(get(c.ver))) %>% pull(Var1),
                                         df.input %>% mutate(Var2 = as.character(get(c.hor))) %>% pull(Var2),
                                         useNA = c.useNA) %>%

                        as.data.frame()


                if(c.type == "abs") {

                        df.output <- df.table.source %>%

                                pivot_wider(id_cols = "Var1", names_from = "Var2", values_from = "Freq") %>%

                                mutate(Total       = rowSums(select_if(., is.numeric)),
                                       `Total (%)` = round(Total / sum(Total, na.rm = TRUE) * 100, 1)) %>%

                                bind_rows(summarise(.,
                                                    across(where(is.numeric), sum),
                                                    across(where(is.character) | where(is.factor), ~"Total")))

                        # Set sum of 100% to '100%'. Sometimes, the total is 99.9 because we sum rounded numbers.
                        df.output$`Total (%)`[nrow(df.output)] <- 100

                } else {

                        df.output <- df.table.source %>%

                                group_by(Var1) %>%

                                mutate(n.tot    = sum(Freq),
                                       perc.ver = round(Freq/n.tot*100, 0)) %>%

                                ungroup() %>%

                                pivot_wider(id_cols = "Var1", names_from = "Var2", values_from = "perc.ver")

                        }
                }


        # Hernoem eerste header
        v.names          <- names(df.output)
        v.names[1]       <- c.ver
        names(df.output) <- v.names


        #########################################################################
        # Return
        #########################################################################

        return(df.output)

        }

