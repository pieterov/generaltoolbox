#' @title Prepare 2D frequency table
#'
#' @description Prepares 2D frequency table.
#'
#' @author Pieter Overdevest
#'
#' @param df.input Data frame containing features c.ver (and c.hor)
#' @param c.ver Feature name to pivot vertically.
#' @param c.hor Feature name to pivot horizontally. If provided this will result in a 2D frequency table, and when not
#' provided the result will be a 1D frequency table (default: NULL).
#' @param c.useNA How to deal with NAs. Options are: "ifany", "no" "always" (default: "ifany").
#' @param c.type Type result. Options are: "abs", "rel" (default: "abs").
#' @param n.round Rounding the numbers (default: 1).
#' @param b.warning Should warnings be shown? (default: TRUE).
#'
#' @returns A 2D (or 1D) frequency table.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_table(
#'
#'       df.input  = mtcars,
#'       c.ver     = "gear",
#'       c.hor     = "am",
#'       c.useNA   = "ifany",
#'       c.type    = "abs",
#'       n.round   = 1,
#'       b.warning = TRUE
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_table <- function(

                df.input,
                c.ver,
                c.hor     = NULL,     # option to have 2D table.
                c.useNA   = "ifany",  # options: "no", "ifany", "always"
                c.type    = "abs",    # options: "abs", "rel"
                n.round   = 1,        # 1 digit roundinbg
                b.warning = TRUE      # Warnings are shown (default).
        ) {


        #########################################################################
        # Test Only!
        #########################################################################

        # Allways
        # c.hor     = NULL
        # c.useNA   = "ifany"
        # c.type    = "abs"
        # n.round   = 1
        # b.warning = TRUE

        # Depending on case:
        # df.input = df.temp
        # c.ver    = "jaar.eerste.vastlegging"
        # c.ver    = "in.ipsm"
        # c.hor    = "scenario"

        # df.input = df.bord.fid
        # c.ver    = "project.name"

        # df.input  = df.bord.drager.sqlite
        # c.ver     = "bord.historie"
        # c.ver     = "datum.eerste.vastlegging"


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


        # Maak van logical attributes character attributes, zodat het bij 'sum' niet tot verkeerde tellingen
        # leidt, aangezien booleans ook opgeteld kunnen worden (= is.numeric).
        #df.input <- df.input %>% mutate(across(where(is.logical) | where(is.Date), as.character))
        df.input <- df.input %>% mutate_all(as.character)

        #########################################################################
        # Processing
        #########################################################################

        if(is.null(c.hor)) {

                df.output <- df.input %>%

                        count(get(c.ver)) %>%

                        arrange(desc(n)) %>%

                        mutate(`Total (%)` = round(n / sum(n, na.rm = TRUE) * 100, n.round)) %>%

                        bind_rows(summarise(., across(where(is.numeric), sum)))

                # Set sum of 100% to '100%'. Sometimes, the total is 99.9 because we sum rounded numbers.
                df.output$`Total (%)`[nrow(df.output)] <- 100
                df.output[nrow(df.output),1]           <- "Total"

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
                                       `Total (%)` = round(Total / sum(Total, na.rm = TRUE) * 100, n.round)) %>%

                                bind_rows(summarise(.,
                                                    across(where(is.numeric), sum),
                                                    across(where(is.character) | where(is.factor), ~"Total")))

                        # Set sum of 100% to '100%'. Sometimes, the total is 99.9 because we sum rounded numbers.
                        df.output$`Total (%)`[nrow(df.output)] <- 100

                } else {

                        df.output <- df.table.source %>%

                                group_by(Var1) %>%

                                mutate(n.tot    = sum(Freq),
                                       perc.ver = round(Freq/n.tot*100, n.round)) %>%

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

