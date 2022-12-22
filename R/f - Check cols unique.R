#' @title Check whether columns are unique
#'
#' @description Checks whether columns are unique.
#'
#' @author Pieter Overdevest
#'
#' @param df.input -----
#' @param v.col.include ----- (default: NULL)
#' @param v.col.exclude ----- (default: NULL)
#' @param c.id -----
#'
#' @returns Nothing.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_check_cols_unique(
#'
#'     df.input,
#'     v.col.include = NULL,
#'     v.col.exclude = NULL,
#'     c.id
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_check_cols_unique <- function(

                df.input,
                v.col.include = NULL,
                v.col.exclude = NULL,
                c.id
        ) {


        ######################################################################################
        # TEST
        ######################################################################################

        # Test!
        # df.input = df.datachamp.baseline.source
        # v.col.include    = v.col.include.must.be.unique
        # c.id     = "ID"

        # df.input <- tibble(ID = letters[1:6], `Variants: SKU` = c(seq(5), 5), dummy1 = c(seq(4), 4, 4), dummy2 = seq(6))
        # v.col.include    <- c("Variants: SKU", "dummy1", "dummy2")
        # c.id     <- "ID"
        # f_check_cols_unique(df.input, v.col.include, c.id)


        ######################################################################################
        # ERROR CHECKS
        ######################################################################################

        # Check presence of columns in df.input.
        f_check_cols_present(df.input, c.id)

        if(!is.null(v.col.include)) f_check_cols_present(df.input, v.col.include)
        if(!is.null(v.col.exclude)) f_check_cols_present(df.input, v.col.exclude)

        # Check that c.id does not contain NA and is unique.
        f_check_col_not_empty(df.input, c.id)
        f_check_col_unique(df.input, c.id)


        ######################################################################################
        # INITIALIZE
        ######################################################################################

        if(is.null(v.col.include))  v.col.include <- names(df.input)
        if(!is.null(v.col.exclude)) v.col.include <- setdiff(v.col.include, v.col.exclude)

        # Remove c.id from v.col.include, if present.
        v.col.include <- v.col.include[v.col.include != c.id]


        ######################################################################################
        # PROCESS
        ######################################################################################

        # Determine features with NA.
        df.temp <- df.input %>%

                # Select concerned columns
                select(all_of(v.col.include)) %>%

                # Determine number of NA per column
                f_summarize(b.view = FALSE, b.return = TRUE) %>%

                # Select columns that are not unique
                mutate(n.not.unique = n.tot - n.unique) %>%

                # Select columns that are not unique.
                filter(n.not.unique > 0) %>%

                # Sort by non-uniqueness.
                arrange(n.not.unique) %>%

                # Create label.
                mutate(n.label = paste0("'", feature, "' (", n.not.unique, ")"))


        ######################################################################################
        # ERROR CHECK
        ######################################################################################

        if(nrow(df.temp) > 0) {

                # Comms.
                cat(paste0(
                        "We observe features that do not contain unique values (out of ",
                        nrow(df.input), "): ",
                        f_paste(df.temp$n.label), ".\n\n"
                ))


                # Create label.
                v.temp <- lapply(df.temp$feature, function(c.unique) { # c.unique <- df.temp$feature[1]

                        v.temp <- df.input %>%

                                # Select concerned columns
                                select(all_of(c(c.id, c.unique))) %>%

                                add_count(get(c.unique)) %>%

                                filter(n > 1) %>%

                                select(all_of(c.unique), n) %>%

                                distinct() %>%

                                mutate(n.label = paste0("'", get(c.unique), "' (", n, ")")) %>%

                                pull(n.label)

                        return(
                                paste0("'", c.unique, "': ", f_paste(v.temp))
                        )

                }) %>% unlist()


                # Comms.
                cat(paste0(

                        "The following features do not contain unique values (", c.id, "):\n",
                        f_paste(v.temp, b.sort = FALSE), ".\n\n"
                ))


                # Create label.
                v.temp <- lapply(df.temp$feature, function(c.unique) { # c.unique <- df.temp$feature[1]

                        df.input %>%

                            # Select concerned columns
                            select(all_of(c(c.id, c.unique))) %>%

                            add_count(get(c.unique)) %>%

                            filter(n > 1) %>%

                            pull(c.id) %>%

                            f_paste(., b.quotation = TRUE) %>%

                            paste0("'", c.unique, "' (", ., ")")

                }) %>% unlist()


                stop(paste0(

                          "Note, the following features do not contain unique values (", c.id, "):\n",
                          f_paste(v.temp, b.sort = FALSE), ".\n\n"
                ))
        }

        }

