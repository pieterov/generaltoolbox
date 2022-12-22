#' @title Convert string columns to integer columns.
#'
#' @description Converts data frame columns of type character to integers.
#'
#' @author Pieter Overdevest
#'
#' @param df.input Data frame with columns you want to convert from char to integer.
#' @param v.col Names of columns you want to convert from char to integer.
#' @param c.id Column name with unique identifier for each row. This is used to report on parsing errors.
#'
#' @returns Data frame with updated columns.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' df.output <- f_as_integer(
#'
#'            df.input  = tibble(id = c("A", "B"), id.product = c("1", "2"), id.variant = c("32", "45")),
#'            v.col     = c("id.product", "id.variant"),
#'            c.id      = "id"
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_as_integer <- function(

                df.input,
                v.col,
                c.id
        ) {


        ######################################################################################
        # TEST
        ######################################################################################

        # df.input <- df.google.merchant.export
        # v.col    <- c("id.product", "id.variant")
        # c.id     <- "id"

        # df.input <- tibble(id = c("x","y","z","zz"), id.product = c("45", "aa", "a", "67"), id.variant = c("q", "9", "10", NA), id.pieter = c("r", 1, 2, 3))
        # df.input <- tibble(id = c("x","y","z","zz"), id.product = c("45", "4", "4", "67"), id.variant = c("4", "9", "10", NA), id.pieter = c(NA, 1, 2, 3))
        # v.col    <- c("id.product", "id.variant", "id.pieter")
        # c.id     <- "id"


        ######################################################################################
        # ERROR CHECKS
        ######################################################################################

        # Check presence of columns in df.input.
        f_check_cols_present(df.input, c.id)
        f_check_cols_present(df.input, v.col)

        # Check that c.id does not contain NA and is unique
        f_check_col_not_empty(df.input, c.id)
        f_check_col_unique(df.input, c.id)

        ######################################################################################
        # INITIALIZATION
        ######################################################################################

        df.output <- df.input


        ######################################################################################
        # PROCESS
        ######################################################################################

        for(c.col in v.col) {

                # We do not want to get a warning message if data cannot be coerced into integer.
                # We deal with that below.
                suppressWarnings(

                        df.output <- df.output %>%

                                mutate(!!c.col := as.integer(get(c.col)))
                )

        }


        ######################################################################################
        # ERROR CHECK
        ######################################################################################

        # ID's that result in coercion error when concerned columns are set to integer.
        v.id.coercion.error <- lapply(

                v.col,

                function(c.col) {

                        df.input[[c.id]][

                                setdiff(
                                        which(is.na(df.output[c.col])),
                                        which(is.na(df.input[c.col]))
                                )
                        ] %>%

                        f_paste(b.quotation = TRUE)
                })


        # Prepare communication.
        v.temp <- lapply(

                seq_along(v.col),

                function(i) {

                        if(!is.null(v.id.coercion.error[[i]])) {

                                paste0(v.col[i], ": ", v.id.coercion.error[[i]], "\n")
                        }

                }) %>% unlist()


        # Error check!
        if(!is.null(v.temp)) {

                stop(paste0(

                        "Note, the following feature(s) result in a coercion error when converted to integer, see '", c.id, "'. ",
                        "This can also be explained by the size of the integer. When larger than ~2*10^9 the integer conversion ",
                        "will also lead to a coercion error.\n",
                        f_paste(v.temp, c.collapse = "", c.and = "")
                ))

        }


        ######################################################################################
        # RETURN
        ######################################################################################

        return(df.output)

        }

