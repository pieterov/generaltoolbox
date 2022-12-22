#' @title Clean up header names
#'
#' @description Clean up header names.
#'
#' @author Pieter Overdevest
#'
#' @param df.data Data frame whose names need to be cleaned.
#'
#' @returns Data frame with cleaned up names.
#'
#' @details Special characters are replaced by period.
#'
#' @export
#'
#' @examples
#' df.data <- f_clean_up_header_names(
#'
#'     df.data
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_clean_up_header_names <- function(

                df.data
        ) {


        #########################################################################
        # TEST ONLY
        #########################################################################


        #########################################################################
        # INITIALIZATION
        #########################################################################

        # Get and clean up header row.
        names.df.data <- names(df.data) %>%

                gsub("\\(|\\)|\\ |/|_|-|–|&|#|\\?|%|\\$|<|>|\u20AC|\\+|'|\\n|\\r|:|,",".", .) %>%
                gsub("\\.+", ".", .) %>%
                gsub("^\\.|\\.$", "", .) %>%
                tolower(.) %>%
                stringi::stri_trans_general('latin-ascii')

        # Clean up column names. \u20AC stands for euro sign.
        names(df.data) <- names.df.data


        #########################################################################
        # RETURN
        #########################################################################

        return(df.data)

        }
