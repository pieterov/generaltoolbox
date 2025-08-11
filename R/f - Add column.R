#' @title Add missing columns to a data frame.
#'
#' @description Adds one or more columns to a data frame if they are not already
#' present, filling them with a specified default value.
#'
#' @author Pieter Overdevest
#'
#' @param df.input A data frame to which columns may be added.
#' @param v.col Character vector of column names to ensure in \code{df.input}.
#' @param c.fill.value Value used to fill new columns (default is \code{NA_character_}).
#'
#' @return A data frame with the specified columns present; newly added columns
#' are filled with \code{c.fill.value}.
#'
#' @details Existing columns in \code{df.input} are left unchanged.
#' Only columns from \code{v.col} that are not already present are added.
#'
#' @examples
#' df <- data.frame(a = 1:3)
#' f_add_column(df, c("b", "c"), 0)
#' # Returns data frame with columns a, b, c; b and c are filled with 0.
#'
#' @export
#'
#################################################################################
# FUNCTION.
#################################################################################

f_add_column <- function(

        df.input,
        v.col,
        c.fill.value = NA_character_
) {


##############################################################################
# TEST ONLY!!
##############################################################################

##############################################################################
# Initialize // Error checking
##############################################################################

##############################################################################
# Main
##############################################################################

        # Which columns are missing?
        v.col.missing <- setdiff(v.col, names(df.input))

        # Nothing to add → return as-is
        if (!length(v.col.missing)) return(df.input)

        # Add all missing columns, filled with c.fill.value
        df.input %>%
                mutate(
                        !!!set_names(
                                rep(list(c.fill.value), length(v.col.missing)),
                                v.col.missing
                        )
                )


##############################################################################
# Return
##############################################################################

}
