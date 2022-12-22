#' @title Define column types
#'
#' @description Defines column type.
#'
#' @author Pieter Overdevest
#'
#' @param v.names -----
#' @param v.double -----
#' @param v.integer -----
#' @param v.date.time -----
#'
#' @returns List of column types.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' l.col.type <- f_define_col_type(
#'
#'     df.input,
#'     v.col.include = NULL,
#'     v.col.exclude = NULL,
#'     c.id
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_define_col_type <- function(

                v.names,
                v.double,
                v.integer,
                v.date.time
        ) {


        ######################################################################################
        # TEST
        ######################################################################################

        # v.names <- v.names.old
        #
        # v.double <- c(
        #
        #         "line.items.original.unit.price", "line.items.unit.cost", "total.discounts",
        #         "refunds.refund.line.items.price", "refunds.total.refund", "net.payment"
        # )
        #
        # v.integer <- c(
        #
        #         "line.items.quantity", "refunds.refund.line.items.quantity", "line.items.current.quantity"
        # )
        #
        # v.date.time <- c(
        #
        #         "created.at", "refunds.created.at"
        # )


        ######################################################################################
        # ERROR CHECKS
        ######################################################################################

        # Error check
        if(!all(c(v.double, v.integer, v.date.time) %in% v.names)) {

                f_stop(
                        "The following column names do not occur in v.names:\n",
                        f_paste(setdiff(c(v.double, v.integer, v.date.time), v.names), b.quotation = TRUE)
                )
        }


        ######################################################################################
        # INITIALIZATION
        ######################################################################################

        # Define column types.
        l.col.type = list()


        ######################################################################################
        # PROCESS
        ######################################################################################

        # Comms to the user.
        cat(
                "The following columns will be read as character:\n",
                f_paste(setdiff(v.names, c(v.double, v.integer, v.date.time)), b.quotation = TRUE)
        )


        # Append col type per col name.
        for (i in seq_along(v.names)) {

                l.col.type <- case_when(

                        v.names[i] %in% v.double    ~ list.append(l.col.type, col_double()),
                        v.names[i] %in% v.integer   ~ list.append(l.col.type, col_integer()),
                        v.names[i] %in% v.date.time ~ list.append(l.col.type, col_datetime()),
                        TRUE                        ~ list.append(l.col.type, col_character())
                )
        }


        ######################################################################################
        # ERROR CHECK
        ######################################################################################

        ######################################################################################
        # RETURN
        ######################################################################################

        return(l.col.type)

        }

