#' @title Calculate standard error
#'
#' @description Calculates standard error.
#'
#' @author Pieter Overdevest
#'
#' @param df.input -----
#' @param c.group.by ----- (default: NULL).
#' @param c.model -----
#' @param c.data -----
#'
#' @returns Vector with standard errors.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' v.output <- f_calculate_std_error(
#'
#'      df.input   = df.sc,
#'      c.group.by = NULL,
#'      c.model    = "solids.median",
#'      c.data     = "solids"
#'
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_calculate_std_error <- function(

                df.input,
                c.group.by = NULL,
                c.model,
                c.data
        ) {

        ######################################################################################
        # TESTING
        ######################################################################################

        # df.input   = df.sc
        # c.group.by = "file.comp.stabilization"
        # c.model    = "solids.median"
        # c.data     = "solids"


        ######################################################################################
        # ERROR CHECK
        ######################################################################################

        ######################################################################################
        # INITIALIZE
        ######################################################################################

        ######################################################################################
        # MAIN
        ######################################################################################

        # When c.group.by is not specified than the std error is calculated acrosss the whole dataframe.
        if(
                !is.null(c.group.by)
        ) {

                df.input$group.by.dummy <- df.input[c.group.by]

        } else {

                df.input$group.by.dummy <- "X"
        }


        v.output <- df.input %>%

                mutate(
                        !!c.model := ifelse(is.na(get(c.model)) & !is.na(get(c.data)),  get(c.data),  get(c.model)),
                        !!c.data  := ifelse(is.na(get(c.data))  & !is.na(get(c.model)), get(c.model), get(c.data)),

                        !!c.model := ifelse(is.na(get(c.model)) & is.na(get(c.data)), 0, get(c.model)),
                        !!c.data  := ifelse(is.na(get(c.model)) & is.na(get(c.data)), 0, get(c.data))
                ) %>%

                mutate(
                        # Calculate square of difference between model and data.
                        diff.squared = (get(c.model) - get(c.data))^2
                ) %>%

                group_by(group.by.dummy) %>%

                        # Divide sum of squares by degrees of freedom (sum(!is.na(diff.squared)) - 1).
                        mutate(
                                n.dof     = (sum(!is.na(diff.squared)) - 1),
                                n.dof     = ifelse(n.dof == 0, 1, n.dof),
                                std.error = sqrt(sum(diff.squared, na.rm = TRUE) / n.dof)
                        ) %>%

                ungroup() %>%

                pull(std.error)


        ######################################################################################
        # ERROR CHECK
        ######################################################################################

        ######################################################################################
        # RETURN
        ######################################################################################

        return(v.output)

}


