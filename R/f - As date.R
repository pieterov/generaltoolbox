#' @title Convert string to date.
#'
#' @description Converts a string to a date.
#'
#' @author Pieter Overdevest
#'
#' @param v.input Vector with strings to be converted.
#' @param c.format Format of date in the strings (default: "\%d/\%m/\%Y").
#' @param c.origin Starting date (default: "1899-12-30")
#'
#' @returns Vector with dates.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' v.output <- f_as_date(
#'            v.input  = c("16/03/2022", "16/03/2022", "16/03/2022"),
#'            c.format = "%d/%m/%Y",
#'            c.origin = "1899-12-30"
#' )

        #######################################################################
        # FUNCTION.
        #######################################################################

        f_as_date <- function(

                v.input,

                c.format = "%d/%m/%Y",

                c.origin = "1899-12-30"
        ) {

                
        #######################################################################
        # PROCESS
        #######################################################################
        
        # 1. Try converting using the format string (Strategy A)
        # We suppress warnings here because non-matching formats will naturally warn
        v.date.fmt <- suppressWarnings(as.Date(v.input, format = c.format))
        
        # 2. Try converting using the numeric origin (Strategy B)
        # Only necessary if Strategy A produced NAs, but fast to compute for all
        v.date.num <- suppressWarnings(as.Date(as.numeric(v.input), origin = c.origin))
        
        # 3. Coalesce: Take result from A; if NA, take result from B
        v.output <- dplyr::coalesce(v.date.fmt, v.date.num)
        
                
        #######################################################################
        # ERROR CHECK
        #######################################################################
        
        # Identify values that were not NA originally but became NA (conversion failed)
        v.temp <- v.input[!is.na(v.input) & is.na(v.output)]
        
        if(length(v.temp) > 0) {

                warning(paste0(
                        "Note, the following 'dates' could not be converted to a ",
                        "valid date format: ",
                        # Assuming f_paste is another function in your package:
                        f_paste(v.temp, b.quotation = TRUE) 
                ))
        }
                

        #######################################################################
        # RETURN
        #######################################################################

        return(v.output)

        }

