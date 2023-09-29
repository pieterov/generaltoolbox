#' @title Give basic info on columns in data frame
#'
#' @description Gives basic info on columns in data frame.
#'
#' @author Pieter Overdevest
#'
#' @param df.input Data frame to summarize.
#' @param b.sort Should items be sorted? (default: TRUE).
#' @param b.view Should we display the result? (default: TRUE).
#' @param b.write Should we write the summary table to XLS? (default: FALSE).
#' @param b.return Should we return the resulting data frame (default: FALSE).
#' @param n.char Number of characters to show of each item (default: "all").
#' @param n.freq Number of items to show (default: 3).
#'
#' @returns Data frame summarizing the data in df.input, in case b.return is TRUE.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_summarize(
#'
#'      df.input = mtcars,
#'      b.sort   = TRUE,
#'      b.view   = TRUE,
#'      b.write  = FALSE,
#'      b.return = FALSE,
#'      n.char   = "all",
#'      n.freq   = 3
#' )


        #################################################################################
        # FUNCTION.
        #################################################################################

        f_summarize <- function(

                df.input,
                b.sort   = TRUE,
                b.view   = TRUE,
                b.write  = FALSE,
                b.return = FALSE,
                n.char   = "all",
                n.freq   = 3

        ) {


        ##############################################################################################
        # Testing.
        ##############################################################################################

        # ALWAYS
        # b.sort   = TRUE
        # b.view   = TRUE
        # b.return = FALSE
        # n.char   = "all"
        # n.freq   = 3

        # OPTIES
        # df.input = df.source %>% select(23) %>% head(10)
        # b.sort   = FALSE
        # b.view   = TRUE
        # b.return = FALSE
        # n.char   = 20
        # n.freq   = 3

        # df.input = l.result1[[1]][[1]]
        # b.view   = FALSE
        # b.return = TRUE

        # df.input <- tibble(a=sample(LETTERS[1:3],20, replace = TRUE), b=sample(c(now(), today()+100),20, replace = TRUE))

        ##############################################################################################
        # Error check.
        ##############################################################################################

        # Check that input is a data frame.
        if(!"data.frame" %in% class(df.input)) {

                stop("Note, the input must be a data frame!")
        }


        # Check whether one or more columns contain listed data.
        v.class         <- sapply(df.input, class)
        v.names.as.list <- names(v.class[v.class %in% c("list")])

        if(length(v.names.as.list) > 0) {

                for (c.temp in v.names.as.list) { # c.temp <- v.names.as.list[1]

                        df.input <- df.input %>%

                                mutate(
                                        !!c.temp := c.temp %>% get() %>% unlist() %>% paste(collapse = "|")
                                )
                }


                warning(glue(

                        "De volgende {length(v.names.as.list)} kolommen zijn van het type 'list'. De waarden ",
                        "in deze kolommen zijn aan elkaar geplakt om f_summary te voltooien:\n",
                        "{f_paste(v.names.as.list)}"
                ))
        }


        ##############################################################################################
        # Main.
        ##############################################################################################

        # Get info per column.
        df.info.per.column <-

                tibble(
                        feature = names(df.input)
                ) %>%

                mutate(
                        class = sapply(

                                v.class, function(c.temp) {

                                        paste(c.temp, collapse = "|")
                                }),

                        n.na = sapply(

                                df.input, function(v.temp) {

                                        sum(is.na(v.temp), na.rm = TRUE)

                                }),

                        n.zero = sapply(

                                df.input, function(v.temp) { # v.temp <- df.input[1]

                                        sum(v.temp == 0, na.rm = TRUE)

                                }),

                        n.unique = sapply(

                                df.input, function(v.temp) {

                                        length(unique(v.temp))

                                }),

                        n.tot = nrow(df.input),

                        min = sapply(

                                df.input, function(v.temp) { # v.temp <- df.input$b

                                        ifelse(
                                                any(c("numeric", "integer", "Date", "POSIXct", "POSIXt") %in% class(v.temp)),
                                                as.character(min(v.temp, na.rm = TRUE)),
                                                NA
                                        )
                                }),

                        max = sapply(

                                df.input, function(v.temp) {

                                        ifelse(
                                                any(c("numeric", "integer", "Date", "POSIXct", "POSIXt") %in% class(v.temp)),
                                                as.character(max(v.temp, na.rm = TRUE)),
                                                NA
                                        )
                                }),

                        example = sapply(

                                df.input,

                                function(v.temp) { # v.temp = df.input[["b"]]

                                        n.freq.present <- v.temp %>% unique() %>% length()

                                        n.freq.used    <- min(n.freq, n.freq.present)

                                        v.temp %>%

                                                f_unique(
                                                        b.show.freq    = TRUE,
                                                        b.sort.by.freq = TRUE,
                                                        n.char         = n.char
                                                ) %>%

                                                .[1:n.freq.used] %>%

                                                {
                                                        if(n.freq.used == n.freq.present) {

                                                                f_paste(v.string = ., b.sort = FALSE)

                                                        } else {

                                                                paste0(
                                                                        f_paste(v.string = ., c.and = "", b.sort = FALSE),
                                                                        ", ..."
                                                                )
                                                        }
                                                }
                                })
                )


        # Sort column names?
        if(b.sort) {

                df.info.per.column <- df.info.per.column %>%

                        arrange(class, desc(n.unique))
        }


        ##############################################################################################
        # Return.
        ##############################################################################################

        if(b.write)  {

                # Write data to XLS.
                f_write_data_to_file(

                        x             = df.info.per.column,
                        c.file.string = "df.info.per.column",
                        v.sheet.name  = "Sheet1",
                        v.path        = path.data,
                        v.xls         = TRUE,
                        v.add.time    = TRUE
                )
        }

        if(b.view)   {View(df.info.per.column)}
        if(b.return) {return(df.info.per.column)}
}

