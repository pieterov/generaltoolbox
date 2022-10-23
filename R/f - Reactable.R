##############################################################################################
# NAME:         FUNCTION - REACTABLE
# AUTHOR:       Pieter Overdevest
##############################################################################################

        f_reactable <- function(

                df.input,
                c.table.header        = NULL,
                c.table.footer        = NULL,
                n.table.number        = NULL,
                v.col.digit           = NULL,
                v.col.digit.name      = NULL,
                v.col.digit.number    = NULL,
                v.col.euro            = NULL,
                v.col.euro.name       = NULL,
                n.defaultPageSize     = 10,
                b.showPageSizeOptions = FALSE,
                v.pageSizeOptions     = c(10, 20, 30),
                b.filterable          = FALSE,
                b.searchable          = FALSE
        ) {


        ######################################################################################
        # TEST
        ######################################################################################

        ## ALWAYS
        # c.table.header        = NULL
        # c.table.footer        = NULL
        # n.table.number        = NULL
        # v.col.digit           = NULL
        # v.col.digit.name      = NULL
        # v.col.digit.number    = NULL
        # v.col.euro            = NULL
        # v.col.euro.name       = NULL
        # n.defaultPageSize     = 10
        # b.showPageSizeOptions = FALSE
        # v.pageSizeOptions     = c(10, 20, 30)
        # b.filterable          = FALSE
        # b.searchable          = FALSE
        # # Set 1
        # df.input           = df.tg.target
        # c.table.header      = "Dit is een TITEL"
        # c.table.footer     = "dit is een footer"
        # n.table.number     = 3
        # v.col.digit        = c("target")
        # v.col.digit.name   = c("target (%)")
        # v.col.digit.number = c(2)
        # n.defaultPageSize  = 30

        ######################################################################################
        # ERROR CHECKS
        ######################################################################################

        ######################################################################################
        # INITIALIZATION
        ######################################################################################

        # Update formatting of variable column names. Using sapply allows keeping the
        # item names (not available in lapply). Using simplify is false prevents
        # the list from being collapsed.
        # https://github.com/glin/reactable/issues/138
        l.colDef.digit <- sapply(

                v.col.digit,

                function(x) { # x = v.col.digit[1]

                        colDef(
                                name   = v.col.digit.name[x == v.col.digit],

                                format = colFormat(

                                        digits = v.col.digit.number[x == v.col.digit]
                                )
                        )
                },

                USE.NAMES = TRUE,
                simplify  = FALSE
        )


        l.colDef.euro <- sapply(

                v.col.euro,

                function(x) { # x = v.col.euro[1]

                        colDef(
                                name   = v.col.euro.name[x == v.col.euro],

                                format = colFormat(

                                        currency   = "EUR",
                                        separators = TRUE,
                                        locales    = "nl-NL",
                                        digits     = 2
                                )
                        )
                },

                USE.NAMES = TRUE,
                simplify  = FALSE
        )

        ######################################################################################
        # PROCESS
        ######################################################################################

        reactable(

                data = df.input,

                style = list(

                        fontFamily = 'Menlo',
                        fontSize   = '16px'
                ),

                defaultColDef = colDef(

                        align    = "center",
                        maxWidth = 120
                ),

                rowStyle = function(index) {

                        if (df.input[index, 1] == "Total") {

                                list(fontWeight = "bold")
                        }
                },

                columns = c(

                        l.colDef.digit,

                        l.colDef.euro
                ),

                defaultPageSize      = n.defaultPageSize,
                showPageSizeOptions  = b.showPageSizeOptions,
                pageSizeOptions      = v.pageSizeOptions,

                filterable           = b.filterable,
                searchable           = b.searchable
        ) %>%

        # Add title.
        # https://stackoverflow.com/questions/63503366/how-to-change-the-font-used-in-htmlwidgets-for-r
        purrr::when(

                !is.null(c.table.header) ~ htmlwidgets::prependContent(

                        ., htmltools::tags$strong(

                                paste0("Table ", n.table.number, " - ", c.table.header),

                                style = "font-family: Menlo; font-size: 14px"
                        )
                )
        ) %>%

        # Add footer.
        # Why using 'i' instead of 'p': https://shiny.rstudio.com/articles/tag-glossary.html
        purrr::when(

                !is.null(c.table.footer) ~ htmlwidgets::appendContent(

                        ., htmltools::tags$i(

                                paste0("Note - ", c.table.footer),

                                style = "font-family: Menlo; font-size: 12px"
                        )
                )
        )


        ######################################################################################
        # ERROR CHECK
        ######################################################################################


        ######################################################################################
        # RETURN
        ######################################################################################


        }

