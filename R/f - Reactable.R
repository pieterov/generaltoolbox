##############################################################################################
# NAME:         FUNCTION - REACTABLE
# AUTHOR:       Pieter Overdevest
##############################################################################################

        f_reactable <- function(

                df.input,
                c.table.title         = "",
                v.col.digit,
                v.col.digit.name,
                v.col.digit.number,
                n.defaultPageSize     = 10,
                b.showPageSizeOptions = FALSE,
                v.pageSizeOptions     = c(10, 20, 30),
                b.filterable          = FALSE,
                b.searchable          = FALSE
        ) {


        ######################################################################################
        # TEST
        ######################################################################################

        # ALWAYS
        # c.table.title         = ""
        # n.defaultPageSize     = 10
        # b.showPageSizeOptions = FALSE
        # v.pageSizeOptions     = c(10, 20, 30)
        # b.filterable          = FALSE
        # b.searchable          = FALSE

        # Set 1
        # df.input           = df.tg.target
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


        ######################################################################################
        # PROCESS
        ######################################################################################

        reactable(

                data = df.input,

                style = list(

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

                        l.colDef.digit
                ),

                defaultPageSize      = n.defaultPageSize,
                showPageSizeOptions  = b.showPageSizeOptions,
                pageSizeOptions      = v.pageSizeOptions,

                filterable           = b.filterable,
                searchable           = b.searchable
        ) %>%

        htmlwidgets::prependContent(

                h2(
                        class = "title",
                        paste("Table - ", c.table.title)
                )
        )


        ######################################################################################
        # ERROR CHECK
        ######################################################################################


        ######################################################################################
        # RETURN
        ######################################################################################


        }

