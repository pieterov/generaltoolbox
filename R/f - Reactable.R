##############################################################################################
# NAME:         FUNCTION - REACTABLE
# AUTHOR:       Pieter Overdevest
##############################################################################################

        f_reactable <- function(

                df.input,

                c.col.default.align     = "center",
                n.col.default.max.width = 120,

                v.col.text              = NULL,
                v.col.text.name         = NULL,
                v.col.text.align        = NULL,
                v.col.text.width        = NULL,

                v.col.digit             = NULL,
                v.col.digit.name        = NULL,
                v.col.digit.number      = NULL,
                v.col.digit.align       = NULL,
                v.col.digit.width       = NULL,

                v.col.euro              = NULL,
                v.col.euro.name         = NULL,
                v.col.euro.number       = NULL,
                v.col.euro.align        = NULL,
                v.col.euro.width        = NULL,

                n.defaultPageSize       = 10,
                b.showPageSizeOptions   = FALSE,
                v.pageSizeOptions       = c(10, 20, 30),

                b.filterable            = FALSE,
                b.searchable            = FALSE
        ) {


        ######################################################################################
        # TEST
        ######################################################################################

        # ALWAYS
        # c.col.default.align     = "center"
        # n.col.default.max.width = 100
        #
        # v.col.text              = NULL
        # v.col.text.name         = NULL
        # v.col.text.align        = NULL
        # v.col.text.width        = NULL
        #
        # v.col.digit             = NULL
        # v.col.digit.name        = NULL
        # v.col.digit.number      = NULL
        # v.col.digit.align       = NULL
        # v.col.digit.width       = NULL
        #
        # v.col.euro              = NULL
        # v.col.euro.name         = NULL
        # v.col.euro.number       = NULL
        # v.col.euro.align        = NULL
        # v.col.euro.width        = NULL
        #
        # n.defaultPageSize       = 10
        # b.showPageSizeOptions   = FALSE
        # v.pageSizeOptions       = c(10, 20, 30)
        #
        # b.filterable            = FALSE
        # b.searchable            = FALSE

        # # Set 1
        # df.input            = df.tg.target
        # c.col.default.align = "left"
        #
        # v.col.text          = "name"
        # v.col.text.width    = 150
        #
        # v.col.digit        = "target"
        # v.col.digit.name   = "target (%)"
        # v.col.digit.number = 2
        #
        # n.defaultPageSize  = 30

        ######################################################################################
        # ERROR CHECKS
        ######################################################################################

        ######################################################################################
        # INITIALIZATION
        ######################################################################################

        # Replace NA with "NA" in first column.
        df.input[is.na(df.input[[1]]), 1] <- "NA"


        ######################################################################################
        # Align text.
        ######################################################################################

        # Update v.col.text vectors.
        if(!is.null(v.col.text) & is.null(v.col.text.name))   v.col.text.name   <- v.col.text
        if(!is.null(v.col.text) & is.null(v.col.text.align))  v.col.text.align  <- rep("left", length(v.col.text))
        if(!is.null(v.col.text) & is.null(v.col.text.width))  v.col.text.width  <- rep(120,    length(v.col.text))


        l.colDef.text <- sapply(

                v.col.text,

                function(x) { # x = v.col.text[1]

                        colDef(
                                name  = v.col.text.name[x == v.col.text],
                                align = v.col.text.align[x == v.col.text],
                                width = v.col.text.width[x == v.col.text],

                                format = colFormat(

                                        currency   = "EUR",
                                        separators = TRUE,
                                        locales    = "nl-NL"
                                )
                        )
                },

                USE.NAMES = TRUE,
                simplify  = FALSE
        )


        ######################################################################################
        # Round and align regular numbers.
        ######################################################################################

        # Update v.col.digit vectors.
        if(!is.null(v.col.digit) & is.null(v.col.digit.name))   v.col.digit.name   <- v.col.digit
        if(!is.null(v.col.digit) & is.null(v.col.digit.number)) v.col.digit.number <- rep(2,       length(v.col.digit))
        if(!is.null(v.col.digit) & is.null(v.col.digit.align))  v.col.digit.align  <- rep("right", length(v.col.digit))
        if(!is.null(v.col.digit) & is.null(v.col.digit.width))  v.col.digit.width  <- rep(120,     length(v.col.digit))

        # Update formatting of variable column names. Using sapply allows keeping the
        # item names (not available in lapply). Using simplify is false prevents
        # the list from being collapsed.
        # https://github.com/glin/reactable/issues/138
        l.colDef.digit <- sapply(

                v.col.digit,

                function(x) { # x = v.col.digit[1]

                        colDef(
                                name  = v.col.digit.name[x == v.col.digit],
                                align = v.col.digit.align[x == v.col.digit],
                                width = v.col.digit.width[x == v.col.digit],

                                format = colFormat(

                                        digits = v.col.digit.number[x == v.col.digit]
                                )
                        )
                },

                USE.NAMES = TRUE,
                simplify  = FALSE
        )


        ######################################################################################
        # Round and align euro amounts.
        ######################################################################################

        # Update v.col.euro vectors.
        if(!is.null(v.col.euro) & is.null(v.col.euro.name))   v.col.euro.name   <- v.col.euro
        if(!is.null(v.col.euro) & is.null(v.col.euro.number)) v.col.euro.number <- rep(2,       length(v.col.euro))
        if(!is.null(v.col.euro) & is.null(v.col.euro.align))  v.col.euro.align  <- rep("right", length(v.col.euro))
        if(!is.null(v.col.euro) & is.null(v.col.euro.width))  v.col.euro.width  <- rep(120,     length(v.col.euro))


        l.colDef.euro <- sapply(

                v.col.euro,

                function(x) { # x = v.col.euro[1]

                        colDef(
                                name  = v.col.euro.name[x == v.col.euro],
                                align = v.col.euro.align[x == v.col.euro],
                                width = v.col.euro.width[x == v.col.euro],

                                format = colFormat(

                                        digits     = v.col.euro.number[x == v.col.euro],
                                        currency   = "EUR",
                                        separators = TRUE,
                                        locales    = "nl-NL"
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

                        #fontFamily = 'Arial',
                        fontSize   = '16px'
                ),

                defaultColDef = colDef(

                        align    = c.col.default.align,
                        maxWidth = n.col.default.max.width
                ),

                rowStyle = function(index) {

                        if (df.input[index, 1] == "Total") {

                                list(fontWeight = "bold")
                        }
                },

                columns = c(

                        l.colDef.text,

                        l.colDef.digit,

                        l.colDef.euro
                ),

                defaultPageSize      = n.defaultPageSize,
                showPageSizeOptions  = b.showPageSizeOptions,
                pageSizeOptions      = v.pageSizeOptions,

                filterable           = b.filterable,
                searchable           = b.searchable
        )


        ######################################################################################
        # ERROR CHECK
        ######################################################################################


        ######################################################################################
        # RETURN
        ######################################################################################


        }

