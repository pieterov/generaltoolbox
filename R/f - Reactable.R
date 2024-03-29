#' @title Create reactable table in html document
#'
#' @description Creates reactable table in html document.
#'
#' @author Pieter Overdevest
#'
#' @param df.input Data frame to print as rectable table in html document.
#' @param c.col.default.align What is default alignment. Options are: "left", "center", and "right" (default: "center").
#' @param n.col.default.max.width What is the max width of a column (default: 500).
#' @param v.col.text Vector with feature names that should be formated as text (default: NULL).
#' @param v.col.text.name Vector with header names that should be used in the table instead (default: NULL).
#' @param v.col.text.align Vector with alignment of the text. Options are: "left", "center", and "right" (default: NULL).
#' @param v.col.text.width Vector with widths of the concerned columns (default: NULL).
#' @param v.col.digit Vector with feature names that should be formated as number (default: NULL).
#' @param v.col.digit.name Vector with header names that should be used in the table instead (default: NULL).
#' @param v.col.digit.number Vector with number of digits to use (default: NULL).
#' @param v.col.digit.align Vector with alignment of the numbers. Options are: "left", "center", and "right" (default: NULL).
#' @param v.col.digit.width Vector with widths of the concerned columns (default: NULL).
#' @param v.col.euro Vector with feature names that should be formated as euro (default: NULL).
#' @param v.col.euro.name Vector with header names that should be used in the table instead (default: NULL).
#' @param v.col.euro.number Vector with number of digits to use (default: NULL).
#' @param v.col.euro.align Vector with alignment of the euro's. Options are: "left", "center", and "right" (default: NULL).
#' @param v.col.euro.width Vector with widths of the concerned columns (default: NULL).
#' @param v.col.link.label Vector with feature names that should be used as link label (default: NULL).
#' @param v.col.link.url Vector with feature names that should be used as link url (default: NULL).
#' @param v.col.link.name Vector with header names that should be used in the table instead (default: NULL).
#' @param v.col.link.align Vector with alignment of the links. Options are: "left", "center", and "right" (default: NULL).
#' @param v.col.link.width Vector with widths of the concerned columns (default: NULL).
#' @param v.row.number.color Row numbers that need to be colored with c.row.number.color (default: NULL).
#' @param c.row.number.color Color to give to the concerned rows (default: NULL).
#' @param n.defaultPageSize What is the default page size? (default: 10).
#' @param b.showPageSizeOptions Should we show page size options? (default: TRUE).
#' @param v.pageSizeOptions What page size options to provide (default: c(10, 20, 30)).
#' @param b.filterable Should table be filterable? (default: TRUE).
#' @param b.searchable Should table be searchable? (default: TRUE).
#'
#' @returns Reactable table.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_reactable(
#'
#'      df.input                = mtcars,
#'      c.col.default.align     = "center",
#'      n.col.default.max.width = 500,
#'      v.col.text              = NULL,
#'      v.col.text.name         = NULL,
#'      v.col.text.align        = NULL,
#'      v.col.text.width        = NULL,
#'      v.col.digit             = NULL,
#'      v.col.digit.name        = NULL,
#'      v.col.digit.number      = NULL,
#'      v.col.digit.align       = NULL,
#'      v.col.digit.width       = NULL,
#'      v.col.euro              = NULL,
#'      v.col.euro.name         = NULL,
#'      v.col.euro.number       = NULL,
#'      v.col.euro.align        = NULL,
#'      v.col.euro.width        = NULL,
#'      v.col.link.label        = NULL,
#'      v.col.link.url          = NULL,
#'      v.col.link.name         = NULL,
#'      v.col.link.align        = NULL,
#'      v.col.link.width        = NULL,
#'      v.row.number.color      = NULL,
#'      c.row.number.color      = NULL,
#'      n.defaultPageSize       = 10,
#'      b.showPageSizeOptions   = TRUE,
#'      v.pageSizeOptions       = c(10, 20, 30),
#'      b.filterable            = TRUE,
#'      b.searchable            = TRUE
#' )


        #################################################################################
        # FUNCTION.
        #################################################################################

        f_reactable <- function(

                df.input,

                c.col.default.align     = "center",
                n.col.default.max.width = 500,

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

                v.col.link.label        = NULL,
                v.col.link.url          = NULL,
                v.col.link.name         = NULL,
                v.col.link.align        = NULL,
                v.col.link.width        = NULL,

                v.row.number.color      = NULL,
                c.row.number.color      = NULL,

                n.defaultPageSize       = 10,
                b.showPageSizeOptions   = TRUE,
                v.pageSizeOptions       = c(10, 20, 30),

                b.filterable            = TRUE,
                b.searchable            = TRUE
        ) {


        ######################################################################################
        # TEST
        ######################################################################################

        # ALWAYS
        # c.col.default.align     = "center"
        # n.col.default.max.width = 500
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
        # v.col.link.label         = NULL
        # v.col.link.url           = NULL
        # v.col.link.name          = NULL
        # v.col.link.align         = NULL
        # v.col.link.width         = NULL
        #
        # v.row.number.color      = NULL
        # c.row.number.color      = NULL
        #
        # n.defaultPageSize       = 10
        # b.showPageSizeOptions   = FALSE
        # v.pageSizeOptions       = c(10, 20, 30)
        #
        # b.filterable            = TRUE
        # b.searchable            = TRUE

        # DQ
        # df.input                = df.result
        # v.col.text              = c.col
        # v.col.text.name         = c.col
        # v.col.text.align        = "center"
        # v.col.text.width        = 300
        # v.col.link.label        = "pub.label"
        # v.col.link.url          = "pub.link"
        # v.col.link.name         = "Pub Link"
        # v.col.link.align        = "center"
        # v.col.link.width        = 150
        # n.defaultPageSize       = 5
        # b.showPageSizeOptions   = TRUE
        # v.pageSizeOptions       = c(5, 25, 50)
        # b.filterable            = TRUE
        # b.searchable            = FALSE


        ######################################################################################
        # ERROR CHECKS
        ######################################################################################

        ######################################################################################
        # INITIALIZATION
        ######################################################################################

        # Replace NA with "NA" in first column. First check that first column is non-numeric. This
        # is to prevent 'Can't convert <character> to <double>.' error.
        #if(class(df.input[[1]]) != "numeric") {df.input[is.na(df.input[[1]]), 1] <- "NA"}
        if(!is.numeric(df.input[[1]])) {df.input[is.na(df.input[[1]]), 1] <- "NA"}


        ######################################################################################
        # Format text.
        ######################################################################################

        # Update and check on v.col.text vectors.
        if(!is.null(v.col.text)) {

                # 'v.col.text.name'.
                if(is.null(v.col.text.name)) {

                        v.col.text.name <- v.col.text

                } else {

                        if(length(v.col.text) != length(v.col.text.name)) {

                                stop("Objects 'v.col.text' and 'v.col.text.name' must have equal number of items.")
                        }
                }

                # 'v.col.text.align'.
                if(is.null(v.col.text.align)) {

                        v.col.text.align  <- rep("left", length(v.col.text))

                } else {

                        if(length(v.col.text) != length(v.col.text.align)) {

                                stop("Objects 'v.col.text' and 'v.col.text.align' must have equal number of items.")
                        }
                }

                # 'v.col.text.width'.
                if(is.null(v.col.text.width)) {

                        v.col.text.width <- rep(120, length(v.col.text))

                } else {

                        if(length(v.col.text) != length(v.col.text.width)) {

                                stop("Objects 'v.col.text' and 'v.col.text.width' must have equal number of items.")
                        }
                }
        }


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
        # Format regular numbers.
        ######################################################################################

        # Update and check on v.col.digit vectors.
        if(!is.null(v.col.digit)) {

                # 'v.col.digit.name'.
                if(is.null(v.col.digit.name)) {

                        v.col.digit.name <- v.col.digit

                } else {

                        if(length(v.col.digit) != length(v.col.digit.name)) {

                                stop("Objects 'v.col.digit' and 'v.col.digit.name' must have equal number of items.")
                        }
                }

                # 'v.col.digit.align'.
                if(is.null(v.col.digit.align)) {

                        v.col.digit.align  <- rep("right", length(v.col.digit))

                } else {

                        if(length(v.col.digit) != length(v.col.digit.align)) {

                                stop("Objects 'v.col.digit' and 'v.col.digit.align' must have equal number of items.")
                        }
                }

                # 'v.col.digit.number'.
                if(is.null(v.col.digit.number)) {

                        v.col.digit.number <- rep(2, length(v.col.digit))

                } else {

                        if(length(v.col.digit) != length(v.col.digit.number)) {

                                stop("Objects 'v.col.digit' and 'v.col.digit.number' must have equal number of items.")
                        }
                }

                # 'v.col.digit.width'.
                if(is.null(v.col.digit.width)) {

                        v.col.digit.width <- rep(120, length(v.col.digit))

                } else {

                        if(length(v.col.digit) != length(v.col.digit.width)) {

                                stop("Objects 'v.col.digit' and 'v.col.digit.width' must have equal number of items.")
                        }
                }
        }


        # Update formatting of variable column names. Using sapply allows keeping the
        # item names (not available in lapply). Using simplify is false prevents
        # the list from being collapsed.
        # https://github.com/glin/reactable/issues/138
        l.colDef.digit <- sapply(

                v.col.digit,

                function(x) { # x = v.col.digit[9]

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
        # Format euro amounts.
        ######################################################################################

        # Update and check on v.col.euro vectors.
        if(!is.null(v.col.euro)) {

                # 'v.col.euro.name'.
                if(is.null(v.col.euro.name)) {

                        v.col.euro.name <- v.col.euro

                } else {

                        if(length(v.col.euro) != length(v.col.euro.name)) {

                                stop("Objects 'v.col.euro' and 'v.col.euro.name' must have equal number of items.")
                        }
                }

                # 'v.col.euro.align'.
                if(is.null(v.col.euro.align)) {

                        v.col.euro.align  <- rep("right", length(v.col.euro))

                } else {

                        if(length(v.col.euro) != length(v.col.euro.align)) {

                                stop("Objects 'v.col.euro' and 'v.col.euro.align' must have equal number of items.")
                        }
                }

                # 'v.col.euro.number'.
                if(is.null(v.col.euro.number)) {

                        v.col.euro.number <- rep(2, length(v.col.euro))

                } else {

                        if(length(v.col.euro) != length(v.col.euro.number)) {

                                stop("Objects 'v.col.euro' and 'v.col.euro.number' must have equal number of items.")
                        }
                }

                # 'v.col.euro.width'.
                if(is.null(v.col.euro.width)) {

                        v.col.euro.width <- rep(120, length(v.col.euro))

                } else {

                        if(length(v.col.euro) != length(v.col.euro.width)) {

                                stop("Objects 'v.col.euro' and 'v.col.euro.width' must have equal number of items.")
                        }
                }
        }




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
        # Format hyperlinks (link's).
        ######################################################################################

        # Update and check on v.col.link.label vectors.
        if(!is.null(v.col.link.label) | !is.null(v.col.link.url)) {

                # v.col.link.label. In case we have no label specified, we use the url itself
                # as label.
                if(is.null(v.col.link.label)) {

                        v.col.link.label <- v.col.link.url
                }

                # 'v.col.link.name'.
                if(is.null(v.col.link.name)) {

                        v.col.link.name <- v.col.link.label

                } else {

                        if(length(v.col.link.label) != length(v.col.link.name)) {

                                stop("Objects 'v.col.link.label' and 'v.col.link.name' must have equal number of items.")
                        }

                        if(length(v.col.link.url) != length(v.col.link.name)) {

                                stop("Objects 'v.col.link.url' and 'v.col.link.name' must have equal number of items.")
                        }
                }

                # 'v.col.link.align'.
                if(is.null(v.col.link.align)) {

                        v.col.link.align  <- rep("left", length(v.col.link))

                } else {

                        if(length(v.col.link.label) != length(v.col.link.align)) {

                                stop("Objects 'v.col.link.label' and 'v.col.link.align' must have equal number of items.")
                        }

                        if(length(v.col.link.url) != length(v.col.link.align)) {

                                stop("Objects 'v.col.link.url' and 'v.col.link.align' must have equal number of items.")
                        }
                }

                # 'v.col.link.width'.
                if(is.null(v.col.link.width)) {

                        v.col.link.width <- rep(120, length(v.col.link))

                } else {

                        if(length(v.col.link.label) != length(v.col.link.width)) {

                                stop("Objects 'v.col.link.label' and 'v.col.link.width' must have equal number of items.")
                        }

                        if(length(v.col.link.url) != length(v.col.link.width)) {

                                stop("Objects 'v.col.link.url' and 'v.col.link.width' must have equal number of items.")
                        }
                }
        }



        l.colDef.link <- sapply(

                v.col.link.url,

                function(x) { # x = v.col.link.url[1]

                        colDef(
                                name  = v.col.link.name[x == v.col.link.url],
                                align = v.col.link.align[x == v.col.link.url],
                                width = v.col.link.width[x == v.col.link.url],

                                cell = function(value, index) {

                                        htmltools::tags$a(

                                                href   = value,

                                                target = "_blank",

                                                df.input %>%

                                                        slice(index) %>%

                                                        pull(v.col.link.label[x == v.col.link.url])
                                        )
                                }
                        )
                },

                USE.NAMES = TRUE,
                simplify  = FALSE
        )


        ######################################################################################
        # PROCESS
        ######################################################################################

        reactable(

                data = df.input %>% select(-all_of(v.col.link.label)),

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

                        if (index %in% v.row.number.color) {

                                list(background = c.row.number.color)
                        }
                },

                columns = c(

                        l.colDef.text,

                        l.colDef.digit,

                        l.colDef.euro,

                        l.colDef.link
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

