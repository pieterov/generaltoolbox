#' @title OCR table data from PDF
#'
#' @description OCR table data from PDF.
#'
#' @author Pieter Overdevest
#'
#' @param c.file.string Filename (or part thereof) of PDF to be OCR-ed.
#' @param c.path.source Path where PDF file can be found.
#' @param c.path.destination Path were resulting XLS should be saved.
#'
#' @returns List of OCR-ed text
#'
#' @details -
#'
#' @export
#'
#' @examples
#' l.text <- f_ocr_table_data_from_pdf(
#'
#'        c.file.string,
#'        c.path.source,
#'        c.path.destination
#' )


#################################################################################
# FUNCTION.
#################################################################################


#################################################################################
# NAME:         FUNCTION - OCR TABLE DATA FROM PDF.
# AUTHOR:       Pieter Overdevest
# DESCRIPTION:  OCR table data from PDF.
#################################################################################

        f_ocr_table_data_from_pdf <- function(

                c.file.string,
                c.path.source,
                c.path.destination
                ) {


##################################################################################
# Testing
##################################################################################

        # c.file.string      <- "test"
        # c.path.source      <- path.data
        # c.path.destination <- path.deliverables


##################################################################################
# Error check
##################################################################################

##################################################################################
# Process
##################################################################################

        # Determine file.
        c.full.path <- f_get_latest_file(

                c.file.string = c.file.string,
                c.file.type   = "pdf",
                c.path        = c.path.source
        )

        # Convert pdf pages to TIFFs.
        img_file <- pdftools::pdf_convert(

                pdf    = c.full.path,
                format = 'tiff',
                dpi    = 400
        )


        # Read text from TIFFs.
        l.text.source <- ocr(img_file)


        # Clean up - phase 1.
        l.text <- l.text.source %>%

                lapply(function(x) { # x <- l.text.source[[1]]

                        x %>%

                                str_split(pattern = "\n") %>%

                                lapply(function(x) str_split(x, pattern = " ")) %>%

                                .[[1]]
                })


        # Determine largest length of the rows.
        l.max <- l.text %>%

                lapply(function(x) { # x <- l.text[[1]]

                        lapply(x, function(y) { # y <- x[[1]]

                                y %>%

                                        length()
                        }) %>%

                                unlist() %>%

                                max()
                })

        # Add "" to make each row of equal length to max length.
        # Convert to matrices.
        l.text.update <- mapply(

                function(x, y) { # x <- l.text[[1]]; y <- l.max[[1]]

                        sapply(x, function(z) { # z <- x[[1]]

                                c(z, rep("", y - length(z)))
                        }) %>%

                                t()
                },

                x = l.text,
                y = l.max)


        # Write data
        f_write_data_to_file(

                x             = l.text.update,
                v.path        = c.path.destination,
                c.file.string = c.file.string
        )

##################################################################################
# Return
##################################################################################

        return(l.text.updated)

        }
