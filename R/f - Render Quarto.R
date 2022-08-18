##############################################################################################
# NAME:         FUNCTION - RENDER QUARTO
# AUTHOR:       Pieter Overdevest
##############################################################################################

        f_render_quarto <- function(

                c.folder.source,
                c.file.source,
                c.folder.destination,
                c.file.destination = NULL
        )

        {

        ###############################################################################
        # TESTING
        ###############################################################################

        # c.folder.source      = paste0(path.code, "2022-06-02_Quarto - R-Ladies Freiburg/")
        # c.file.source        = "Quarto demonstration - test.qmd"
        # c.folder.destination = path.deliverables
        # c.file.destination   = NULL

        # c.folder.source      = path.code
        # c.file.source        = "Verkoopoverzicht.qmd"
        # c.folder.destination = paste0(path.deliverables, "Verkoopoverzichten/")
        # c.file.destination   = glue("Verkoopoverzicht - {c.period.new} - {c.partner.order}")

        # c.folder.source      = path.code
        # c.file.source        = "Verkoopoverzicht - Per Partner - PDF.qmd"
        # c.folder.destination = paste0(path.deliverables, "Verkoopoverzichten/")
        # c.file.destination   = glue("Verkoopoverzicht - {c.period.new} - {c.partner.order}")


        #################################################################################
        # ERROR CHECK
        #################################################################################

        # Does c.file.source end with '.qmd'?
        if(!grepl("\\.qmd$", c.file.source)) {

                f_stop(
                        "The source file (c.file.source) '{c.file.source}' must end with '.qmd'!\n"
                )
        }


        # Does c.file.source contain 'HTML' or 'PDF'?
        if(!(grepl("HTML", c.file.source) | grepl("PDF", c.file.source))) {

                f_stop(
                        "The source file (c.file.source) '{c.file.source}' does not contain
                        'HTML' or 'PDF'!\n"
                )
        }


        # Is c.file.source in c.folder.source?
        if(!c.file.source %in% (

                f_get_filenames_in_folder(

                        c.path       = c.folder.source,
                        b.recursive  = FALSE,
                        c.file.type  = "qmd",
                        b.return.md5 = FALSE
                ) %>%

                pull(file.name.ext)
        )) {

                f_stop(
                        "The source file (c.file.source), '{c.file.source}', does not occur
                        in the source folder (c.folder.source), '{c.folder.source}'!"
                )
        }


        ###############################################################################
        # INITIALIZATION
        ###############################################################################

        # Get original working directory.
        c.original.working.directory <- paste0(getwd(), "/")

        # Set new working directory.
        setwd(c.folder.source)

        # Destination file.
        c.file.destination <- paste0(

                format(Sys.time(), "%Y %m %d"), " - ",

                format(Sys.time(), "%H %M %S"), " - ",

                ifelse(
                        is.null(c.file.destination),

                        gsub("\\.qmd$", "", c.file.source),

                        c.file.destination
                ),

                ".",

                case_when(
                        grepl("HTML", c.file.source) ~ "html",
                        grepl("PDF", c.file.source)  ~ "pdf",
                        TRUE                         ~ "unknown extension"
                )
        )


        ###############################################################################
        # Rendering.
        ###############################################################################

        # Render Quarto file.
        quarto_render(

                input       = c.file.source,

                output_file = c.file.destination
        )


        ###############################################################################
        # Move file to path destination.
        ###############################################################################

        file.copy(

                from = paste0(c.folder.source, c.file.destination),
                to   = paste0(c.folder.destination, c.file.destination)
        )

        file.remove(

                file1 = paste0(c.folder.source, c.file.destination)
        )

        ###############################################################################
        # Set working directorry back to what it was.
        ###############################################################################

        # Set new working directory.
        setwd(c.original.working.directory)
}

