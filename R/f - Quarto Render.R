##############################################################################################
# NAME:         FUNCTION - QUARTO RENDER
# AUTHOR:       Pieter Overdevest
##############################################################################################

        f_quarto_render <- function(

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


        #################################################################################
        # ERROR CHECK
        #################################################################################

        # Does c.file.source end with '.qmd'?
        if(!grepl("\\.qmd$", c.file.source)) {

                stop("The source file (c.file.source) '", c.file.source, "' must end with '.qmd'!\n")
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

                stop("The source file (c.file.source), '", c.file.source, "', does not occur ",
                     "in the source folder (c.folder.source), '", c.folder.source, "'!")
        }


        ###############################################################################
        # INITIALIZATION
        ###############################################################################

        # Get original working directory.
        c.original.working.directory <- paste0(getwd(), "/")

        # Set new working directory.
        setwd(c.folder.source)

        # Destination file.
        c.file.destination <- ifelse(

                is.null(c.file.destination),

                paste0(
                        format(Sys.time(), "%Y %m %d"), " - ",

                        format(Sys.time(), "%H %M %S"), " - ",

                        gsub("qmd$", "html", c.file.source)
                ),

                paste0(c.file.destination, ".html")
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

