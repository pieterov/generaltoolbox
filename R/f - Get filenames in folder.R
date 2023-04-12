#' @title Get names and information on files in folder
#'
#' @description Get names and information on files in folder.
#'
#' @author Pieter Overdevest
#'
#' @param c.path Full path to folder of interest.
#' @param b.recursive Should we look inside subfolders (default: FALSE).
#' @param c.file.type What file type to retrieve? (default: NULL).
#' @param b.return.md5 Should MD5 be returned? (default: FALSE).
#'
#' @returns Data frame with information on the files observed in concerned folder.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' df.output <- f_get_filenames_in_folder(
#'
#'     c.path,
#'     b.recursive,
#'     c.file.type,
#'     b.return.md5
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_get_filenames_in_folder = function(

                c.path,
                b.recursive  = FALSE,
                c.file.type  = NULL,
                b.return.md5 = FALSE
        ) {


        #################################################################################
        # TEST!
        #################################################################################

        # ALTIJD
        # b.recursive  = FALSE
        # c.file.type  = NULL
        # b.return.md5 = FALSE

        # c.path       = c.path.source
        # b.recursive  = FALSE
        # c.file.type  = NULL

        # c.path       = c.path.destination
        # b.recursive  = FALSE
        # c.file.type  = NULL

        # c.path       = paste0(path.iwd, "Documents/")
        # b.recursive  = FALSE
        # c.file.type  = NULL

        # c.path       = path.data
        # b.recursive  = FALSE
        # c.file.type  = "xls"
        # b.return.md5 = TRUE

        # c.path       = path.datachamp.dropbox
        # c.file.type  = "csv"

        # c.path       = c.folder.source
        # b.recursive  = FALSE
        # c.file.type  = "qmd"
        # b.return.md5 = FALSE

        # BLC
        # c.path       = c.folder.source
        # b.recursive  = FALSE
        # c.file.type  = "qmd"
        # b.return.md5 = FALSE


        #################################################################################
        # ERROR CHECK
        #################################################################################

        if(!dir.exists(c.path)) {

                stop("The folder '", c.path, "' does not exist!\n\n")
        }


        #################################################################################
        # INITIALIZE
        #################################################################################

        if(is.null(c.file.type)) {

                c.pattern <- NULL

        } else {

                c.pattern <- paste0("\\.", c.file.type, "[xm]?$")

                }


        #################################################################################
        # MAIN BODY
        #################################################################################

        # Get all files in the path folder that match c.pattern.
        # path	        a character vector of path names. The default corresponds to the working directory returned by getwd(). You can specify more than one directory path.
        # pattern	a character string containing a regular expression. If the basename of a file matches the regular expression, it is returned. See the regexpr function for details on regular expressions. By default, NULL, specifying returning all files found in path.
        # all.files	a logical value. If TRUE, the entire contents of directory are returned. If FALSE (the default, only visible (non-hidden) file or directory names are returned. (Hidden files or directories are those with names that begin with a period.)
        # full.names	a logical value. If TRUE, the directory path is prepended to the returned names. If FALSE (the default), only the file or directory name is in the return value.
        # recursive	a logical value. If TRUE, the file names in all directory paths are listed recursively. When include.dirs=TRUE, the names of subdirectories are also returned. The default is FALSE.
        # ignore.case	a logical value. If TRUE, uppercase and lowercase characters are considered equivalent when matching. The default is FALSE.
        # include.dirs	a logical value. Only used when recursive=TRUE. If TRUE, both names of files and subdirectories are returned. Otherwise, only names of files are returned.

        df.file <- tibble(

                full.path = list.files(

                        # We remove the '/' at the end to prevent a double '/' in the full.path.
                        path         = gsub("/$", "", c.path),
                        recursive    = b.recursive,
                        full.names   = TRUE,
                        ignore.case  = TRUE,
                        pattern      = c.pattern
                )
        )


        # Check whether a file was found.
        if (nrow(df.file) == 0) {

                glue(
                        "No files found of {ifelse(is.null(c.file.type), 'any', c.file.type)} type in path '{c.path}'!\n\n"
                )

        }


        # Clean up the file names and get the concerned file name.
        df.output <- df.file %>%

                # Is observation a file or a folder?
                mutate(is.dir = file.info(full.path)$isdir) %>%

                # Verwijder directories
                filter(!is.dir) %>%

                mutate(
                        # Determine file and folder name.
                        folder.name       = dirname(full.path),
                        file.name.ext     = basename(full.path),
                        file.name         = gsub("\\.[a-zA-Z]*$", "", file.name.ext),
                        file.name.core    = gsub("^[0-9]{4} [0-9]{2} [0-9]{2} - ", "", file.name),
                        file.name.core    = gsub("^[0-9]{2} [0-9]{2} [0-9]{2} - ", "", file.name.core),
                        file.ext          = str_extract(file.name.ext, "[a-zA-Z]*$"),

                        # Is observation a hidden file?
                        is.hidden         = grepl("^~", file.name),

                        # Get the last modified date for file.
                        date.last.mod     = as_date(file.info(full.path)$mtime),

                        # Get the last modified datetime for file.
                        datetime.last.mod = as_datetime(file.info(full.path)$mtime),

                        # File size (kB).
                        file.size.kb      = file.info(full.path)$size / 1024,

                        # Extract date from file name.
                        date.in.file.name = file.name %>%

                                str_extract("^[0-9]{4} [0-9]{2} [0-9]{2}") %>%
                                ymd(),

                        time.in.file.name = file.name %>%

                                gsub("^[0-9]{4} [0-9]{2} [0-9]{2} - ", "", .) %>%
                                str_extract("^[0-9]{2} [0-9]{2} [0-9]{2}"),

                        contains.date     = !is.na(date.in.file.name),
                        contains.time     = !is.na(time.in.file.name)
                )


        # Added 'quiet = TRUE' because hms() gives warning when it cannot parse time, or all are NA.
        df.output <- df.output %>%

                mutate(
                        time.in.file.name = lubridate::hms(time.in.file.name, quiet = TRUE)
                ) %>%

                arrange(date.in.file.name, time.in.file.name)



        # Voeg md5 toe, indien gevraagd.
        if(b.return.md5) {

                df.output <- df.output %>%

                        mutate(
                                file.md5 = tools::md5sum(full.path)
                        )
                }


        #################################################################################
        # COMMUNICATION
        #################################################################################

        if(sum(df.output$is.hidden) > 0) {

                glue(
                        "The folder '{c.path}' contains {sum(df.output$is.hidden)} case(s) of hidden files. These are ",
                        "included in the listed output:\n {paste(df.output %>% filter(!is.hidden) %>% pull(file.name),
                        collapse = '\n')}\n\n"
                )
        }


        #################################################################################
        # RETURN
        #################################################################################

        return(df.output)

}
