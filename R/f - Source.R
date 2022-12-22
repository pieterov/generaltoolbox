#' @title Source latest file.
#'
#' @description Sources latest file.
#'
#' @author Pieter Overdevest
#'
#' @param script.to.source String in filename to source.
#' @param path Full path to folder containing the file.
#'
#' @returns None
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_source(
#'        script.to.source = "I - IWD",
#'        path             = paste0(path.root.local, "IWD/Projects/Initialization IWD")
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_source <- function(

                script.to.source,
                path
        ) {

                # Determine c.pattern.
                c.pattern <- paste0(script.to.source, ".*\\.R$")

                # Get all files in the path folder.
                df.file <- data.frame(full.path = list.files(

                        path        = path,
                        recursive   = FALSE,
                        full.names  = TRUE,
                        ignore.case = TRUE,
                        pattern     = c.pattern),

                        stringsAsFactors = FALSE
                )

                # Check whether a file was found.
                if (nrow(df.file) == 0) stop(paste0("There was no file found with string '", script.to.source,
                                                       "' in path '", path, ".'"))

                # Get the full path.
                df.file$filename <- basename(df.file$full.path)

                # Get the last modified date for each file.
                df.file$date.last.mod <- file.info(df.file$full.path)$mtime

                # Get boolean to express if file is the most recently modifed version. For example,
                # if there are two v01's. This will set TRUE to the most recently modified one.
                df.file$most.recent.mod.version <- ifelse(df.file$date.last.mod == max(df.file$date.last.mod),
                                                              TRUE, FALSE)

                # Get combined boolean which file to open.
                df.file$to.open <- ifelse((df.file$most.recent.mod.version),
                                              TRUE, FALSE)

                # Get file to open.
                df.file <- df.file[df.file$to.open, ]


                # Communicate stats to the user.
                cat(paste0("\nString   : ", script.to.source))

                cat(paste0("\nName     : ", df.file$filename))

                cat(paste0("\nLast mod : ", df.file$date.last.mod))

                cat("\n\n")

                print(round(difftime(Sys.time(), df.file$date.last.mod, units = "auto"), 1))

                cat("\n==========================\n")

                # Source latest version of the file. Mogelijk is dit bij PC computers wel noodzakelijk??
                # Op mijn mac heb ik de input 'encoding = 'UTF-8'' weggelaten. Op windows stond ie er wel in.
                # Het kan ook zijn dat Mac en cronR gecombineerd juist een probleem heeft met UTF-8 als encoding.
                #source(df.file$full.path, encoding = 'UTF-8')
                source(df.file$full.path)

        }
