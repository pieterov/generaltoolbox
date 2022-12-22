#' @title Upload given file to Google Drive
#'
#' @description Uploads given file to Google Drive.
#'
#' @author Pieter Overdevest
#'
#' @param c.file.string Core name of the file to upload.
#' @param c.file.type File type of file to upload.
#' @param c.path Path holding the file to upload.
#' @param c.folder Google folder where to upload the file.
#' @param b.exact.match Should file name have exact match with c.file.string (default: FALSE)
#' @param c.file.string.exclude Any string to exclude files for (default: NULL).
#' @param c.show.report What info should be printed to console when searching for files (default: "all")
#'
#' @returns Nothing.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_upload_file_to_google_drive(
#'
#'      c.file.string         = "Daisycon Feed",
#'      c.file.type           = "csv",
#'      c.path                = path.deliverables,
#'      c.folder              = "12. Data Science BUHV-Oletti/",
#'      b.exact.match         = FALSE,
#'      c.file.string.exclude = NULL,
#'      c.show.report         = "all"
#' )


        #################################################################################
        # FUNCTION.
        #################################################################################

        f_upload_file_to_google_drive <- function(

                # String in the filename
                c.file.string,

                c.file.type,

                # Path where file can be found.
                c.path,

                # Google Drive folder.
                c.folder,

                # Exact match or not.
                b.exact.match         = FALSE,
                c.file.string.exclude = NULL,
                c.show.report         = "all"

                ) {


        # https://stackoverflow.com/questions/57792566/how-do-i-upload-download-with-googledrive-package-using-r


        ##############################################################################
        # TEST ONLY!!
        ##############################################################################

        # # Always
        # b.exact.match          <- FALSE
        # c.file.string.exclude  <- NULL
        #
        # # Test
        # c.file.string          <- "Daisycon Feed"
        # c.file.type            <- "csv"
        # c.path                 <- path.deliverables
        # c.folder               <- "12. Data Science BUHV-Oletti/"


        ##############################################################################
        # INITIALIZE
        ##############################################################################

        # Get latest file in case other file than Google Sheet is read.
        c.path.file <- f_get_latest_file(

                        c.file.string         = c.file.string,
                        c.file.type           = c.file.type,
                        c.path                = c.path,
                        b.exact.match         = b.exact.match,
                        c.file.string.exclude = c.file.string.exclude,
                        c.show.report         = c.show.report
        )


        cat("\n")

        # Info on destination folder
        df.destination <- drive_get(path = c.folder)

        cat("\n")


        ##############################################################################
        # ERRROR CHECK
        ##############################################################################

        if(nrow(df.destination) > 1) {

                stop(paste0(
                        "Note, we observe ", nrow(df.destination), " folders on Google Drive that fit the requested string ('", c.folder, "'), ",
                        "namely, ", f_paste(paste0("https://drive.google.com/drive/u/0/folders/", df.destination$id))
                ))
        }


        ##############################################################################
        # PROCESS
        ##############################################################################

        # Upload file in my own Google Drive
        drive_upload(

                media = c.path.file,
                path  = as_id(df.destination$id)
        )

        # Comms to user.
        cat(paste0(
                "\nThe file shown above was successfuly uploaded to Google Drive, in folder, ",
                paste0("https://drive.google.com/drive/u/0/folders/", df.destination$id)
        ))

        }
