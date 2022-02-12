##############################################################################
# NAME:         FUNCTION - UPLOAD FILE TO GOOGLE DRIVE
# AUTHOR:       Pieter Overdevest.
# DESCRIPTION:  Upload given file to Google Drive.
#               https://stackoverflow.com/questions/57792566/how-do-i-upload-download-with-googledrive-package-using-r
##############################################################################

        f_upload_file_to_google_drive <- function(

                # String in the filename
                c.file.string,

                c.file.type,

                # Path where file can be found.
                c.path,

                # Exact match or not.
                b.exact.match            = FALSE,
                c.file.string.exclude    = NULL,

                # Google Drive folder.
                c.folder
                ) {


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
