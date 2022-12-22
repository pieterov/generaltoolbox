#' @title Download file from Google Drive
#'
#' @description Downloads file from Google Drive.
#'
#' @author Pieter Overdevest
#'
#' @param c.file.string -----
#' @param c.folder -----
#' @param c.path -----
#' @param b.overwrite Should local file be overwritten? (default: FALSE)
#'
#' @returns Nothing.
#'
#' @details -
#'
#' @export
#'
#' @examples
#' f_download_file_from_google_drive(
                #'
#'     c.file.string,
#'     c.folder,
#'     c.path,
#'     b.overwrite
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        # https://stackoverflow.com/questions/57792566/how-do-i-upload-download-with-googledrive-package-using-r


        f_download_file_from_google_drive <- function(

                # String in the filename
                c.file.string,

                # Google Drive folder.
                c.folder,

                # Path where file can be found.
                c.path,

                # Exact match or not.
                b.overwrite = FALSE
        ) {


        ##############################################################################
        # TEST ONLY!!
        ##############################################################################

        # c.file.string = "Google Merchant Export"
        # c.folder      = "8. Data Science BUHV-Oletti/Data/Affiliate Marketing/Google/"
        # c.path        = path.data


        ##############################################################################
        # INITIALIZE
        ##############################################################################

        cat("\n")

        # Folder info.
        df.google.drive.folder <- drive_get(path = c.folder)
        c.folder.id            <- df.google.drive.folder$id

        cat("\n")

        # File info.
        df.google.drive.file <- drive_find(pattern = c.file.string) %>%

                mutate(
                        timestamp = name %>% str_extract("[0-9]{4} [0-9]{2} [0-9]{2}") %>% ymd(),
                        folder.id = lapply(drive_resource, function(x) { x$parents[[1]] }) %>% unlist()
                ) %>%

                filter(folder.id %in% c.folder.id) %>%

                arrange(desc(timestamp)) %>%

                slice(1)

        cat("\n")


        ##############################################################################
        # ERRROR CHECK
        ##############################################################################

        if(nrow(df.google.drive.folder) > 1) {

                stop(paste0(
                        "Note, we observe ", nrow(df.google.drive.folder), " folders on Google Drive that fit the requested string, '", c.folder, "', ",
                        "namely, ", f_paste(paste0("https://drive.google.com/drive/u/0/folders/", df.google.drive.folder$id))
                ))
        }


        if(nrow(df.google.drive.file) == 0) {

                stop(paste0(
                        "Note, we observe no file(s) on Google Drive that fit the requested string, '", c.file.string, "', ",
                        "in folder, '", c.folder, "'!"
                ))
        }


        ##############################################################################
        # PROCESS
        ##############################################################################

        # Upload file in my own Google Drive
        drive_download(

                file      = as_id(df.google.drive.file$id),
                path      = paste0(c.path, df.google.drive.file$name),
                overwrite = b.overwrite
        )

        # Comms to user.
        cat(paste0(
                "\nThe file shown above was successfuly downloaded from Google Drive into folder,\n",
                c.path, "\n"
        ))

        }
