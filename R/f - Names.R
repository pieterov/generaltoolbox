#################################################################################
# NAME:         FUNCTION - Names.
# AUTHOR:       Pieter Overdevest.
# DESCRIPTION:  Give names of data frame.
#################################################################################

        f_names <- function(df,
                            b.sort.name  = TRUE,
                            b.sort.class = TRUE) {

##############################################################################
# TEST ONLY!!
##############################################################################

        # df <- data.frame(pieter = c("sd", "sd", "ds", "ds", "sd"),
        #                 x = c("", "na", "N/A", 0, NA),
        #                 y = c(1,2,3,4,5),
        #                 w = as.factor(c("ioo","oioi", "oio", "oio", "ioo")))

        # df <- df.temp1


##############################################################################
# Initialize // Error checking.
##############################################################################

        # Check that input is data frame.
        if (!any(class(df) == "data.frame"))

                stop("Input should be data frame.")


        if (identical(names(df), character(0))) {

                warning("Data frame has no names.")

                return(cat(""))

        }


##############################################################################
# Analyse data.
##############################################################################

        # Print header.
        cat("\nBasic Info Data Frame:\n\n")

        # Create basic info data frame.
        df.basic.info <- data.frame(Name  = c("=============", names(df)),
                                    Class = c("=============", unlist(sapply(df, function(x) paste(class(x), collapse = "; "))))
                                    )

        if (b.sort.name) {

                if (b.sort.class) {

                        df.basic.info <- df.basic.info %>% arrange(Class, Name)
                } else {

                        df.basic.info <- df.basic.info %>% arrange(Name)
                }

        }

        rownames(df.basic.info) <- c("", seq(nrow(df.basic.info)-1))

        # Show in console, left align.
        print(x          = df.basic.info,
              #row.names = FALSE,
              right      = FALSE)

        }
