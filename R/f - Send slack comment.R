#' @title Send Slack Comment.
#'
#' @description Sends Slack comment.
#'
#' @author Pieter Overdevest
#'
#' @param c.slack.hook String with slack hook.
#' @param c.marker String to code the marker: 'v' for checkbox and 'x' for white cross.
#' @param c.title Title of message (string).
#' @param c.message.main Main message (string).
#' @param v.message.list Message to communicate (list with strings).
#' @param c.image.url Link to image to show in Slack message (default NULL).
#'
#' @returns None
#'
#' @details How to obtain a Slack Hook:
#'
#' * Go to https://api.slack.com/apps
#'
#' * If needed, create a new app
#'
#' * Go to ‘Incoming Webhooks’
#'
#' * If not already, activate Incoming Webhooks
#'
#' * Add new webhook to workspace
#'
#' * Select channel
#'
#' * Copy webhook and use for POST() function.
#'
#' * Add app to Slack by ‘Add apps’, or the channel may already be pointing to it for you
#'
#'
#' @export
#'
#' @examples
#' f_send_slack_comment(
#'        c.slack.hook   = "https://hooks.slack.com/services/........./......../...........",
#'        c.marker       = "xxx",
#'        c.title        = "A greate title!",
#'        c.message.main = "The main message.",
#'        v.message.list = c("Hello World!", "This is item 2", "And a third item"),
#'        c.image.url    = "https://media.licdn.com/dms/image/C560BAQEGsMeS8zRHcg/company-logo_200_200/0/1612876167821?e=1682553600&v=beta&t=ypO6V4vhFioktDnqLdCKra0K6IGaI2bl6klfHaxA0lw"
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_send_slack_comment <- function(

                c.slack.hook,
                c.marker,
                c.title,
                c.message.main,
                v.message.list,
                c.image.url = NULL
        ) {

        # TESTING - Succesfull download
        # c.marker        = "v"
        # c.title         = c.file.string
        # c.message.main  = "File was successfully downloaded from Dumps."
        # v.message.list  = "See Synology"
        # c.image.url     = c.image.url.hrgroep


        # Translate marker(s).
        c.marker  <- lapply(

                c.marker %>% strsplit("") %>% unlist(),

                function(x) {

                        if(x == "v") {

                                ":white_check_mark:"

                        } else if(x == "x") {

                                ":x:"

                        } else {

                                stop("Unknown marker.")
                        }

                }) %>% unlist() %>% paste0(collapse = "")


        # Prepare message from list.
        c.message.list <- paste(

                paste0(
                        # Don't add '-' in case v.message.list contains one element.
                        ifelse(length(v.message.list) == 1, "", "- "),
                        v.message.list
                ),

                collapse = "\n"
        )


        # URL message in case URL is provided.
        c.image.url.message <- ifelse(

                is.null(c.image.url),
                "",
                paste0(", 'accessory': {'type': 'image','image_url': '", c.image.url, "','alt_text': 'ALTERNATIVE'}")
        )


        # Compose message.
        c.message <- paste0(

                "{'blocks':[

                        {
                                'type': 'section',

                                'text': {

                                        'type': 'mrkdwn',
                                        'text': '*",
                                                c.marker, "\t" , c.title, "*\n\n",
                                                c.message.main, "\n\n", c.message.list, "'
                                }", c.image.url.message,
                        "},

                        {
                        	'type': 'actions',

                        	'elements': [

                                         {
                        			'type': 'button',
                        			'text': {
                        				'type': 'plain_text',
                        				'text': ':spiral_note_pad: Dumps'
                        			},
                        			'url': '", c.ipsm.dumps, "'
                        		},

                                        {
                        			'type': 'button',
                        			'text': {
                        				'type': 'plain_text',
                        				'text': ':spiral_note_pad: Log'
                        			},
                        			'url': '", c.ipsm.allocatie.log, "'
                                        },

                                        {
                        			'type': 'button',
                        			'text': {
                        				'type': 'plain_text',
                        				'text': ':spiral_note_pad: Synology'
                        			},
                        			'url': '", c.ipsm.synology.data, "'
                        		},

                                        {
                        			'type': 'button',
                        			'text': {
                        				'type': 'plain_text',
                        				'text': ':spiral_note_pad: IPSm'
                        			},
                        			'url': '", "https://ipsm.nl/", "'
                                        }
                        	]
                        },

                	{
                		'type': 'context',
                		'elements': [
                			{
                				'type': 'plain_text',
                				'text': ' Timestamp of message: ", Sys.time() , "',
                				'emoji': true
                			}
                		]
                	},

                        {
                		'type': 'divider'
                	}

                ]}")




        # How many times to attempt to send Slack.
        n_counter_max <- 5

        # Initialize
        b_continue <- TRUE
        n_counter  <- 1

        # Try for at most n_counter_max times.
        while (n_counter <= n_counter_max & b_continue) {

                # Comms to user.
                cat(paste0("\n\n", now(), " - Attempt send to Slack ", n_counter, " of ", n_counter_max, ".\n"))

                # Ask R to try to download each hour.
                result <- try({

                        httr::POST(
                                url    = c.slack.hook,
                                encode = "json",
                                body   = c.message
                        )

                        # Testing.
                        #b=a+2

                }, silent = FALSE)

                # Comms to user.
                cat(paste0(
                        "\n", now(), " - Attempt send to Slack ", n_counter, " of ", n_counter_max,
                        " passed the 'POST()' function. The result is as follows:\n\n"
                ))

                print(result)

                #cat("\n")

                # Check result of attempt. If attempt was succesful.
                if(class(result) == "NULL") {

                        b_continue <- FALSE

                        # Comms to user.
                        cat(paste0(
                                "\n", now(), " - Attempt send to Slack ", n_counter, " of ", n_counter_max,
                                " was successful!\n"
                        ))

                        return()

                # If attempt was not succesfull, we try again after we wait 15 sec.
                } else {

                        # Comms to user.
                        cat(paste0(
                                "\n", now(), " - Attempt send to Slack ", n_counter, " of ", n_counter_max,
                                " was not successful. We will try again.\n"
                        ))

                        Sys.sleep(15)

                        n_counter <- n_counter + 1
                }
        }

        # Comms to user.
        cat(paste0(
                "\n", now(), " - Attempt send to Slack ", n_counter, " of ", n_counter_max,
                " was not successful. We tried ", n_counter_max,
                " times and we will not try again.\n"
        ))


        }
