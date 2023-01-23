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
        c.message.list <- paste(paste("-", v.message.list), collapse = "\n")


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
                        			'url': '", "https://ipsm.nl/dumps/", "'
                        		},

                                        {
                        			'type': 'button',
                        			'text': {
                        				'type': 'plain_text',
                        				'text': ':spiral_note_pad: Synology'
                        			},
                        			'url': '", "http://gofile.me/5TdQw/7dJbToZlO/", "'
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



        POST(
                url    = c.slack.hook,
                encode = "json",
                body   = c.message
        )

        }
