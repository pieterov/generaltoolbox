#' @title Send Slack Comment.
#'
#' @description Sends Slack comment.
#'
#' @author Pieter Overdevest
#'
#' @param c.slack.hook String with slack hook.
#' @param c.message Message to communicate.
#'
#' @returns None
#'
#' @details How to obtain a Slack Hook:
#' - Go to https://api.slack.com/apps
#' - If needed, create a new app
#' - Go to ‘Incoming Webhooks’
#' - If not already, activate Incoming Webhooks
#' - Add new webhook to workspace
#' - Select channel
#' - Copy webhook and use for POST() function.
#' - Add app to Slack by ‘Add apps’, or the channel may already be pointing to it for you
#'
#' @export
#'
#' @examples
#' f_send_slack_comment(
#'        c.slack.hook = "https://hooks.slack.com/services/........./......../...........",
#'        c.message    = "Hello World!"
#' )

        #################################################################################
        # FUNCTION.
        #################################################################################

        f_send_slack_comment <- function(

                c.slack.hook,
                c.message
        ) {



        c.marker  <- ":white_check_mark:" #":x:"

        c.success <- paste(
                "Hi Mick - en hier een mooie boodschap! Het plaatje (L02) is simpelweg een URL naar een plaatje op het web",
                "en dit kan dus ook een plaatje worden op de pictureserver, of any other link naar een plaatje.",
                "De buttons hieronder kunnen we uitbreiden."
        )


        c.message.variable <- paste0(

                "Test0: ", strrep("\t", 1),
                9999, "\n",

                "Test1: ", strrep("\t", 1),
                9999, "\n",

                "Test2: ", strrep("\t", ),
                9999, "\n"
        )

        c.image.url <- "https://verkeersregels.vvn.nl/sites/default/files/styles/traffic_sign/public/L02.png?itok=vh-WBj1Y"



        c.message <- paste0(

                "{'blocks':[

                        {
                                'type': 'section',

                                'text': {

                                        'type': 'mrkdwn',
                                        'text': '*",
                                                c.marker, "\t" , "HIER KAN EEN TITEL KOMEN", "*\n\n",
                                                c.success, "\n\n", c.message.variable, "'
                                },

                                'accessory': {

                			'type': 'image',
                			'image_url': '", c.image.url, "',
                			'alt_text': '", "ALTERNATIVE", "'
                		}
                        },

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


