library(httr)
library(jsonlite)
library(base64enc)

#----------------------------------------
# Functions to access RemoteControl API
#----------------------------------------
get_session_key <- function(username, password) {
  body.json = list(method = "get_session_key",
                   id = 1,
                   params = list(admin = username,
                                 password = password))
  
  # Need to use jsonlite::toJSON because single elements are boxed in httr, which 
  # is goofy. toJSON can turn off the boxing automatically, though it's not 
  # recommended. They say to use unbox on each element, like this:
  #   params = list(admin = unbox("username"), password = unbox("password"))
  # But that's a lot of extra work. So auto_unbox suffices here.
  # More details and debate: https://github.com/hadley/httr/issues/159
  r <- POST(LIME_API, content_type_json(),
            body = toJSON(body.json, auto_unbox = TRUE))
  
  session_key <- fromJSON(content(r))$result
  return(session_key)
}

export_resposes <- function(session_key, survey_id) {
  body.json <- list(method = "export_responses",
                    id = 1,
                    params = list(sSessionKey = session_key,
                                  iSurveyID = survey_id,
                                  DocumentType = "csv",
                                  sLanguageCode = "en",
                                  sHeadingType = "full"))
  
  r <- POST(LIME_API, content_type_json(), 
            body = toJSON(body.json, auto_unbox = TRUE))
  
  # The API returns a base 64 encoded string, so it has to be decoded. R decodes 
  # it as raw bytes, which then have to be converted into characters. That raw 
  # text then has to be treated as a file with textConnection
  raw_csv <- rawToChar(base64decode(fromJSON(content(r, as ="text"))$result))
  
  return(read.csv(textConnection(raw_csv)))
}

release_session_key <- function(session_key) {
  body.json <- list(method = "release_session_key",
                    id = 1,
                    params = list(sSessionKey = session_key))
  
  r <- POST(LIME_API, content_type_json(), 
            body = toJSON(body.json, auto_unbox = TRUE))
  return(fromJSON(content(r))$result)
}


#-------------------------
# Get actual survey data
#-------------------------
LIME_API <- "http://localhost/limesurvey/index.php/admin/remotecontrol"
USERNAME <- "admin"
PASSWORD <- "admin"
SURVEY_ID <- "282299"

session_key <- get_session_key(USERNAME, PASSWORD)
final.data <- export_resposes(session_key, SURVEY_ID)
release_session_key(session_key)