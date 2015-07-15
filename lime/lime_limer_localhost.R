library(devtools)
# install_github("petrbouchal/limer")

library(limer)

options(lime_api = 'http://localhost/limesurvey/admin/remotecontrol')
options(lime_username = 'admin')
options(lime_password = 'admin')

# use_proxy("proxy2",8080)
# Sys.setenv(http_proxy="http://proxy2:8080")


session_key <- limer::get_session_key()
responses <- get_responses(282299)

get_responses(282299)

raw_data <- call_limer(method = "export_responses", 
                       params = list(iSurveyID = 282299, 
                                     sDocumentType = "csv", 
                                     sLanguageCode = "en", 
                                     sCompletionStatus = "complete", 
                                     sHeadingType = "code", 
                                     sResponseType = "long"))
base64_to_df(raw_data)