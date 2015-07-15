library(devtools)
# install_github("petrbouchal/limer")

library(limer)

options(lime_api = 'https://demo.limesurvey.org/index.php?r=admin/remotecontrol')
options(lime_username = 'admin')
options(lime_password = 'test')

session_key <- limer::get_session_key()
responses <- get_responses(888224)

get_responses(888224)

raw_data <- call_limer(method = "export_responses", 
                       params = list(iSurveyID = 888224, 
                                     sDocumentType = "csv", 
                                     sLanguageCode = "en", 
                                     sCompletionStatus = "complete", 
                                     sHeadingType = "code", 
                                     sResponseType = "long"))
base64_to_df(raw_data)

