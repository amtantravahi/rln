# Adi Tantravahi

################## Lexis API authorization function
lexis_auth <- function(key, secret) {


  lexis <- httr::oauth_endpoint(request = "https://auth-api.lexisnexis.com/oauth/v2/token",
                                authorize = "https://auth-api.lexisnexis.com/oauth/v2/authorize",
                                access = "https://auth-api.lexisnexis.com/oauth/v2/token")



  auth.code <<- httr::oauth2.0_token(endpoint = lexis,
                                     app = httr::oauth_app(appname = "lexis_api",
                                                           key=key, secret=secret),
                                     scope = "http://oauth.lexisnexis.com/all",
                                     client_credentials = T,
                                     use_basic_auth = T,
                                     user_params = lexis,
                                     cache = F)
  }
