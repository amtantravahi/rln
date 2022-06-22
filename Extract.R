# Adi Tantravahi

################## Lexis API authorization function
lexis_auth <- function(key, secret) {
  
  
  lexis <- oauth_endpoint(request = "https://auth-api.lexisnexis.com/oauth/v2/token",
                          authorize = "https://auth-api.lexisnexis.com/oauth/v2/authorize",
                          access = "https://auth-api.lexisnexis.com/oauth/v2/token")
  
  
  auth.code <<- oauth2.0_token(endpoint = lexis,
                               app = oauth_app(appname = "lexis_api",
                                               key=key, secret=secret),
                               scope = "http://oauth.lexisnexis.com/all", 
                               client_credentials = T,
                               use_basic_auth = T,
                               user_params = lexis,
                               cache = F)
  
  
  
}

################## Lexis Extraction function
# searchdata = conflict_keywords dataset
# url = conflict_keywords$urls
# id = conflict_id 

lexis_extract <- function(searchdata, url, searchdata_id, result_id) {
  
  result_comb <- list()

      # Loop through URL's 
      for (j in unique(url)) {
        
        # Make get request
        request <- GET(j,
                       config(token = auth.code))
        print(j)
        
        # Make sure your request went through. If nothing comes up, it worked.
        # If it says 401, it didn't authorize. If it says 404, it didn't connect.
        stop_for_status(request)
        
        # Convert request to raw data (json)
        this.raw.content <- rawToChar(request$content)
        
        # Convert JSON data into list
        apiresponse <- jsonlite::fromJSON(this.raw.content, flatten = T, simplifyDataFrame = TRUE)
        
        # Obtain next link for pagination
        # Lexis API only does 10 responses a page
        next_url <- apiresponse$`@odata.nextLink`
        
        print(next_url)
        
        # Extract data from first 10 responses
        apidata <- apiresponse$value
        
        # Pagination API requests
        data <- list()
        i <- 1
        
        while(!is.null(next_url)) {
          x <- GET(apiresponse$`@odata.nextLink`, 
                   config(token = auth.code))
          this.raw.content <- rawToChar(x$content)
          apiresponse <- jsonlite::fromJSON(this.raw.content, flatten = T, simplifyDataFrame = TRUE)
          next_url <- apiresponse$`@odata.nextLink`
          print(next_url)
          temp <- apiresponse$value
          rownames(temp) <- NULL
          data[[i]] <- temp
          i <- i + 1
        }
        
        # Combine data from pagination 
        result_full <- dplyr::bind_rows(data, apidata, .id = "column_label")
        
        
        # Bind original API results
        #result_full <- dplyr::bind_rows(result, apidata, .id = "column_label")
        
        # Mark search to merge with original conflicts data
        # change this as needed
        result_full[result_id] <- searchdata_id[length(j)]
      
        
        # Merge original conflict data to match articles with conflict data
        result_full <- left_join(result_full, searchdata, by = result_id)
        
        
        # Save data in RDS format
        saveRDS(result_full, paste0("01 - Data/LexisAPI/", "conflict_", searchdata_id[length(j)], ".RDS"))
        
        result_comb <<- dplyr::bind_rows(result_comb, result_full, .id = "column_label")
        #result_comb <<- do.call(rbind, result_comb, result_full)
        #result_comb <<- rbind(result_comb, result_full)
        
        #Sys.sleep(2)
        #gc()
      }
  
  saveRDS(result_comb, "01 - Data/LexisAPI/LexisAPI_results_combined.RDS")
  
}

# need to fix extract loop. Not saving data as it goes, and only saves last api call loop. # might need to delete nested
# dataset column and save as csv. 
