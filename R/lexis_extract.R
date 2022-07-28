# Adi Tantravahi

################## Lexis Extraction function
lexis_extract <- function(searchdata, url, searchdata_id, result_id) {



    # Make get request
    request <- httr::GET(url,
                         httr::config(token = auth.code))
    print(url)

    # Make sure your request went through. If nothing comes up, it worked.
    # If it says 401, it didn't authorize. If it says 404, it didn't connect.
    httr::stop_for_status(request)

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
      x <- httr::GET(apiresponse$`@odata.nextLink`,
                     httr::config(token = auth.code))
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
    result_full[result_id] <- searchdata_id


    # Merge original conflict data to match articles with conflict data
    result_full <<- dplyr::left_join(result_full, searchdata, by = result_id)

    # garbage collection
    gc()




}
