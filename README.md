# rln
rln: R Package for Lexis Nexis Web Services API


## Installation
```r
devtools::install_github(amtantravahi/rln)
```

## API Authorization
You can authorize access into the Lexis API using your Lexis provided key and secret
and the `lexis_auth` function:
```r
rln::lexis_auth(key = key,
                secret = secret)
```
If nothing shows up in the console after running `lexis_auth` function, you should 
be good to go.


## Extract text
You can scrape text with the function `lexis_extract` function: 
```r
rln::lexis_extract(searchdata = df,
                  url = df$lexis_urls,
                  searchdata_id = df$id,
                  result_id = "id",
                  sleep = 40)
```

 

I built the package based on the project I was working on. I needed a way to way to keep track of the specific searches I used, so I created a data frame where each row had columns with the search terms, an id, and a column for the actual URL (happy to help with that as well if you haven't gotten that far yet!). That's why there are searchdata_id and result_id options. 


You'll also note a sleep option in there. This is because Lexis has a throttle limit of 2500 calls in a single day, with a max of 50 articles per call (125k articles/day), meaning to stay within that, you can only make a single call about every 40 seconds. If you aren't planning to hit this throttle limit, you can reduce the sleep option, but I would be weary of going too low as it's pretty easy to hit the limit, and it will make you wait to re-authorize.


## Transform text

The `lexis_extract` function will produce an output data frame named result_full. This is the raw API output, and its contents are essentially useless at this stage. You'll finally want to either use the `lexis_transform` or `lexis_transform_batchnews` functions depending on the type of API calls you send through. It will give you an output data frame named final_result. Here is an example: 
```r
rln::lexis_transform_batchnews(data = result_full$Document.Content,
                               orig_data = result_full)
```

Columns in the output that end in '_ln' indicate original lexis outputs that I preserved to maintain data integrity. The package also has some functions for processing whatever text column you want into a text corpus, but it's only English specific right now. Another thing to note is that the package marks duplicate titles, but this isn't perfect, as slight differences in the string will cause it not to detect a duplicate. I plan to improve this eventually.


The transformation functions are mainly for news at this stage, but in the future,
I hope to add functions for the other Lexis features like legal and corporate text.
 
