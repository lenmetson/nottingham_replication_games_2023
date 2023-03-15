# You need to set an API key: 
# Go to Harvard Dataverse, create an account and log in. 
# Click on your name and select "API Token"
# Copy the token by pressing copy to clipboard
# Run the following line (after installing academictwitteR) to open your .Renviron file
usethis::edit_r_environ()
# Type DATAVERSE_KEY="your-key-goes-here"
# Paste DATAVERSE_SERVER = "dataverse.harvard.edu" below

url <- "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/AWSQTW&version=1.1"
id <- "doi:10.7910/DVN/AWSQTW&version=1.1"
install.packages("dataverse")

dataverse::dataverse_fetch(id, 
                           path = 
                                here::here(
                                    "original_materials"))


?dataverse::get_dataverse()



# Replace with the persistent identifier of the Dataverse folder
persistent_id <- "doi:10.7910/DVN/AWSQTW&version=1.1"

# Construct the URL of the Dataverse API endpoint
url <- "https://en.wikipedia.org/wiki/Silicon_Valley_Bank"

# Send a GET request to the API endpoint to obtain the download URL
response <- httr::GET(url)
download_url <- httr::content(response)$data$downloadUrl

# Download the file using the download URL
download_file <- httr::GET(download_url)

httr::content(download_file, type = "application/zip", as = "raw") |>
  writeBin(here::here("original_materials", "orginal_meterial.zip"))

Sys.getenv()


