# You need to set an API key: 
# Go to Harvard Dataverse, create an account and log in. 
# Click on your name and select "API Token"
# Copy the token by pressing copy to clipboard
# Run the following line (after installing academictwitteR) to open your .Renviron file
usethis::edit_r_environ()
# Type DATAVERSE_KEY="your-key-goes-here"
# Paste DATAVERSE_SERVER = "dataverse.harvard.edu" below

#install.packages("dataverse")

filename <- "brazil-RD-analysis.R"

dataverse::get_file_by_name(
              filename = filename,
              dataset = "doi:10.7910/DVN/AWSQTW&version=1.1",
              server = "dataverse.harvard.edu" 
              ) |>
           writeBin(con = here::here("original_materials", paste0(filename)))
  