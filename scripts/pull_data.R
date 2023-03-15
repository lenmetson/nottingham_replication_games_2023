# You need to set an API key: 
# Go to Harvard Dataverse, create an account and log in. 
# Click on your name and select "API Token"
# Copy the token by pressing copy to clipboard
# Run the following line (after installing academictwitteR) to open your .Renviron file
usethis::edit_r_environ()
# Type DATAVERSE_KEY="your-key-goes-here"
# Paste DATAVERSE_SERVER = "dataverse.harvard.edu" below

#install.packages("dataverse")

get_full_rep_files <- function(data_doi,server_link){
  files <- dataverse::dataset_files(
              dataset = data_doi,
              server =  server_link)

filenames <- c()

for(file in files){
  filenames <- c(filenames, file$label)
}

for (i in 1:length(filenames)){
  dataverse::get_file_by_name(
              filename = filenames[i],
              dataset = data_doi,
              server = "server_link" 
              ) |>
           writeBin(con = here::here("original_materials", paste0(filenames[i])))
}

}

get_full_rep_files(data_doi = "doi:10.7910/DVN/AWSQTW&version=1.1",
                   server_link = "dataverse.harvard.edu")
