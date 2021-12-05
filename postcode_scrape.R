library(RSelenium)
library(stringr)
library(tidyverse)
library(reshape2)
library(sf)
library(PostcodesioR)

##setup the selenium browser driver. This instance uses firefox and a random 4 digit number as the port
rsd <- rsDriver(browser = "firefox", port = 4833L)
remDr <- rsd$client
##define pattern to search through the text
pattern <- "\\b(?:([Gg][Ii][Rr] 0[Aa]{2})|((([A-Za-z][0-9]{1,2})|(([A-Za-z][A-Ha-hJ-Yj-y][0-9]{1,2})|(([A-Za-z][0-9][A-Za-z])|([A-Za-z][A-Ha-hJ-Yj-y][0-9]?[A-Za-z]))))\\s?[0-9][A-Za-z]{2}))\\b"

official_link <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/995458/covid-private-testing-providers-general-testing-220621.csv"

##import csv file saved
# <- read.csv("covid-private-testing-providers-general-testing-180621.csv")
covid <- read.csv(official_link)
##remove empty first row
covid <- covid[2:NROW(covid),]
##find unique websites
covids <- unique(covid$Website)
##create list to populate with loop
all_pcodes <- list()
for (c in covids){
  ##try catch error function to prevent loop dropping out
  tryCatch({
##navigate to the url  
remDr$navigate(c)
##extract the page source as raw text
pg <- remDr$getPageSource()
##search for the postcode
pcodes <- str_extract(pg, pattern)
##if strings have been found, write these to a dataframe and then to the list
if(NROW(pcodes)>0){
df <- data.frame(Website = c, pcodes)

all_pcodes[[c]] <- df
##print where the loop is up to
print(match(c, covids))
flush.console()

}
##try catch error function to prevent the loop finishing if there is an error
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
##bind list as a ddata frame
pc_df <- do.call(rbind, all_pcodes)
##save postcodes in case R crashes
saveRDS(pc_df, "postcodes_out.RDS")

##find all unique IDs in the valid postcodes
id_T <- unique(pc_df$Website)
m <- id_T[1]
##loop through each and pull out the data on each from the PostcodeIO package
all_mems <- list()
for (m in id_T){
  tryCatch({
  df <- filter(pc_df, Website == m)
  
  PC <- postcode_lookup(df$pcodes)
  df$latitude <- PC$latitude
  df$longitude <- PC$longitude

  all_mems[[m]] <- df
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

##bind all together
all_pc <- do.call(rbind, all_mems)
##define crs code for latitude and longitude
latlong = "+init=epsg:4326"
##filter out websites that have no postcode data and convert to sf objects with latlong crs
all_pc_sf <- all_pc %>% 
  filter(!is.na(latitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = latlong) %>% 
  left_join(covid, by = "Website")
## save data as RDS for map to impot
saveRDS(all_pc_sf, "map_dat.RDS")

