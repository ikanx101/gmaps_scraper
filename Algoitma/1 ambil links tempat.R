setwd("~/Documents/Gmaps Scraper")

library(dplyr)
library(rvest)
library(RSelenium)

rm(list=ls())

url = "https://www.google.com/maps/search/"

# memulai selenium
driver =  RSelenium::rsDriver(browser = "chrome",
                              chromever = "103.0.5060.53" )
remote_driver = driver[["client"]] 

# membuka situs google maps
remote_driver$navigate(url)

# bikin function ambil links
ambil_link = function(){
  urls = 
    remote_driver$getPageSource()[[1]] %>% 
    read_html() %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  # siapin url
  links = urls[grepl("maps/place",urls)]
  return(links)
}

# bikin template
temp_link = c()

# scraping
for(i in 1:50){
  print(i)
  print("scraping")
  temp = ambil_link()
  temp_link = c(temp,temp_link)
  Sys.sleep(3)
  cat("\014")
  print("klik next")
  Sys.sleep(4)
  print("scroll yuk")
  Sys.sleep(4)
}

# restoran makan 
# sekolah kampus 
# warung 
# atm bank gym
# bioskop
# bengkel

temp_link = temp_link %>% unique()

save(temp_link,file = "makassar.rda")
