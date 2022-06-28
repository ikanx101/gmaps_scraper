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


# ini adalah function untuk scrape info dari google maps
scrape_google_maps = function(url){
  remote_driver$navigate(url)
  Sys.sleep(7)
  output = 
    remote_driver$getPageSource()[[1]] %>% 
    read_html() %>% {
      tibble(
        nama_tempat = html_nodes(.,".fontHeadlineLarge") %>% html_text() %>% .[1],
        ratings = html_nodes(.,".dmRWX") %>% html_text(),
        alamat = html_nodes(.,".AG25L:nth-child(3) .fontBodyMedium") %>% html_text()
      )
    }
  output$url = url
  return(output)
}

load("makassar.rda")

# template data
hasil = vector("list",length(temp_link))
# proses scraping
for(i in 1:length(temp_link)){
  hasil[[i]] = scrape_google_maps(temp_link[i])
  print(i)
}

save(hasil,temp_link,file = "makassar.rda")
