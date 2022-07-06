rm(list=ls())

# libraries
library(dplyr)
library(tidyr)

# set path folder terlebih dahulu
path = "E:/DATA SCIENCE/Google Maps Scraper/POI Cleaned"

# ambil semua nama files
files = list.files(path,full.names = T)

# save nama kota
kota = c("Bandar Lampung","Kediri","Makassar","Samarinda")

# bikin template dulu
data_final = data.frame()

# kita mulai dulu
for(i in 1:4){
  # import data
  load(files[i])
  # gabung data dari list
  temp = do.call(rbind,hasil) %>% distinct()
  # masukin kota
  temp$kota = kota[i]
  # bebersih dulu
  temp =
    temp %>% 
    separate(ratings,
             into = c("ratings","dummy"),
             sep = "\\·") %>% 
    select(-dummy) %>% 
    mutate(ratings = gsub("\\,",".",ratings),
           ratings = as.numeric(ratings)) %>% 
    separate(url,
             into = c("dummy","saved"),
             sep = "!3d") %>% 
    separate(saved,
             into = c("lat","lng"),
             sep = "!4d") %>% 
    separate(lng,
             into = c("lng","dummy"),
             sep = "\\!") %>% 
    select(-dummy) %>% 
    relocate(lng,.before = kota) %>% 
    mutate(lat = as.numeric(lat),
           lng = as.numeric(lng))
  # gabung lagi ke semua
  data_final = rbind(temp,data_final)
}

# kita hanya akan ambil POI dari kota kabupaten tersebut
data_final = 
  data_final %>% 
  rowwise() %>% 
  mutate(marker = ifelse(grepl(kota,alamat,ignore.case = T),
                         1,
                         0)) %>% 
  ungroup() %>% 
  filter(marker == 1)

save(data_final,
     file = "E:/DATA SCIENCE/Google Maps Scraper/Cleaned Data/POI utama.rda")


# ================================================================================

