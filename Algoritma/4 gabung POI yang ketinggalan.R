rm(list=ls())

library(dplyr)
library(tidyr)

load("E:/DATA SCIENCE/Google Maps Scraper/POI Ketinggalan/poi clean.rda")

# gabung dari list
df = do.call(rbind,hasil) %>% distinct()

# save nama kota
kota = c("Bandar Lampung","Kediri","Makassar","Samarinda")

# bebersih dulu
df =
  df %>% 
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
  mutate(lat = as.numeric(lat),
         lng = as.numeric(lng))

# kita masukin nama kota lalu kita buat marker ya
data_final_ketinggalan = 
  df %>% 
  rowwise() %>% 
  mutate(kota = case_when(
    grepl(kota[1],alamat,ignore.case = T) ~ kota[1],
    grepl(kota[2],alamat,ignore.case = T) ~ kota[2],
    grepl(kota[3],alamat,ignore.case = T) ~ kota[3],
    grepl(kota[4],alamat,ignore.case = T) ~ kota[4]
  )
         ) %>% 
  ungroup() %>% 
  filter(!is.na(kota))

# save files
save(data_final_ketinggalan,
     file = "E:/DATA SCIENCE/Google Maps Scraper/Cleaned Data/POI ketinggalan.rda")
