rm(list=ls())

# libraries
library(dplyr)
library(tidyr)

# set path folder terlebih dahulu
path = "~/Documents/Gmaps Scraper/POI Cleaned"

# ambil semua nama files
files = list.files(path,full.names = T)

# save nama kota
kota = c("Bandar Lampung","Kediri","Makassar","Samarinda")

# kita mulai dulu
i = 1

# import data
load(files[i])

temp = do.call(rbind,hasil) %>% distinct()
temp$kota = kota[i]
temp %>% 
  separate(ratings,
           into = c("ratings","dummy"),
           sep = "\\·") %>% 
  select(-dummy) %>% 
  mutate(ratings = gsub("\\,",".",ratings),
         ratings = as.numeric(ratings))
