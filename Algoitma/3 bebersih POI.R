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

data_final %>% 
  mutate(poi = case_when(
    grepl("mobil|Mitsubishi|toyota|bmw|astra|bengkel|nissan|daihatsu|auto|motor|custom",nama_tempat,ignore.case = T) ~ "bengkel",
    grepl("bioskop|cinema|Nusantara Sejahtera Raya|cinep|xxi|cgv|teater|movie|theater|21",nama_tempat,ignore.case = T) ~ "bioskop",
    grepl("hotel",nama_tempat,ignore.case = T) ~ "hotel",
    grepl("strada|tkit|unp |min |stikes|man |mis |ypi|BPK|stim|institut|UIN|sd|school|smk|stm|stih|smp|sma|politani|stmik|ikip|balai|akademi|stimi|stie|fakultas|asrama|ponpes|stak|iain|politeknik|sekolah|madrasah|Universitas|kampus|yayasan|perpustakaan",nama_tempat,ignore.case = T) ~ "sekolah/college",
    grepl("indomaret",nama_tempat,ignore.case = T) ~ "indomaret",
    grepl("alfamart",nama_tempat,ignore.case = T) ~ "alfamart",
    grepl("volly|arena|lapangan|sanggar|zumba|senam|muay|studio|fit|sport|firness|bugar|mma|fitnes|gym|yoga|martial",nama_tempat,ignore.case = T) ~ "olahraga",
    grepl("ocbc|cimb|atm|bank|bca|mandiri|bri|bni",nama_tempat,ignore.case = T) ~ "perbankan",
    grepl("src",nama_tempat,ignore.case = T) ~ "src",
    grepl("food|gado|minang|kedai|waroenk|teler|richeese|bebek|warkop|sate|satay|pecel|sari laut|waroeng|fat |deli|hotplate|konro|ndut|pentol|dapur|rawon|cendol|lesehan|soto|coto|padang|burger|grill|nasi|kfc|ikan|nasgor|restau|depot|mie|crab|ayam|bfc|suki|resto|seafood|solaria|batagor|sushi|pempek|warung|makan|pizza|bakso|r.m|rm",nama_tempat,ignore.case = T) ~ "rumah makan",
    grepl("masjid|gereja",nama_tempat,ignore.case = T) ~ "rumah ibadah",
    grepl("kos",nama_tempat,ignore.case = T) ~ "kos",
    grepl("auditorium",nama_tempat,ignore.case = T) ~ "gedung pertemuan",
    grepl("coffe|kopi|cofe|cafe",nama_tempat,ignore.case = T) ~ "cafe",
    grepl("Photography|poto|foto|picture",nama_tempat,ignore.case = T) ~ "fotografi",
    grepl("jne",nama_tempat,ignore.case = T) ~ "logistik",
    grepl("halal",nama_tempat,ignore.case = T) ~ "retail",
    grepl("insurance",nama_tempat,ignore.case = T) ~ "insurance",
    grepl("Kaltimtoday|berita",nama_tempat,ignore.case = T) ~ "pers",
    grepl("klinik",nama_tempat,ignore.case = T) ~ "klinik",
    grepl("statistik|kabupaten|kota",nama_tempat,ignore.case = T) ~ "pemerintahan",
    grepl("fashion|seragam",nama_tempat,ignore.case = T) ~ "fashion"
  ) 
           ) %>% 
  filter(is.na(poi)) %>% 
  select(nama_tempat) %>% 
  .[sample(100,20),]



sum(data_final$marker)

save(data_final,
     file = "~/Documents/Gmaps Scraper/Cleaned Data/ready.rda")
