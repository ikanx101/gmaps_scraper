rm(list=ls())

# ambil libraries yang 
library(dplyr)
library(tidyr)

# path data
path = "E:/DATA SCIENCE/Google Maps Scraper/Cleaned Data"
files = list.files(path,full.names = T)

# load semua data yang mungkin
for(i in files){
  load(i)
}

# gabung ke data final
data_final = data_final %>% select(-marker)
data_final = rbind(data_final,data_final_ketinggalan)

# saatnya bebersih
data_final %>% 
  mutate(poi = case_when(
    grepl("mall|plaza|square",nama_tempat,ignore.case = T) ~ "pusat perbelanjaan",
    grepl("mobil|Mitsubishi|toyota|bmw|astra|bengkel|nissan|daihatsu|auto|motor|custom",nama_tempat,ignore.case = T) ~ "bengkel",
    grepl("sinema|Silverstone|Tridatu bali|bioskop|cinema|Nusantara Sejahtera Raya|cinep|xxi|cgv|teater|movie|theater|21",nama_tempat,ignore.case = T) ~ "bioskop",
    grepl("hotel|novotel",nama_tempat,ignore.case = T) ~ "hotel",
    grepl("unhas|udinus|pkbm|MI Hasyim|unila|amik|dikti|pgri|strada|tkit|unp |min |stikes|man |mis |ypi|BPK|stim|institut|UIN|sd|school|smk|stm|stih|smp|sma|politani|stmik|ikip|balai|akademi|stimi|stie|fakultas|asrama|ponpes|stak|iain|politeknik|sekolah|madrasah|Universitas|kampus|yayasan|perpustakaan",nama_tempat,ignore.case = T) ~ "sekolah/college",
    grepl("indomaret",nama_tempat,ignore.case = T) ~ "indomaret",
    grepl("alfamart",nama_tempat,ignore.case = T) ~ "alfamart",
    grepl("health|diadems|barito state|swimming|futsal|basket|tennis|badminton|volly|arena|lapangan|sanggar|zumba|senam|muay|studio|fit|sport|firness|bugar|mma|fitnes|gym|yoga|martial",nama_tempat,ignore.case = T) ~ "olahraga",
    grepl("src",nama_tempat,ignore.case = T) ~ "src",
    grepl("ta wan|lombok|ciganea|ong palace|Warong Wenang|citra palace|pondok|roll|hokben|soup|kuliner|cobek|sambal|sambel|kue|dapoer|gurih|pawon|rawon|baso|sego|raja kuring|ampera|uduk|eat|masakan|geprek|steak|takoyak|chicken|mcdona|pindang|laut|kitchen|santap|belut|food|gado|minang|kedai|waroenk|teler|richeese|bebek|warkop|sate|satay|pecel|sari laut|waroeng|fat |deli|hotplate|konro|ndut|pentol|dapur|rawon|cendol|lesehan|soto|coto|padang|burger|grill|nasi|kfc|ikan|nasgor|restau|depot|mie|crab|ayam|bfc|suki|resto|seafood|solaria|batagor|sushi|pempek|warung|makan|pizza|bakso|r.m|rm",nama_tempat,ignore.case = T) ~ "rumah makan",
    grepl("masjid|gereja",nama_tempat,ignore.case = T) ~ "rumah ibadah",
    grepl("kos",nama_tempat,ignore.case = T) ~ "kos",
    grepl("auditorium",nama_tempat,ignore.case = T) ~ "gedung pertemuan",
    grepl("coffe|kopi|cofe|cafe",nama_tempat,ignore.case = T) ~ "cafe",
    grepl("Photography|poto|foto|picture",nama_tempat,ignore.case = T) ~ "fotografi",
    grepl("jne",nama_tempat,ignore.case = T) ~ "logistik",
    grepl("halal|market",nama_tempat,ignore.case = T) ~ "retail",
    grepl("insurance",nama_tempat,ignore.case = T) ~ "insurance",
    grepl("Kaltimtoday|berita",nama_tempat,ignore.case = T) ~ "pers",
    grepl("klinik",nama_tempat,ignore.case = T) ~ "klinik",
    grepl("statistik|kabupaten|kota|mabes",nama_tempat,ignore.case = T) ~ "pemerintahan",
    grepl("fashion|seragam|daster|konveksi",nama_tempat,ignore.case = T) ~ "fashion",
    grepl("indomar|indo mar",nama_tempat,ignore.case = T) ~ "indomaret",
    grepl("alfamar|alfa ma",nama_tempat,ignore.case = T) ~ "alfamart",
    grepl("bsi|bpr|ocbc|cimb|atm|bank|bca|mandiri|bri|bni",nama_tempat,ignore.case = T) ~ "perbankan",
    grepl("kaset|music",nama_tempat,ignore.case = T) ~ "recording",
    grepl("benteng|museum|wisata|carita",nama_tempat,ignore.case = T) ~ "wisata",
    grepl("pasar",nama_tempat,ignore.case = T) ~ "pasar",
    grepl("emas",nama_tempat,ignore.case = T) ~ "toko emas",
    grepl("kaset",nama_tempat,ignore.case = T) ~ "recording"
  ) 
  ) %>% 
  filter(is.na(poi)) %>% 
  select(nama_tempat,kota) %>% 
  .[sample(50,20),]



sum(data_final$marker)

save(data_final,
     file = "~/Documents/Gmaps Scraper/Cleaned Data/ready.rda")
