library(tidyverse)
path <- "C:\\Users\\Jonathan\\Hackathon\\DBData.csv"
path2 <- "C:\\Users\\Jonathan\\Hackathon\\BahnhofCords.csv"

raw <- read_delim(file = path, delim = ";")
Bahnhof <- read_delim(file = path2, delim = ";")

cleaned <- raw %>% 
  filter(link_gw_conn) %>%
  #arrange(sid, created) %>%
  mutate(uploadpp = tprx / pax_auth,
         downloadpp = tptx / pax_auth,
         time = created %>% as.character() %>% substr(1, 16))
  
aggregated <- cleaned %>%
  group_by(time, sid) %>%
  summarize(
    pingavg = mean(link_ping, na.rm = TRUE),
    uploadppavg = mean(uploadpp, na.rm = TRUE),
    downloadppavg = mean(downloadpp, na.rm = TRUE),
    gps_breiteavg = mean(gps_breite, na.rm = TRUE),
    gps_laengeavg = mean(gps_laenge, na.rm = TRUE),
  )

finde_bahnhof <- function(breite, laenge) {
  result <- Bahnhof %>%
    mutate(Lat_Breite = abs(Lat_Breite - breite),
           Lat_Breite = abs(Lat_Long - laenge),
           ) %>%
    filter(Lat_Breite <= 0.05 & Lat_Long <= 0.05)
  if(nrow(result) == 0) {
    return(NA)
  } else {
    return(result$Bahnhof)
  }
}    

full <- aggregated %>%
  rowwise() %>%
  mutate(Bahnhof = finde_bahnhof(breite = gps_breiteavg,
                                 laenge = gps_laengeavg)
  )




