# Chargement des packages
library(dplyr)
library(sf)
library(mapsf)
library(classInt)
library(leaflet)

dep21 <- st_read("fonds/dep_francemetro_2021.gpkg")
tx_pauv <- openxlsx::read.xlsx("data/Taux_pauvrete_2018.xlsx")

mer <- st_read("fonds/merf_2021.gpkg")

str(tx_pauv)
str(dep21)

dep_francemetro_2018_pauv <- dep21 %>% 
  left_join(tx_pauv %>% select(-Dept), by=c("code"="Code"))

mf_map(x = dep_francemetro_2018_pauv,
       var = "Tx_pauvrete",
       type = "choro",
       nbreaks = 4,
       breaks= "jenks"
)


couleur = rev(mf_get_pal(4, "Mint"))
mf_map(x = dep_francemetro_2018_pauv,
       var = "Tx_pauvrete",
       type = "choro",
       breaks= c(0,13,17,25, max(dep_francemetro_2018_pauv$Tx_pauvrete)),
       pal=couleur,
       leg_pos=NA
)

mf_inset_on(x=dep_francemetro_2018_pauv, pos="topright", cex=.2)

mf_init(dep_francemetro_2018_pauv %>% 
          filter(code %in% c("75", "92", "93", "94")))

mf_map(x = dep_francemetro_2018_pauv %>% 
         filter(code %in% c("75", "92", "93", "94")),
       var = "Tx_pauvrete",
       type = "choro",
       breaks= c(0,13,17,25, max(dep_francemetro_2018_pauv$Tx_pauvrete)),
       #pal=couleur,
       leg_pos=NA,
       add = TRUE
)

mf_label(dep_francemetro_2018_pauv %>% 
           filter(code %in% c("75", "92", "93", "94")),
         var = "code",
         col = "black")

mf_inset_off()

mf_legend(
  type = "choro",
  title = "taux de pauvreté",
  val = c("", "Moins de 13", "De 13 à moins de 17", "De 17 à moins de 25", "25 ou plus "),
  pal = couleur,
  pos = "left"
)

mf_map(mer, add=TRUE, col="yellow")

st_write(dep_francemetro_2018_pauv, file = "TP2/carte_tp2.gpkg")

