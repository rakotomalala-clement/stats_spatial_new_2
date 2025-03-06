library(dplyr)
library(sf)
library(mapsf)
library(classInt)
library(leaflet)
pop_com_2019 <- openxlsx::read.xlsx("data/Pop_legales_2019.xlsx")

pop_com_2019 <- pop_com_2019 %>% 
  mutate(COM=if_else(substr(COM,1,3)=="751", "75056", COM)) %>% 
  group_by(code=COM) %>% 
  summarise(pop=sum(PMUN19))

commune_fm <- st_read("fonds/commune_francemetro_2021.gpkg")

commune_fm <- commune_fm %>% 
  left_join(pop_com_2019,
            by="code") %>% 
  mutate(densite=pop/surf)

#2
summary(commune_fm$densite)
hist(commune_fm$densite)

#3
plot(commune_fm["densite"], border=FALSE)
plot(commune_fm["densite"], breaks='quantile', main="quantile", border=FALSE)
plot(commune_fm["densite"], breaks='sd', main="sd", border=FALSE)
plot(commune_fm["densite"], breaks='jenks', main="jenks", border=FALSE)
plot(commune_fm["densite"], breaks='pretty', main="pretty", border=FALSE)

#4
denspop_quant <- classIntervals(
  commune_fm$densite,
  style = "quantile",
  n=5
)
str(denspop_quant)

head(denspop_quant$var)
denspop_quant$brks

#5
quantile(commune_fm$densite, probs=seq(0,1,0.1))

denspop_man_brks5 <- c(0, 78, 250, 2000, 15000, 28000)

popcomfm_sf <- commune_fm %>% 
  mutate(
    densite_c = cut(densite,
                    breaks = denspop_man_brks5,
                    labels = paste0(denspop_man_brks5[1:5], "-", denspop_man_brks5[2:6]),
                    include.lowest = TRUE,
                    right = FALSE,
                    ordered_result = TRUE
    )
  )

pla2 <- c(
  RColorBrewer::brewer.pal(
    n=5,
    name="Greens"
  )[4:3],
  RColorBrewer::brewer.pal(
    n=5,
    name="YlOrRd"
  )[c(2,4:5)]
)

plot(popcomfm_sf["densite_c"], border=FALSE, pal=pla2)
