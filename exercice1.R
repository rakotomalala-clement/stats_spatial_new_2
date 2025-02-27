library("sf")
library(dplyr)

commune_france <- st_read("fonds/commune_francemetro_2021.gpkg")

summary(commune_france)
View(commune_france[1:10,])


st_crs(commune_france)

communes_AURA <- commune_france %>% 
  filter(reg==84) %>% 
  select(code, libelle, epc, dep, surf)
# il y a tjrs geom

str(communes_AURA)

plot(communes_AURA, lwd=0.3)

plot(st_geometry(communes_AURA), lwd=0.3)

communes_AURA$surf2 <- st_area(communes_AURA$geom)
communes_AURA$surf2 <- units::set_units(communes_AURA$surf2, km^2)

# 

dept_aura <- communes_AURA %>% 
  group_by(dep) %>% 
  summarise(surf_dep = sum(surf2))
