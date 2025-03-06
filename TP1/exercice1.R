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

plot(dept_aura)

dept_aura2 <- communes_AURA %>% 
  group_by(dep) %>% 
  summarise(geom = st_union(geom))

plot(dept_aura2)


#14

centroid_dept_aura <- st_centroid(dept_aura2)
plot(st_geometry(dept_aura2))
plot(st_geometry(centroid_dept_aura), add=TRUE)

departements_AURA <- c("Ain", "Allier", "Ardèche", "Cantal", "Drôme", 
                       "Isère", "Loire", "Haute-Loire", "Puy-de-Dôme", 
                       "Rhône", "Savoie", "Haute-Savoie")

data_departements_AURA <- data.frame(dept_lib = departements_AURA)

centroid_dept_aura <- cbind(centroid_dept_aura, data_departements_AURA)

centroid_cords <- st_coordinates(centroid_dept_aura)

centroid_cords <- centroid_cords %>% 
  bind_cols(
    centroid_dept_aura %>% 
      select(dep, dept_lib) %>% 
      st_drop_geometry()
  )

plot(st_geometry(dept_aura2))
plot(st_geometry(centroid_dept_aura), add=TRUE)
text(x=centroid_cords$X, y=centroid_cords$Y, labels=centroid_cords$dept_lib, pos = 3, cex = 0.8,
     col = "orange")

st_intersection(centroid_dept_aura, communes_AURA)
