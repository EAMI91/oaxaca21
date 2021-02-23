library(tidyverse)
library(sf)
library(leaflet)
library(renv)
library(here)
library(readxl)
library(magrittr)
library(glue)
library(mapedit)
library(mapview)
# Lectura de datos --------------------------------------------------------

data <- read_excel(here("data/municipios y distritos a encuestar.xlsx"))
setwd("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/SHP/2020/20 - Oaxaca")

muns <- st_read("MUNICIPIO.shp") %>% st_transform(4326)
secc <- st_read("SECCION.shp")%>% st_transform(4326)

ln <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/Lista Nominal/Nacional_Dic_2020/Lista Nominal Dic 2020.csv")
ln <- filter(ln,ENTIDAD == 20,DISTRITO != 0) %>% select(MUNICIPIO,SECCION,LISTA_NAL)
secc %<>% left_join(ln) 
# Prepración --------------------------------------------------------------

select_muns <- muns %>% semi_join(data, by = c("NOMBRE"="MUNICIPIO")) 

seccs <- secc %>% semi_join(select_muns%>% as_tibble() %>% select(MUNICIPIO,NOMBRE), by = "MUNICIPIO")%>% 
  left_join(select_muns%>% as_tibble() %>% select(MUNICIPIO,NOMBRE), by = "MUNICIPIO")
seccs %>% st_is_valid() %>% is.na %>% which

pal <- colorNumeric(topo.colors(n_distinct(seccs$MUNICIPIO)),domain = unique(seccs$MUNICIPIO))
leaflet(seccs %>% slice(-650)) %>% addPolygons(weight = 1, color = ~pal(MUNICIPIO)) %>% 
  addPolygons(data = select_muns, fillColor = "transparent", color = "black", opacity = 1, weight = 1)


# Seleccionar regiones ----------------------------------------------------

r1 <- selectFeatures(select_muns)

r2 <- selectFeatures(select_muns %>% anti_join(r1 %>% as_tibble %>% select(MUNICIPIO)))

r3 <- selectFeatures(select_muns %>% anti_join(r1 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r2 %>% as_tibble %>% select(MUNICIPIO))
                     )

r4 <- selectFeatures(select_muns %>% anti_join(r1 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r2 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r3 %>% as_tibble %>% select(MUNICIPIO))
                     )

r5 <- selectFeatures(select_muns %>% anti_join(r1 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r2 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r3 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r4 %>% as_tibble %>% select(MUNICIPIO))
)

r6 <- selectFeatures(select_muns %>% anti_join(r1 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r2 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r3 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r4 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r5 %>% as_tibble %>% select(MUNICIPIO))
)

r7 <- selectFeatures(select_muns %>% anti_join(r1 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r2 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r3 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r4 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r5 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r6 %>% as_tibble %>% select(MUNICIPIO))
)

r8 <- selectFeatures(select_muns %>% anti_join(r1 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r2 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r3 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r4 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r5 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r6 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r7 %>% as_tibble %>% select(MUNICIPIO))
)

r9 <- selectFeatures(select_muns %>% anti_join(r1 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r2 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r3 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r4 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r5 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r6 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r7 %>% as_tibble %>% select(MUNICIPIO)) %>% 
                       anti_join(r8 %>% as_tibble %>% select(MUNICIPIO))
)

# Crear carpetas ----------------------------------------------------------
paste0("R",1:9) %>% map(~dir.create(.x))

r1 %>% pull(NOMBRE) %>% map(~dir.create(glue("R1/{.x}")))
r2 %>% pull(NOMBRE) %>% map(~dir.create(glue("R2/{.x}")))
r3 %>% pull(NOMBRE) %>% map(~dir.create(glue("R3/{.x}")))
r4 %>% pull(NOMBRE) %>% map(~dir.create(glue("R4/{.x}")))
r5 %>% pull(NOMBRE) %>% map(~dir.create(glue("R5/{.x}")))
r6 %>% pull(NOMBRE) %>% map(~dir.create(glue("R6/{.x}")))
r7 %>% pull(NOMBRE) %>% map(~dir.create(glue("R7/{.x}")))
r8 %>% pull(NOMBRE) %>% map(~dir.create(glue("R8/{.x}")))
r9 %>% pull(NOMBRE) %>% map(~dir.create(glue("R9/{.x}")))

paste0("R",1:9) %>% map(~paste(.x,list.files(path = .x,pattern = "^[^.]+$"),sep = "/") %>% map(~{
  dir.create(glue("{.x}/csv"))
  dir.create(glue("{.x}/maps"))
  dir.create(glue("{.x}/png"))
  dir.create(glue("{.x}/shp"))
}))

# Guardar municipios por región -------------------------------------------

r1 %>% st_write("R1/r1.shp")
r2 %>% st_write("R2/r2.shp")
r3 %>% st_write("R3/r3.shp")
r4 %>% st_write("R4/r4.shp")
r5 %>% st_write("R5/r5.shp")
r6 %>% st_write("R6/r6.shp")
r7 %>% st_write("R7/r7.shp")
r8 %>% st_write("R8/r8.shp")
r9 %>% st_write("R9/r9.shp")
