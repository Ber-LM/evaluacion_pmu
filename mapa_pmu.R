##########################################################
## Evaluación del Programa de Mejoramiento Urbano (PMU) ##

## Proyecto de Carlos Buburrón y Bernardo LM
## Código por Bernardo LM
## Redacción por Carlos Buburrón (documento)

## Esquema del script
## 1. Limpieza y preparación de datos
## 2. Elaboración de mapas
## 3. Análisis de datos general y estadística descriptiva
## 4. Análisis de datos inferencial

## Disclaimer: los comentarios pueden ser indicativos, estructurales o personales (je)

rm(list=ls())

library (stringr)
library (dplyr)
library (Hmisc)
library (tidyverse)
library (ggplot2)
library (scales)
library (ggrepel)
library (ggthemes)
library (mosaic)
library (readxl)
library (stringi)
library (RColorBrewer)
library (viridis)
library (broom)
library (rgdal)
library (texreg)
library(rgeos)
library(RJSONIO)
library(sp)
library(sf)
library(rgdal)
library("mxmaps")

setwd("C:/Users/Admin/Documents/Inputs")
options(scipen=999)

## 1. Limpieza y preparación de datos
#####
## 1. Limpieza y preparación de datos

## cargamos los datos
pmu <- read.csv("pmu_limpio.csv") # Viene de la solicitud de info a SEDATU
                                  # con folio: 0001500098621
glimpse(pmu)

## Exploramos la base de datos de los municipios de mxmaps
 ## Tienen información del Censo de Población y Vivienda 2020 del INEGI
data("df_mxmunicipio_2020")

## Limpiamos la base
df_mxmunicipio_2020$state_name_official <- toupper(stri_trans_general(
  df_mxmunicipio_2020$state_name_official,
  "Latin-ASCII"))

df_mxmunicipio_2020$municipio_name <- toupper(stri_trans_general(
  df_mxmunicipio_2020$municipio_name,
  "Latin-ASCII"))

municipios <- df_mxmunicipio_2020 %>% 
  select(region, state_name_official, municipio_name, pop, indigenous_language) %>% 
  rename(estado = state_name_official,
         municipio = municipio_name) %>% 
  mutate(hablaind = (indigenous_language/pop)*100)

## Hay diferencias en el nombre de los municipios de Coahuila, Veracruz, Michoacán Y Ciudad de México
## Correcciones

municipios$estado[municipios$estado == "VERACRUZ DE IGNACIO DE LA LLAVE"] <- "VERACRUZ"

municipios$estado[municipios$estado == "MICHOACAN DE OCAMPO"] <- "MICHOACAN"

municipios$estado[municipios$estado == "COAHUILA DE ZARAGOZA"] <- "COAHUILA"

municipios$estado[municipios$estado == "DISTRITO FEDERAL"] <- "CIUDAD DE MEXICO"

## Ya tenemos homologados los municipios
filter(municipios, estado == "MEXICO") # Están bien los nombres

## Limpiamos la base de pmu
pmu$estado <- toupper(stri_trans_general(
  pmu$estado,
  "Latin-ASCII"))

pmu$municipio <- toupper(stri_trans_general(
  pmu$municipio,
  "Latin-ASCII"))

filter(municipios, municipio == "JALPA DE MENDEZ")
filter(pmu, municipio == "JALPA DE MENDEZ  ") # ¡¿Quién $#%&*+ hizo este Excel?!

pmu$municipio <- str_trim(pmu$municipio, side = "right")
pmu$estado <- str_trim(pmu$estado, side = "right")

pmu$municipio[pmu$municipio == "SAN ANDRES DE CHOLULA"] <- "SAN ANDRES CHOLULA"
pmu$municipio[pmu$municipio == "CD OBREGON"] <- "CAJEME"
pmu$municipio[pmu$municipio == "CIUDAD OBREGON"] <- "CAJEME"
pmu$municipio[pmu$municipio == "HUEHUETLAN"] <- "HUEHUETLAN EL CHICO"
pmu$municipio[pmu$municipio == "AYOXUXTLA"] <- "HUEHUETLAN EL CHICO"
pmu$municipio[pmu$municipio == "VILLAHERMOSA"] <- "CENTRO"
pmu$municipio[pmu$municipio == "AYOXUXTLA"] <- "HUEHUETLAN EL CHICO"
pmu$municipio[pmu$municipio == "SALINAS CRUZ"] <- "SALINA CRUZ"
pmu$municipio[pmu$municipio == "CANCUN"] <- "BENITO JUAREZ"

filter(pmu, municipio == "PUERTO MORELOS")
filter(municipios, municipio == "SALINA CRUZ")
filter(municipios, estado == "QUINTANA ROO")

## Descargamos datos de marginación CONAPO
margin_raw <- read_excel("IMM_2020_limpio.xls")

# margin_raw$NOM_MUN <- toupper(stri_trans_general(
#   margin$NOM_MUN,
#   "Latin-ASCII"))

margin <- margin_raw %>% 
  select(-NOM_MUN) %>% 
  rename(region = CVE_MUN)

margin_sh <- margin_raw %>% 
  select(-NOM_MUN) %>% 
  rename(id = CVE_MUN)

margin_sh$GM_2020 <- factor(margin_sh$GM_2020, levels=c("Muy bajo",
                                        "Bajo",
                                        "Medio",
                                        "Alto",
                                        "Muy alto"))

margin$GM_2020 <- factor(margin$GM_2020, levels=c("Muy bajo",
                                                        "Bajo",
                                                        "Medio",
                                                        "Alto",
                                                        "Muy alto"))

## Descargamos datos de marginación CONEVAL
margin_CONEVAL_raw <- read.csv("IRS_entidades_mpios_2020.csv")

margin_CONEVAL_raw$entidad <- toupper(stri_trans_general(
  margin_CONEVAL_raw$entidad,
  "Latin-ASCII"))

margin_CONEVAL_raw$municipio <- toupper(stri_trans_general(
  margin_CONEVAL_raw$municipio,
  "Latin-ASCII"))

margin_CONEVAL_raw$entidad[margin_CONEVAL_raw$entidad == "VERACRUZ DE IGNACIO DE LA LLAVE"] <- "VERACRUZ"

margin_CONEVAL_raw$entidad[margin_CONEVAL_raw$entidad == "MICHOACAN DE OCAMPO"] <- "MICHOACAN"

margin_CONEVAL_raw$entidad[margin_CONEVAL_raw$entidad == "COAHUILA DE ZARAGOZA"] <- "COAHUILA"

margin_CONEVAL_raw$entidad[margin_CONEVAL_raw$entidad == "DISTRITO FEDERAL"] <- "CIUDAD DE MEXICO"

margin_CONEVAL_raw$grado_rez_soc <- factor(margin_CONEVAL_raw$grado_rez_soc,
                                           levels=c("Muy bajo",
                                                        "Bajo",
                                                        "Medio",
                                                        "Alto",
                                                        "Muy alto"))
margin_CONEVAL <- margin_CONEVAL_raw %>% 
  rename(estado = entidad) %>% 
  inner_join(municipios, by = c("estado", "municipio")) %>% 
  rename(id = region) %>% 
  select(-pop, -indigenous_language, -hablaind, - CVE_MUN)

## Descargamos los datos de población afromexicana
afrom_raw <- read.csv("inegi_2020_ind_afro.csv")

afrom_raw$NOM_ENT <- toupper(stri_trans_general(
  afrom_raw$NOM_ENT,
  "Latin-ASCII"))

afrom_raw$NOM_MUN <- toupper(stri_trans_general(
  afrom_raw$NOM_MUN,
  "Latin-ASCII"))

afrom_raw$NOM_ENT[afrom_raw$NOM_ENT == "VERACRUZ DE IGNACIO DE LA LLAVE"] <- "VERACRUZ"

afrom_raw$NOM_ENT[afrom_raw$NOM_ENT == "MICHOACAN DE OCAMPO"] <- "MICHOACAN"

afrom_raw$NOM_ENT[afrom_raw$NOM_ENT == "COAHUILA DE ZARAGOZA"] <- "COAHUILA"

afrom_raw$NOM_ENT[afrom_raw$NOM_ENT == "DISTRITO FEDERAL"] <- "CIUDAD DE MEXICO"

afrom_raw$POB_AFRO <- as.numeric(afrom_raw$POB_AFRO)
afrom_raw$P3YM_HLI <- as.numeric(afrom_raw$P3YM_HLI)
filter(afrom_raw, POB_AFRO == "*") # los asteriscos son de localidades de menos
                                   # de tres viviendas; cuestión de privacidad
afrom_raw[is.na(afrom_raw)] = 0
summary(afrom_raw)

afrom <- afrom_raw %>% 
  mutate(if_municipio = ifelse(LOC == 1, 1, 0)) %>%
  filter(if_municipio == 1) %>% 
  rename(estado = NOM_ENT,
         municipio = NOM_MUN) %>% 
  select(estado, municipio, POBTOT, P3YM_HLI, POB_AFRO) %>% 
  group_by(estado, municipio) %>% 
  summarise(POBTOT, P3YM_HLI, POB_AFRO) %>%
  mutate(porcen_afro = (POB_AFRO/POBTOT)*100) %>% 
  inner_join(municipios, by = c("estado", "municipio")) %>% 
  select(-pop, -indigenous_language, -hablaind) %>% 
  rename(id = region) %>% 
  data.frame(.)

## Hacemos la base final limpia de pmu
homol <- pmu %>% 
  full_join(municipios, c("estado", "municipio")) %>%
  full_join(margin, by="region") %>% 
  drop_na(., num)

table(homol$GM_2020)
filter(homol, GM_2020 == "Alto")
summary(homol)

# Súper! Sólo que ahora me doy cuenta que mxmaps no hace el mapa que quiero...
# Quiero hacer uno de puntos, voy a tener que ponerle sus coordenadas a cada
# uno de los municipios. Ta' güeno, así aprendo de una vez a hacerlo

# Saqué la siguiente info de aquí: https://www.inegi.org.mx/app/ageeml/#
coord <- read.csv("C:/Users/Admin/Documents/Inputs/AGEEML_20211022230495.csv")
glimpse(coord)

coord$NOM_ENT <- toupper(stri_trans_general(
  coord$NOM_ENT,
  "Latin-ASCII"))

coord$NOM_MUN <- toupper(stri_trans_general(
  coord$NOM_MUN,
  "Latin-ASCII"))

coord$NOM_LOC <- toupper(stri_trans_general(
  coord$NOM_LOC,
  "Latin-ASCII"))

coord_cl <- coord %>% 
  select(CVE_ENT, CVE_MUN, CVE_LOC, NOM_ENT, NOM_MUN, NOM_LOC, LAT_DECIMAL, LONGITUD) %>%
  rename(LON_DECIMAL = LONGITUD) %>% 
  mutate(if_municipio = ifelse(CVE_LOC == 1, 1, 0)) %>% # WOW, lo logré
  filter(if_municipio == 1) %>% 
  select(-NOM_LOC, -if_municipio, -CVE_LOC, -CVE_MUN, -CVE_ENT) %>% 
  rename(estado = NOM_ENT, municipio = NOM_MUN)

homol_02 <- homol %>% 
  full_join(coord_cl, c("estado", "municipio")) %>% 
  drop_na(., num)

final <- homol_02 %>% 
  select(estado, municipio, avance, LAT_DECIMAL, LON_DECIMAL, region, nombre) %>% 
  group_by(estado, municipio, LAT_DECIMAL, LON_DECIMAL) %>% 
  summarise(frec = n(),
            prom_avance = mean(avance))

final <- data.frame(final)

## 2. Elaboración de mapas
#####
## 2. Elaboración de mapas

## Comenzamos el mapeo, quiero hacerlo de volumen de puntos
## ¡YA QUEDÓ! (me hago el estresado pero me encanta)

## Aquí empiezan los mapas!!! (con GADM)

# El mapa salió de aquí: https://gadm.org/download_country.html
setwd("C:/Users/Admin/Documents/Inputs")
mex <- readRDS("gadm36_MEX_0_sp.rds") # O.O
mex_mun <- readRDS("gadm36_MEX_2_sp.rds")

ggplot() + 
  geom_polygon(data=mex, aes(long, lat, group=group), fill="whitesmoke")+
  geom_path(data=mex, aes(long, lat, group=group), color="black",
            size=0.2) +
  geom_point(data=final,
             aes(x=LAT_DECIMAL, y=LON_DECIMAL, size = frec),
             colour="red", fill = "pink", pch=21,
             alpha=I(0.5)) + scale_size(range = c(1,10))

## Aquí empiezan los mapas con shapes!!
# Shapes de municipios
setwd("C:/Users/Admin/Documents/Inputs/mg_2020_integrado")
capa_municipios <- readOGR("conjunto_de_datos", layer="00mun")
capa_municipios@proj4string

capa_municipios_2 <- spTransform(capa_municipios, CRS("+proj=longlat +datum=WGS84"))
capa_municipios_2@proj4string

# ggplot() +  
#   geom_polygon(data=capa_municipios_2, aes(x=long, y=lat, group=group), 
#                fill="white", color="black")

capa_municipios_df <- tidy(capa_municipios_2, region="CVEGEO")

## Mapa PMU y Marginación CONAPO
capa_municipios_df_margin <- inner_join(capa_municipios_df, margin_sh, by="id")

ggplot(capa_municipios_df_margin) +  
  geom_polygon(aes(x=long, y=lat, 
                   group=group,       
                   fill=GM_2020)) +
  scale_fill_manual(values = c("#fcd74e",
                               "#ffb14e",
                               "#fa8775",
                               "#d17393",
                               "#bb4da7")) +
  geom_point(data=final,
             aes(x=LAT_DECIMAL, y=LON_DECIMAL, size = frec),
             colour="red", fill = "pink", pch=21,
             alpha=I(0.75)) + scale_size(range = c(1,20))+
  labs(fill = "Grado de
Marginación", 
       size = "No. proyectos",
       title = "Mapa de calor de municipios y su correspondiente grado de
marginación social (CONAPO, 2020) y la concentración municipal
de proyectos del Programa de Mejoramiento Urbano de la SEDATU",
       subtitle = "Corte: 27 de septiembre 2021",
       caption = "Elaboración por Bernardo L. Mc Kelligan y Carlos Buburrón
Código en: https://github.com/Ber-LM/evaluacion_pmu")+
  theme_bw()+
  theme(plot.caption.position = "panel")

## Mapa PMU y Rezago Social CONEVAL
capa_municipios_df_rez <- inner_join(capa_municipios_df, margin_CONEVAL, by="id")

ggplot(capa_municipios_df_rez) +  
  geom_polygon(aes(x=long, y=lat, 
                   group=group,       
                   fill=grado_rez_soc)) +
  scale_fill_manual(values = c("#fcd74e",
                               "#ffb14e",
                               "#fa8775",
                               "#d17393",
                               "#bb4da7")) +
  geom_point(data=final,
             aes(x=LAT_DECIMAL, y=LON_DECIMAL, size = frec),
             colour="red", fill = "pink", pch=21,
             alpha=I(0.75)) + scale_size(range = c(1,20))

## Mapa población indígena
mun_hablaind <- municipios %>% 
  rename(id = region) %>% 
  select(-pop)
capa_municipios_df_hablaind <- inner_join(capa_municipios_df, mun_hablaind, by="id")

ggplot(capa_municipios_df_hablaind) +  
  geom_polygon(aes(x=long, y=lat, 
                   group=group,       
                   fill=hablaind)) +
  geom_point(data=final,
             aes(x=LAT_DECIMAL, y=LON_DECIMAL, size = frec),
             colour="red", fill = "pink", pch=21,
             alpha=I(0.75)) + scale_size(range = c(1,20))


## Mapa población afromexicana
capa_municipios_df_afrom <- inner_join(capa_municipios_df, afrom, by="id")

ggplot(capa_municipios_df_afrom) +  
  geom_polygon(aes(x=long, y=lat, 
                   group=group,       
                   fill=porcen_afro)) +
  geom_point(data=final,
             aes(x=LAT_DECIMAL, y=LON_DECIMAL, size = frec),
             colour="red", fill = "pink", pch=21,
             alpha=I(0.75)) + scale_size(range = c(1,20))


## Mapa Tren Maya | son tres capas!!
sureste <- municipios %>% 
  filter(estado == "TABASCO" |
           estado == "CHIAPAS" |
           estado == "YUCATAN" |
           estado == "CAMPECHE" |
           estado == "QUINTANA ROO" ) %>% 
  rename(id = region)

final_sureste <- final %>% 
  filter(estado == "TABASCO" |
           estado == "CHIAPAS" |
           estado == "YUCATAN" |
           estado == "CAMPECHE" |
           estado == "QUINTANA ROO" )
  
capa_municipios_df_sur <- inner_join(capa_municipios_df, sureste, by="id")

setwd("C:/Users/Admin/Documents/Inputs")
tren_maya <- readOGR("TrenMAya3", layer = "TrenMAya3")
tren_maya@proj4string # Súper! ya viene limpio
                      # Olvídalo... hay una línea extra

tren_maya_1 <- tren_maya
tren_maya_2 <- tren_maya
tren_maya_3 <- tren_maya

tren_maya_1@lines[[3]] <- NULL
tren_maya_1@lines[[2]] <- NULL

tren_maya_2@lines[[1]] <- NULL
tren_maya_2@lines[[3]] <- NULL

tren_maya_3@lines[[1]] <- NULL
tren_maya_3@lines[[2]] <- NULL

capa_tren_maya_df <- tidy(tren_maya, region="lines")
capa_tren_maya_1_df <- tidy(tren_maya_1, region="lines")
capa_tren_maya_2_df_raw <- tidy(tren_maya_2, region="lines")
capa_tren_maya_3_df <- tidy(tren_maya_3, region="lines")

capa_tren_maya_2_df <- capa_tren_maya_2_df_raw %>%
  filter(long <= -89.9)

ggplot() +  
  geom_polygon(data = capa_municipios_df_sur, aes(x=long, y=lat, 
                    group=group), fill="white", color="grey50")+
  geom_path(data = capa_tren_maya_1_df, aes(x=long, y=lat, group = F),
       alpha = 1, color = "red", size = 1.15)+
  geom_path(data = capa_tren_maya_2_df, aes(x=long, y=lat, group = F),
       alpha = 1, color = "red", size = 1.15)+
  geom_path(data = capa_tren_maya_3_df, aes(x=long, y=lat, group = F),
               alpha = 1, color = "red", size = 1.15) +## Tres horas aquí...
  geom_point(data=final_sureste,
             aes(x=LAT_DECIMAL, y=LON_DECIMAL, size = frec),
             colour="red", fill = "pink", pch=21,
             alpha=I(0.5)) + scale_size(range = c(1,10)) +
  labs(size = "No. proyectos",
       title = "Representación geográfica del Tren Maya y la concentración municipal
de proyectos del Programa de Mejoramiento Urbano de la SEDATU",
       subtitle = "Corte: 27 de septiembre 2021",
       caption = "Elaboración por Bernardo L. Mc Kelligan y Carlos Buburrón
Código en: https://github.com/Ber-LM/evaluacion_pmu")+
  theme_bw()+
  theme(plot.caption.position = "panel")
## Aquí acaban los mapas!!

##### 3. Análisis de datos general y estadística descriptiva
##### 
##### 3. Análisis de datos general y estadística descriptiva

## Inicia análisis estadístico descriptivo
## Tablas comparativas CONAPO / CONEVAL
pmu_CONEVAL_mun <- final %>% 
  inner_join(margin_CONEVAL, by = c("estado", "municipio"))

table(pmu_CONEVAL_mun$grado_rez_soc)

pmu_CONAPO_mun <- final %>% 
  inner_join(municipios, by = c("estado", "municipio")) %>% 
  rename(id = region) %>% 
  select(-pop, -indigenous_language, - hablaind) %>% 
  inner_join(margin_sh, by = "id")

table(pmu_CONAPO_mun$GM_2020)

## Análisis población indígena superior a la media nacional
## y PMU por municipio
setwd("C:/Users/Admin/Documents/Inputs/")

pmu_indig_mun <- final %>% 
  inner_join(municipios, by = c("estado", "municipio"))

ggplot(pmu_indig_mun, aes(hablaind))+
  geom_histogram(fill = "pink",
                 col = "royalblue",
                 bins = 30) +
  labs(x = "Población de habla indígena (%)",
       y = "Cantidad de municipios del PMU\n",
       title = "Distribución de los municipios con personas de habla indígena
donde hay proyectos del Programa de Mejoramiento Urbano (PMU) (2020)")+
  theme_bw()

filter(pmu_indig_mun, hablaind > 6.5) # Promedio INEGI
16/58 #27.59 % de los proyectos están en municipios por encima del promedio nac

filter(municipios, hablaind > 6.5)
878/2469 #35.56 % de los municipios en México están por encima del promedio nac

## Análisis población indígena y PMU por proyecto ## 45.7%
pmu_indig_proy <- pmu %>% 
  inner_join(municipios, by = c("estado", "municipio"))

ggplot(pmu_indig_proy, aes(hablaind))+
  geom_histogram(fill = "pink",
                 col = "royalblue",
                 bins = 30) +
  labs(x = "Población de habla indígena (%)",
       y = "Cantidad de municipios del PMU\n",
       title = "Distribución de los proyectos en municipios con personas
de habla indígena del Programa de Mejoramiento Urbano (PMU) (2020)")+
  theme_bw()

count(filter(pmu_indig_proy, hablaind > 5))
134/293

filter(pmu_indig_mun, municipio == "GUAYMAS")

## Análisis porcentaje de avance ## 55.72%
final %>% 
  summarise(avance_total = mean(prom_avance))

## Análisis PMU y marginación municipios ## 24.13%
pmu_margin_mun <- municipios %>% 
  inner_join(margin, by = "region") %>% 
  inner_join(final, by = c("estado", "municipio"))

ggplot(pmu_margin_mun, aes(GM_2020))+
  geom_bar(fill = "pink",
           col = "royalblue") +
  labs(x = "Índice de marginación (2020)",
       y = "Cantidad de municipios del PMU\n",
       title = "Distribución de los municipios por índice de marginación
donde hay proyectos del Programa de Mejoramiento Urbano (PMU) (2020)")+
  theme_bw()

## Ejercicios comparativos
## Municipios

filter(pmu_margin_mun, GM_2020 == "Alto" | GM_2020 == "Medio")
14/58 # 24.14 % de los muncipios del PMU tienen al menos la cat "medio" de margin

filter(margin_sh, GM_2020 == "Alto" | GM_2020 == "Medio" |
         GM_2020 == "Muy alto")
1284/2469 # 52.00 % de los municipios en México tienen al menos la cat "medio" de margin

pmu_rez_soc_mun <- final %>% 
  inner_join(margin_CONEVAL, by = c("estado", "municipio"))

filter(pmu_rez_soc_mun, grado_rez_soc == "Alto" |
         grado_rez_soc == "Medio" | 
         grado_rez_soc == "Muy alto")
3/58 # 5.17 % de los municipios en el PMU son al menos la cat "medio" de rezago

filter(margin_CONEVAL, grado_rez_soc == "Alto" |
         grado_rez_soc == "Medio" | 
         grado_rez_soc == "Muy alto")
899/2469 # 36.41 % de los municipios en México son al menos la cat "medio" de rez

## Análisis comparativo población afromexicana
pmu_afrom_mun <- final %>% 
  inner_join(afrom, by = c("estado", "municipio"))

filter(pmu_afrom_mun, porcen_afro > 2.04)
18/58 # 31.03 % de los municipios en el PMU están por encima del promedio nacio

filter(afrom, porcen_afro > 2.04)
(142+471)/2469 # 24.83 % de los municipios en México están encima del prom nacio

## Proyectos
pmu_margin_proy <- municipios %>% 
  inner_join(margin, by = "region") %>% 
  inner_join(pmu, by = c("estado", "municipio"))

filter(pmu_margin_proy, GM_2020 == "Alto" | GM_2020 == "Medio")
55/293 # 18.77 % de los proyectos del PMU tienen al menos la cat "medio" de margin

filter(margin_sh, GM_2020 == "Alto" | GM_2020 == "Medio" |
         GM_2020 == "Muy alto")
1284/2469 # 52.00 % de los municipios en México tienen al menos la cat "medio" de margin

pmu_rez_soc_proy <- pmu %>% 
  inner_join(margin_CONEVAL, by = c("estado", "municipio"))

filter(pmu_rez_soc_proy, grado_rez_soc == "Alto" |
         grado_rez_soc == "Medio" | 
         grado_rez_soc == "Muy alto")
13/293 # 4.43 % de los proyectos en el PMU son al menos la cat "medio" de rezago

filter(margin_CONEVAL, grado_rez_soc == "Alto" |
         grado_rez_soc == "Medio" | 
         grado_rez_soc == "Muy alto")
899/2469 # 36.41 % de los municipios en México son al menos la cat "medio" de rez

## Análisis comparativo población afromexicana
pmu_afrom_proy <- pmu %>% 
  inner_join(afrom, by = c("estado", "municipio"))

filter(pmu_afrom_proy, porcen_afro > 2.04)
82/293 # 27.99 % de los municipios en el PMU están por encima del promedio nacio

filter(afrom, porcen_afro > 2.04)
(142+471)/2469 # 24.83 % de los municipios en México están encima del prom nacio

## Análisis PMU y marginación proyectos ## 18.77%
pmu_margin_proy <- municipios %>% 
  inner_join(margin, by = "region") %>% 
  inner_join(pmu, by = c("estado", "municipio"))

ggplot(pmu_margin_proy, aes(GM_2020))+
  geom_bar(fill = "pink",
           col = "royalblue") +
  labs(x = "Índice de marginación (2020)",
       y = "Cantidad de proyectos del PMU\n",
       title = "Distribución de los proyectos del Programa de Mejoramiento Urbano (PMU)
por índice de marginación municipal (2020)")+
  theme_bw()

count(filter(pmu_margin_proy, GM_2020 == "Alto" | GM_2020 == "Medio"))
55/293

# Proyectos de infraestructura dentro de: 
# https://www.gob.mx/proyectosyprogramasprioritarios

## 1. 100 universidades públicas
uni_bienestar <- read_excel("uni_bienestar.xlsx")

## 6. Producción para el bienestar - café
## 6. Producción para el bienestar - caña de azúcar
## 7. Caminos rurales
## 9. Construcción refinería Dos Bocas
# TABASCO, PARAISO

## 10. Crédito ganadero a la palabra
## 11. Desarrollo del Istmo de Tehuantepec
istmo <- read_excel("municipios_istmo.xlsx")

final %>% 
  mutate(value_istmo = case_when(
  ((estado %in% istmo$estado)&(municipio %in% istmo$municipio)) ~ 1)) %>% 
  filter(value_istmo == 1)

## 12. Fertilizantes para el bienestar
## 16. Mejoramiento Urbano (hay estados que no coinciden, revisar esto)
## 17. Rehabilitación de refinerías
## 18. Modernización AICM
## 19. Nuevo Aeropuerto Internacional Felipe Ángeles
## 25. Reconstruyendo Esperanza
## 26. Rescate del Lago de Texcoco
## 27. Sembrando vida
## 29. Tren Maya
# Info de aquí: https://www.trenmaya.gob.mx/estados/
tren_maya_mun <- read_excel("tren_maya_municipios.xlsx")
final %>% 
  mutate(value_tm = case_when(
    ((estado %in% tren_maya_mun$estado)&
       (municipio %in% tren_maya_mun$municipio)) ~ 1)) %>% 
  filter(value_tm == 1)
18/41 # en 43.9 % de los municipios por los que pasa el Tren Maya hay al menos
      # un proyecto del PMU

18/58 # 31.03 % de los municipios del PMU corresponden con alguno
      # de los 41 municipios por donde pasa el Tren Maya

pmu %>% 
  mutate(value_tm = case_when(
    ((estado %in% tren_maya_mun$estado)&
       (municipio %in% tren_maya_mun$municipio)) ~ 1)) %>% 
  filter(value_tm == 1)
81/293 # 27.64 % de los proyectos del PMU se encuentran en alguno
       # de los 41 municipios (1.66% de los mun a nivel nacional)

## 4. Análisis de datos inferencial
#####
## 4. Análisis de datos inferencial

## Weno, ahí va la regresión logística binomial de Tren Maya
logit_tm_pmu <- municipios %>% 
  mutate(value_tm = case_when(
    ((estado %in% tren_maya_mun$estado)&(municipio %in% tren_maya_mun$municipio)) ~ 1)) %>% 
  mutate(value_pmu_mun = case_when(
    ((estado %in% final$estado)&(municipio %in% final$municipio)) ~ 1))

logit_tm_pmu[is.na(logit_tm_pmu)] = 0
summary(logit_tm_pmu)

m1 <- glm(value_pmu_mun ~ value_tm, data = logit_tm_pmu ,family = binomial("logit"))
summary(m1)

exp(coef(m1)) # Ser un municipio por donde pase el Tren Maya incremeta en 3,962 %
              # los momios de ser un municipio con al menos un proyecto del PMU

## Regresión lineal de proyectos y hablaind
lineal_pmu_hablaind <- municipios %>% 
  full_join(final, by = c("estado", "municipio"))

lineal_pmu_hablaind[is.na(lineal_pmu_hablaind)] = 0

m2 <- lm(frec ~ hablaind, data = lineal_pmu_hablaind)
summary(m2) # p-value de 0.129 y R^2 de 0.0009339

## Regresión lineal de proyectos y población afromexicana
lineal_pmu_afrom <- afrom %>% 
  full_join(final, by = c("estado", "municipio"))

lineal_pmu_afrom[is.na(lineal_pmu_afrom)] = 0

m3 <- lm(frec ~ porcen_afro, data = lineal_pmu_afrom)
summary(m3) # p-value de 0.604 y R^2 de 0.0001089

## Regresión lineal de proyectos e Índice de Marginación de CONAPO
lineal_pmu_margin <- municipios %>%
  inner_join(margin, by = "region") %>%
  full_join(final, by = c("estado", "municipio"))

lineal_pmu_margin[is.na(lineal_pmu_margin)] = 0

m4 <- lm(frec ~ IMN_2020, data = lineal_pmu_margin)
summary(m4) # p-value de 0.0000319 y R^2 de 0.006989

## Regresión lineal de proyectos y Tren Maya
lineal_pmu_tm <- municipios %>% 
  mutate(value_tm = case_when(
    ((estado %in% tren_maya_mun$estado)&
       (municipio %in% tren_maya_mun$municipio)) ~ 1)) %>%
  full_join(final, by = c("estado", "municipio"))

lineal_pmu_tm[is.na(lineal_pmu_tm)] = 0

m5 <- lm(frec ~ value_tm, data = lineal_pmu_tm)
summary(m5) # p-value de 0.0000000000000002 y R^2 de 0.04099

## Tabla para presentar:
screenreg(list(m2, m3), 
          custom.model.names = c("Modelo 1", "Modelo 2"),
          custom.coef.names = c("(Intercepto)", 
                                "% pob indígena", 
                                "% pob afrom"),
          digits = 6)

## 30. Zona libre de la frontera norte

# ejem <- coord_cl %>% 
#   select(estado, municipio) %>% 
#   mutate(value_istmo = case_when(
#     ((estado %in% istmo$estado)&(municipio %in% istmo$municipio)) ~ 1)) %>% 
#   mutate(value_tren_m = case_when(
#     ((estado %in% tren_maya$estado)&(municipio %in% tren_maya$municipio)) ~ 1)) %>% 
#   mutate(value_unis = case_when(
#     ((estado %in% uni_bienestar$estado)&(municipio %in% uni_bienestar$municipio)) ~ 1)) %>% 
#   mutate(value_dos_b = case_when(
#     ((estado == "TABASCO")&(municipio == "PARAISO")) ~ 1))

# Número de proyectos del PMU que se encuentran en municipios o estados
# donde hay otros proyectos prioritarios de infraestructura o condicionados
# por territorio
