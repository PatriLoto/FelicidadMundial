
#instalo los paquetes de una vez, lo malo que si tiene errores con un paquete corta la instalación del resto
pacman::p_load(viridisLite, ggpubr, countrycode, paletteer)
#install.packages("paletteer")
library(easypackages)
install.packages("formattable")
library(formattable)
libraries("tidyverse", "grid","gridExtra","ggpubr", "ggrepel","extrafont","countrycode", "viridis", "viridisLite", "maps")
#install.packages("highcharter")
#library(maps)
extrafont::fonts()
loadfonts()
extrafont::choose_font(c("GillSans", "Verdana", "Roboto"), quiet = TRUE)

#library(gridExtra)
#library(ggpubr)
#library(ggrepel)
#library(extrafont)
#library(countrycode) 
#library(viridis)
#library(viridisLite)


libertad <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-08-14/libertad.csv")
View(libertad)

country2<-countrycode::codelist
View(country2)
#para unir genc3c y iso3c
country <-country2%>%select(genc3c,country.name.en, un.name.es)   #selecciona el código y el nombre del país
libertad$codigo_iso[libertad$codigo_iso=='BRD']='BRB'
#country$genc3c[country$genc3c=='BRB']='BRD' el incorrecto está en libertad
View(country)
# es mejor trabajar con un.name.es, ya que tiene los nombres en español

#para unir con los datos del mapa: world, necesito country.name.e
country$country.name.en[country$country.name.en=='United States']='USA'
country$country.name.en[country$country.name.en=='Hong Kong SAR China']='Hong Kong'


world_map <- map_data("world")
View(world_map)
world_map %>% filter(subregion=='Hong Kong')
world_map$region[world_map$subregion=='Hong Kong']='Hong Kong'


dfp<-libertad %>%
  left_join(country, by=c('codigo_iso'='genc3c'))%>%   # tbm podría utilizar para unir iso3c
  left_join(world_map, by=c('country.name.en'='region'))%>%
  filter(anio==max(libertad$anio))
View(dfp)
#datos de Latinoamérica
Latinoamerica<-dfp%>% filter(region=='Latinoamérica y el Caribe')%>%View

#me trae valores repetidos, ya que este se unio a map y por cada subregión repito el país
menor_lib= dfp%>%filter(anio==2016)%>%top_n(-5,libertad_economica_puntaje)%>%
  select(pais,libertad_economica_puntaje)%>%
  arrange(libertad_economica_puntaje)%>%View()
#---------------------------------------------------------------------------
## Libertad económica
#menor libertad económica
menor_lib_economica <-libertad%>%filter(anio==2016)%>%top_n(-5,libertad_economica_puntaje)%>%
  select(pais,libertad_economica_puntaje, codigo_iso)%>%
  arrange(libertad_economica_puntaje)             #%>%View()
menor_lib_economica
#mayor libertad económica
mayor_lib_económica <- libertad%>%filter(anio==2016)%>%top_n(5,libertad_economica_puntaje)%>%
  select(pais,libertad_economica_puntaje, codigo_iso)%>%
  arrange(desc(libertad_economica_puntaje))        # %>%View()

# probar patchwork con los gráficos de libertad económica 
#------------------------------------------------------------------------------
##Libertad humana

#menor libertad humana
menor_lib_humana <-libertad%>%filter(anio==2016)%>%top_n(-5,libertad_humana_puntaje)%>%
  select(pais,libertad_humana_puntaje)%>%
  arrange(libertad_humana_puntaje)  #%>%View()
menor_lib_humana 
#mayor libertad humana
mayor_lib_humana <-libertad%>%filter(anio==2016)%>%top_n(5,libertad_humana_puntaje)%>%
  select(pais,libertad_humana_puntaje)%>%
  arrange(desc(libertad_humana_puntaje))      #%>%View()
#------------------------------------------------------------------------------
##Libertad personal

#menor libertad personal
menor_lib_personal <-libertad%>%filter(anio==2016)%>%top_n(-5,libertad_personal_puntaje)%>%
  select(pais,libertad_personal_puntaje)%>%
  arrange(libertad_personal_puntaje)             #%>%View()

#mayor libertad personal
mayor_lib_personal <-libertad%>%filter(anio==2016)%>%top_n(5,libertad_personal_puntaje)%>%
  select(pais,libertad_personal_puntaje, )%>%
  arrange(desc(libertad_personal_puntaje))        #%>%View()
# agregar graficos de barras para libertad personal 

#--------------------------------------------------------------------------------------------
menor_lib_economica
#NO TENEMOS EN CUENTA REBUBLICA DEL CONGO PORQUE NO TIENE LOS VALORES DE LAT Y LONG
dfp_menos<-dfp%>%
  filter(pais %in% c('Argentina','Venezuela', 'Libia', 'Siria','Algeria'))%>%group_by(pais)%>%
  summarise(lat=mean(lat),
            long=mean(long))%>%
  left_join(menor_lib_economica)         #%>%View()
dfp_menos
#SOLO VERIFICO
libertad%>%filter(codigo_iso=='COG'& anio==2016)%>%View()
dfp%>%filter(codigo_iso=='COG'& anio==2016)%>%View()

dfp_mayor <-dfp%>%
  filter(pais %in% c('Singapur','Hong Kong', 'Nueva Zelanda', 'Irlanda','Suiza'))%>% group_by(pais)%>%
  summarise(lat=mean(lat),
            long=mean(long, na.rm = F))%>%
  left_join(mayor_lib_económica)               #%>%View()
dfp_mayor
#dfp_mayor <-dfp%>%
 #ifelse ((libertad$pais == mayor_lib_económica$pais),pais)%>% group_by(libertad$pais)%>%
  #summarise(lat=mean(lat),
   #         long=mean(long, na.rm = F))%>%
  #left_join(mayor_lib_económica)

df_leyenda <-rbind(dfp_mayor,dfp_menos)%>%mutate("country"= df_leyenda$pais)
df_leyenda

#prueba <- dfp %>% filter(pais  %in%  c('Argentina','Venezuela', 'Libia', 'Siria', 'República del Congo'))%>%
 # select(pais,libertad_economica_puntaje,long, lat)%>%view()
#no
library(highcharter)
library(dplyr)
library(purrr)
n <- 10
hcmap() %>% 
  hc_add_series(data = df_leyenda, type = "mapbubble",
                minSize = 0, maxSize = 30) %>% 
  hc_motion(enabled = TRUE, series = 1, labels = 1:n,
            loop = TRUE, autoPlay = TRUE, 
            updateInterval = 1000, magnet = list(step =  1)) %>% 
  hc_plotOptions(series = list(showInLegend = FALSE))

mapdata <- get_data_from_map(download_map_data("custom/world"))
mapdata %>%filter('iso-a3' =="FRO")%>%view()

hcmap(map = "custom/world", data = df_leyenda,
      joinBy = c('iso-a3', 'codigo_iso'), value = 'libertad_economica_puntaje', name = "Indíce de libertad económica")

USArrests <- mutate(df_leyenda, "woe-name"= rownames(df_leyenda))
USArrests
#para determinar la escala del grafico
vmax <- max(df_leyenda$libertad_economica_puntaje, na.rm=T)
vmin <- min(df_leyenda$libertad_economica_puntaje, na.rm=T)  

ggplot()+
geom_polygon(data=world_map, aes(x = long, y = lat, group=group),fill='grey')+
  geom_polygon(data=df_leyenda, aes(x = long, y = lat,fill=libertad_economica_puntaje))+
  scale_fill_viridis(name="Libertad ecnonomica", begin = 0, end = 1,
                     limits = c(vmin,vmax), na.value="gray99") +
  geom_label_repel(aes(label = paste0(libertad$country,'\n',libertad_economica_puntaje),
                       x=long,
                       y=lat),
                   data = dfp_menos,  
                   size = 4,  fill ='#440154FF',
                   family="Atma Light" ,
                   color='white',fontface = 'bold',
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.3, "lines"))+
   geom_label_repel(aes(label = paste0(libertad$country,'\n',libertad_economica_puntaje),
                       x=long,
                       y=lat),
                   data = dfp_mayor,  
                   size = 5,  fill ='#e8fa5bff',
                   family="Atma Light" ,
                   color='black',fontface = 'bold',
                   box.padding = unit(0.35, "lines"),
                   point.padding = unit(0.3, "lines"))+
  labs(x='',y='',title = paste0('Libertad económica ',emojifont::emoji('earth_americas')),
       caption=paste0('informe año: 2016 ~ ',emojifont::emoji('heart'),' by @'),
       subtitle = '"La libertad humana es un concepto social que reconoce la dignidad de los individuos "')+
  theme_void()+
  theme(
    legend.background = element_rect(fill='#2a2a2a'),
    legend.text = element_text(color='white'),
    plot.background = element_rect(fill='#2a2a2a',color ='#2a2a2a' ),
    panel.background =element_rect(fill='#2a2a2a'),
    legend.position=c(0.11, 0.32),
    legend.key = element_rect(fill = "#2a2a2a", color = NA),
    legend.title = element_text(color = "white", size = 10, hjust = 0.5),
    plot.title = element_text(family =     "Pacifico"    ,
                              hjust = 0.5,
                              size=30,colour = 'white',
                              face = 'bold'),
    plot.subtitle =  element_text(family =     "Atma Light"     ,
                                  hjust = 0.5,
                                  size=15, face = 'bold',
                                  colour = '#73d055ff' ),
    plot.caption = element_text(family =     "Atma Light"     ,
                                hjust =0.8,
                                size=13, face = 'bold',
                                colour = '#73d055ff' )
  )

#(Countries shown in blue have a smaller ratio, while countries shown in red have a larger (less favorable) ratio). Grey indicates missing data.
#White indica a median ratio close to the world median

hc_add_series(mapData = world, showInLegend = FALSE, nullColor = "#424242",
              borderWidth = 0)

data("USArrests", package = "datasets")
USArrests <- mutate(USArrests, "woe-name" = rownames(USArrests))
USArrests

#agregar la tabla donde muestre asaltos, vioalciones y asesinatos

x <- c("Asesinatos", "Assaltos","Violaciones")
y <- sprintf("{point.%s:.3f}", c(USArrests$Murder, USArrests$Assault, USArrests$Rape))
tltip <- tooltip_table(x, y)

hcmap(map = "countries/us/us-all", data = USArrests,
      joinBy = "woe-name", value = "Assault", name = "asaltos", color = "#2980b9")%>%
 
   hc_chart(backgroundColor = "#161C20")%>%  #
  hc_title(
    text = "Crimenes por Estado <span style=\"color:#e5b13a\"> Estado</span>",
    useHTML = TRUE) %>%
  hc_tooltip(table = TRUE,useHTML = TRUE, headerFormat = "", pointFormat = tltip)%>%
  hc_add_theme(hc_theme_db())


# download_map_data = FALSE        
hcmap(map = "countries/us/us-all", data = USArrests,
      joinBy = "woe-name", value = "UrbanPop", name = "Urban Population",
      download_map_data = FALSE) 
