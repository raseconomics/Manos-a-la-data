library(readxl)
library(dplyr)
library(tidyr)

# aqu√≠ pon tu ruta exacta del archivo.
# Recuerda, entra arriba a: 1 "session", 2 "set working directory" y 3 la opci√≥n "to source file location"
setwd("C:/Users/C…SAR/Desktop/Curso R/MDv01sesion2_parte-practica-master/1 Reactiva Peru")

empresas_reactiva_peru <- readxl::read_xlsx("empresas.xlsx",skip = 1)


View(empresas_reactiva_peru)
# ¬øqu√© ves?

empresas_reactiva_peru <- empresas_reactiva_peru[complete.cases(empresas_reactiva_peru$DEPARTAMENTO),]
# ¬øqu√© cambios has visto?

empresas_reactiva_peru <- empresas_reactiva_peru[,-1]
# ¬øqu√© cambios has visto?

# como hacerlo en una sola linea?
empresas_reactiva_peru <- readxl::read_xlsx("empresas.xlsx",skip = 1) # cargamos de nuevo para tener la data en bruto
empresas_reactiva_peru <- empresas_reactiva_peru[complete.cases(empresas_reactiva_peru$DEPARTAMENTO),-1]
# Otra cosa!
View(empresas_reactiva_peru)



# veamos qu√© tipo de data es cada uno
sapply(empresas_reactiva_peru,class)

# hay variables en formato character que no deber√≠an ser. Las columnas 6 y 7.

empresas_reactiva_peru[,c(6,7)] <- sapply(empresas_reactiva_peru[,c(6,7)],function(x) round(as.numeric(as.character(x)),2) )

# veamos qu√© tipo de data es cada uno nuevamente
sapply(empresas_reactiva_peru,class)

# veamos qu√© sectores hay
table(empresas_reactiva_peru$SECTOR)

# veamos qu√© sectores hay
table(empresas_reactiva_peru$`NOMBRE ENTIDAD OTORGANTE DEL CR…DITO`)


# hay que crear una nueva variable cobertura! hora de usar dplyr

empresas_reactiva_peru <- empresas_reactiva_peru %>%
  mutate(nivel_cobertura = round(`MONTO COBERTURA`/`MONTO PR…STAMO`*100)) 

View(empresas_reactiva_peru)

####################### usando dplyr #############################################################

#### 1 øCu·l es el monto promedio de los prÈstamos otorgados por Reactiva Per˙ por cada banco 
####   y ordenar la data de mayor a menor?

empresas_reactiva_peru %>% # la BD!
  group_by(`NOMBRE ENTIDAD OTORGANTE DEL CR…DITO`) %>% # agrupala segun...
  summarise(promedio_prestamos=mean(`MONTO PR…STAMO`))%>% # crea una nueva variable
  arrange(desc(promedio_prestamos))%>% # reordenala de mayor a menor
  View() # mira la data

#### 2 øCu·l es el monto promedio de los prÈstamos otorgados por Reactiva Per˙ seg˙n sector
#### y ordenar la data de mayor a menor?

empresas_reactiva_peru %>%# la BD!
  group_by(SECTOR) %>% # agrupala segun...
  summarise(promedio_prestamos=mean(`MONTO PR…STAMO`))%>%# crea una nueva variable
  arrange(desc(promedio_prestamos))%>% # reordenala de mayor a menor
  View() # mira la data

#### 3 øCu·l es el monto promedio de los prÈstamos otorgados por Reactiva Per˙ seg˙n sector y banco
#### y ordenar la data de mayor a menor?

empresas_reactiva_peru %>%# la BD!
  group_by(SECTOR, `NOMBRE ENTIDAD OTORGANTE DEL CR…DITO`) %>% # agrupala segun...
  summarise(promedio_prestamos=mean(`MONTO PR…STAMO`))%>%# crea una nueva variable
  arrange(desc(promedio_prestamos))%>% # reordenala de mayor a menor
  View() # mira la data

#### 4 øCu·l es el monto promedio y n˙mero de prÈstamos otorgados por Reactiva Per˙ seg˙n sector y banco. 
####   Ordenar seg˙n n˙meros de prÈstamos.

empresas_reactiva_peru %>%# la BD!
  group_by(SECTOR,`NOMBRE ENTIDAD OTORGANTE DEL CR…DITO`) %>% # agrupala segun...
  summarise(promedio_prestamos=mean(`MONTO PR…STAMO`),numero=n())%>%# crea una nueva variable
  arrange(desc(numero))%>% # reordenala de mayor a menor
  View() # mira la data

#### 5 Filtra las CMAC y Financieras y øCu·l es el monto promedio y n˙mero de prÈstamos otorgados por 
#### Reactiva Per˙ seg˙n sector y banco. Ordenar seg˙n n˙meros de prÈstamos.

empresas_reactiva_peru %>%# la BD!
  filter(`TIPO DE ENTIDAD OTORGANTE DEL CR…DITO` %in% c("CMAC","FINANCIERAS") ) %>% # c() es una funciÛn para crear vectores.  
  group_by(SECTOR,`NOMBRE ENTIDAD OTORGANTE DEL CR…DITO`) %>% # agrupala segun...
  summarise(promedio_prestamos=mean(`MONTO PR…STAMO`),numero=n())%>%# crea una nueva variable
  arrange(desc(numero))%>% # reordenala de mayor a menor
  View() # mira la data

#### 6 Filtra todos excepto las CMAC y Financieras y øCu·l es el monto promedio y n˙mero de prÈstamos otorgados por Reactiva
#### Per˙ seg˙n sector y banco?
#### Ordenar seg˙n n˙meros de prÈstamos.

empresas_reactiva_peru %>%# la BD!
  filter(!`TIPO DE ENTIDAD OTORGANTE DEL CR…DITO` %in% c("CMAC","FINANCIERAS") ) %>% # c() es una funciÛn para crear vectores.  
  group_by(SECTOR,`NOMBRE ENTIDAD OTORGANTE DEL CR…DITO`) %>% # agrupala segun...
  summarise(promedio_prestamos=mean(`MONTO PR…STAMO`),numero=n())%>%# crea una nueva variable
  arrange(desc(numero))%>% # reordenala de mayor a menor
  View() # mira la data



####################### TAREA DIRIGIDA ##############################################################################

#### 1 Filtra las CRAC, calculas los montos totales (usa sum()) y ordena seg⁄n total de prÈstamos (variable creada) 

empresas_reactiva_peru %>%# la BD!
  filter(`TIPO DE ENTIDAD OTORGANTE DEL CR…DITO` %in% c("CRAC") ) %>% # c() es una funciÛn para crear vectores.  
  group_by(`numero`) %>% # agrupala segun...
  summarise(sumatoria_prestamos=sum(`MONTO PR…STAMO`), numero=n() %>% # crea una nueva variable
  arrange(desc(numero))%>% # reordenala de mayor a menor
  View()

#### 2 øCu·l es el monto promedio de los prÈstamos otorgados por Reactiva Per˙ seg˙n departamento? 
#### Ordenar por monto promedio

empresas_reactiva_peru %>%# la BD!
  group_by(DEPARTAMENTO) %>% # agrupar
  summarise(prom_prestamos=mean(`MONTO PR…STAMO`))%>% #nueva variable
  arrange(desc(prom_prestamos))%>% #reorden de mayor a menor
  View() # mira la data

#### 3 øCu·l es el monto promedio y n˙mero de los prÈstamos otorgados por Reactiva Per˙ seg˙n departamento y sector? 
#### Ordenar por numero de prestamos

empresas_reactiva_peru %>%
  group_by(DEPARTAMENTO, SECTOR) %>%
  summarise(prom_prestamos=mean(`MONTO PR…STAMO`), prestamos=n()) %>%
  arrange(desc(prestamos)) %>% 
  View()


#### 4 Filtra (1) Sector: comercio y (2) entidades financieras: No banca m˙ltiple y 
#### Calcula el monto promedio seg˙n departamento y banco y ordenar por mediana de prÈstamos.

empresas_reactiva_peru %>% 
  filter(SECTOR %in% c("COMERCIO") ) %>% # c() es una funciÛn para crear vectores.  
  filter(!`TIPO DE ENTIDAD OTORGANTE DEL CR…DITO` %in% c("BANCA MULTIPLE") ) %>%
  group_by(DEPARTAMENTO,`NOMBRE ENTIDAD OTORGANTE DEL CR…DITO`) %>% 
  summarise(prom_prestamos=mean(`MONTO PR…STAMO`), mediana_prestamos=median(`MONTO PR…STAMO`)) %>% 
  arrange(desc(mediana_prestamos)) %>% 
  View()


#### 5 Filtra los prÈstamos mayores a 1 000 000 (un millÛn) con niveles de cobertura especÌfico de 90 y 80, 
#### Considerando solo 4 principales bancos del paÌs ordenados seg˙n n˙mero de prÈstamos y monto total

empresas_reactiva_peru %>% 
  filter(PR…STAMOS %in% c(">1000000")) %>%
  filter(nivel_cobertura %in% c("90"&"80")) %>% 
  summarise(prestamos=n(), monto_total=(`MONTO PR…STAMO`), bancos_top=(CR…DITO&BANCO BBVA PERU&INTERBANK)) %>% 
  group_by(prestamos, monto_total) %>% 
  arrange(desc(prestamos))
  View()
  

####################### Retos

#### 1 ¬øCu√°l es el sector con m√°s pr√©stamos totales seg√∫n cada departamento? (RETO)



