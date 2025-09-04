# Código empleado para la realización del trabajo de fin de máster titulado:

# Análisis de la evolución anual de la afluencia en el metro de Madrid

# Máster en Ciencia de Datos e Inteligencia de Negocios

# Universidad Complutense de Madrid

# Autora: Ángela Trueba Fernández

# Septiembre de 2025

#-------------------------------------------------------------------------------
#                         CARGA DE LIBRERÍAS Y DATOS
#-------------------------------------------------------------------------------

library(readxl)
library(corrplot)
library(mice)
library(psych)
library(dplyr)
library(e1071)
library(caret)
library(car)
library(class)
library(FNN)
library(pROC)
library(tidyr)
library(lubridate)
library(purrr)
library(ggplot2)
library(scales)
library(MetBrewer)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(cluster)  
library(factoextra)
library(dbscan)
library(leaflet)
library(RColorBrewer)


setwd("~/Desktop/TFM")
datos<-read_xlsx("Demanda Histórica MdM.xlsx")


#-------------------------------------------------------------------------------
#                              DEPURACIÓN DE DATOS
#-------------------------------------------------------------------------------
# Estudiar el número de NAs en cada columna
sum(is.na(datos))
colSums(is.na(datos)) #No hay NAs

# Estudiar si existen datos negativos de entradas
any(datos$Entradas < 0) #False

# Estudiar la estructura de la base de datos
str(datos) #Transformamos la variable fecha a formato fecha y id a caracter

datos$AÑO <- as.Date(paste0(datos$AÑO, "-12-31"))
datos$ID_NUMERICA<- as.character(datos$ID_NUMERICA)
datos$NOMBRE<-as.factor(datos$NOMBRE)
str(datos) #La estructura ya es correcta

# Estudiar si coincide el número de IDs con el número de Nombres de estaciones
length(unique(datos$ID_NUMERICA))#=251
length(unique(datos$NOMBRE))#=249

#Son distintas, por lo que hay 2 estaciones con doble ID

# Crear una tabla cruzada para ver la frecuencia de cada combinación de estación e ID
tabla <- table(datos$NOMBRE, datos$ID_NUMERICA)
print(tabla)

# Verificar si alguna estación tiene más de un ID
combinaciones <- apply(tabla, 1, function(x) sum(x > 0) > 1)

# Estaciones con más de un ID
estaciones_con_multiples_ids <- names(combinaciones[combinaciones])
print(estaciones_con_multiples_ids)

#Las Tablas y Pinar de Chamartin tienen asociados dos IDs

#Esto ocurre porque se toman por separado los datos de metro y metro ligero
#Le cambiamos el nombre a las de Metro Ligero

datos <- datos %>%
  mutate(NOMBRE = case_when(
    ID_NUMERICA %in% c(5119, 5111) ~ paste0(NOMBRE, " ML"),
    TRUE ~ NOMBRE
  ))

#-------------------------------------------------------------------------------
#                       IMPUTACIÓN DE DATOS FALTANTES
#-------------------------------------------------------------------------------

# Otro problema con el que nos encontramos: resulta que no todas las estaciones
# tienen datos desde el 2000 porque puede que se hayan añadido después.

# Estudiar la frecuencia con la que aparecen los datos, para así ver si hay 24
# observaciones de todas las estaciones
frecuencias <- datos %>%
  count(datos$NOMBRE, name = "frecuencia") %>%
  arrange(desc(frecuencia))  # Ordenar de mayor a menor

print(frecuencias) #No todas tienen 24 datos



# Filtrar las estaciones que tienen datos en el año 2000
estaciones_desde_2000 <- datos %>%
  filter(AÑO == "2000-12-31") %>%
  distinct(NOMBRE)
# Contar el número de estaciones
num_estaciones_2000 <- nrow(estaciones_desde_2000)

print(num_estaciones_2000) #158 estaciones tienen datos desde el año 2000


# Encontrar el primer año con datos para cada estación
años_inicio <- datos %>%
  group_by(NOMBRE) %>%
  summarise(Año_Inicio = min(AÑO), .groups = "drop")

# Obtener el año de inicio más tardío
año_inicio_max <- max(años_inicio$Año_Inicio)

# Filtrar las estaciones que comenzaron en ese año
estaciones_mas_tardias <- años_inicio %>%
  filter(Año_Inicio == año_inicio_max)

# Mostrar los resultados
print(año_inicio_max) #2019
print(estaciones_mas_tardias) #La estación que más tarde empieza es Arroyofresno

# Con las estaciones que se han inaugurado más tarde no podemos hacer nada, es
# decir, no podemos imputar datos porque no existían con anterioridad. 

# Sin embargo, vamos a imputar datos de aquellas que han cerrado durante algún
# período de tiempo por obras.






#-------------------------------------------------------------------------------
#               IMPUTACIÓN DE DATOS DE GRAN VÍA USANDO 3 MÉTODOS
#-------------------------------------------------------------------------------

# Gran Vía cerró por obras en los años 2019 y 2020. Se van a probrar 3 métodos
# distintos para imputar los datos, buscando el resultado más coherente posible

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~USANDO EL PROMEDIO~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Se le atribuye a esos dos años, el valor del promedio de entradas en Gran Vía
# para el resto de años con información disponible

# Filtrar los datos de la estación de Gran Vía
df_granvia <- datos %>%
  filter(NOMBRE == "Gran Vía")

# Crear dos filas nuevas con valores NA para los años 2019 y 2020
nuevas_filas <- tibble(
  NOMBRE = c("Gran Vía", "Gran Vía"),
  AÑO = c(ymd("2019-12-31"), ymd("2020-12-31")),
  Entradas = c(NA, NA),
  ID_NUMERICA = rep(df_granvia$ID_NUMERICA[1], 2)
)

# Unir las filas nuevas con el dataframe original de la estación
df_granvia <- bind_rows(df_granvia, nuevas_filas) %>%
  arrange(AÑO)  # Ordenar por año

# Calcular el promedio de los años anteriores y posteriores para imputar
# Primero identificamos los años adyacentes disponibles para esa estación
años_adyacentes <- df_granvia %>%
  filter(!is.na(Entradas)) %>%
  select(AÑO, Entradas) %>%
  arrange(AÑO)

# Imputación usando el promedio de los valores de los años cercanos
promedio <- mean(años_adyacentes$Entradas, na.rm = TRUE)

# Imputar los valores faltantes para los años de cierre 
df_granvia1 <- df_granvia %>%
  mutate(Entradas = ifelse(is.na(Entradas), promedio, Entradas))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~INTERPOLACION LINEAL~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Interpolación lineal para imputar los valores faltantes de la variable Entradas,
# es decir, se busca que estos años faltantes tengan datos estimados coherentes
# con las tendencias de los datos disponibles

# Utilizamos el df anterior, que ya tiene las filas con NAs en 2019 y 2020
df_granvia2 <- df_granvia

# Aplicamos interpolación lineal para los valores faltantes, es decir, utiliza
df_granvia2$Entradas <- approx(df_granvia2$AÑO, df_granvia2$Entradas, 
                               xout = df_granvia2$AÑO, method = "linear")$y


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~INTERPOLACION SEVILLA~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Se toma una estación que no comparta línea con Gran Vía pero sí sea cercana a
# ella, pues tomando su crecimiento estimaremos cómo se hubiera comportado Gran
# Vía si no hubiera cerrado

# Se toma el crecimiento de la estación Sevilla en 2019 y 2020 para predecir esos 
# años de Gran Vía utilizando los datos de 2018


# Filtrar los datos para Gran Vía y Sevilla
df_granvia3 <- df_granvia

df_sevilla <- datos %>%
  filter(NOMBRE == "Sevilla")


# Calcular el crecimiento porcentual anual de Sevilla
df_sevilla <- df_sevilla %>%
  arrange(AÑO) %>%
  mutate(Crecimiento = (Entradas - lag(Entradas)) / lag(Entradas) * 100)

# Filtrar los años faltantes que queremos estudiar
df_sevilla_faltantes <- df_sevilla %>%
  filter(AÑO %in% c("2019-12-31", "2020-12-31"))

crecimiento2019<-df_sevilla_faltantes %>%
  filter(AÑO =="2019-12-31")
crecimiento2019<-crecimiento2019$Crecimiento
crecimiento2020<-df_sevilla_faltantes %>%
  filter(AÑO =="2020-12-31")
crecimiento2020<-crecimiento2020$Crecimiento

# Obtener las entradas de Gran Vía para 2018
entradas_2018_granvia <- df_granvia3$Entradas[df_granvia3$AÑO == ymd("2018-12-31")]

# Predecir las entradas de Gran Vía usando el crecimiento específico de Sevilla
df_granvia3_predicho <- df_granvia3 %>%
  arrange(AÑO) %>%
  mutate(Entradas_Predichas = case_when(
    AÑO == ymd("2019-12-31") ~ entradas_2018_granvia * (1 + crecimiento2019 / 100),  # Crecimiento para 2019
    AÑO == ymd("2020-12-31") ~ entradas_2018_granvia * (1 + crecimiento2020 / 100),  # Crecimiento para 2020
    TRUE ~ Entradas  # No cambiar los años donde ya hay datos
  ))

# Este es el método elegido como más coherente, pues no se ve afectado por los
# valores de la pandemia y los resultados que obtiene parecen encajar bien





#-------------------------------------------------------------------------------
#               Jarama, Henares y Hospital del Henares en 2023
#-------------------------------------------------------------------------------

# En 2023, el último año de recogida de datos, Jarama, Henares y Hospital del
# Henares no contienen datos debido a su cierre por obras. En este caso, se
# decide que el método a utilizar es la regresión lineal

# Se crea una función que prediga linealmente los valores de 2023
predecir_2023 <- function(df, estacion) {
  # Filtrar datos de la estación
  df_estacion <- df %>%
    filter(NOMBRE == estacion)
  
  # Verificar que haya suficientes datos para hacer la predicción
  if (nrow(df_estacion) < 2) {
    stop(paste("No hay suficientes datos para predecir 2023 en la estación:", estacion))
  }
  
  # Convertir el año a numérico para regresión
  df_estacion <- df_estacion %>%
    mutate(AÑO_NUM = year(AÑO))
  
  # Ajustar una regresión lineal (Entradas ~ AÑO_NUM)
  modelo <- lm(Entradas ~ AÑO_NUM, data = df_estacion)
  
  # Predecir el valor de Entradas para 2023
  prediccion_2023 <- predict(modelo, newdata = data.frame(AÑO_NUM = 2023))
  
  # Crear la fila con la predicción
  nueva_fila <- tibble(
    NOMBRE = estacion,
    AÑO = ymd("2023-12-31"),
    Entradas = prediccion_2023,
    ID_NUMERICA = df_estacion$ID_NUMERICA[1]  # Usar el mismo ID de la estación
  )
  
  # Unir datos originales con la nueva fila
  df_estacion <- bind_rows(df_estacion, nueva_fila) %>%
    arrange(AÑO)
  
  return(df_estacion)
}

# Lista de estaciones a predecir
estaciones <- c("Jarama", "Henares", "Hospital del Henares")

# Aplicar la función a cada estación y unir los resultados
df_predicciones <- bind_rows(lapply(estaciones, function(est) predecir_2023(datos, est)))

# Ver los resultados
print(df_predicciones)




#-------------------------------------------------------------------------------
#                  Añadimos las imputaciones al df original
#-------------------------------------------------------------------------------

# Filtrar solo las filas predichas para "Gran Vía" en 2019 y 2020
df_granvia_pred <- subset(df_granvia3_predicho, NOMBRE == "Gran Vía" & AÑO %in% c("2019-12-31", "2020-12-31"))

# Renombrar la columna Entradas_Predichas a Entradas para que coincida con el 
# formato original
df_granvia_pred$Entradas <- df_granvia_pred$Entradas_Predichas

# Eliminar la columna original de predicciones
df_granvia_pred$Entradas_Predichas <- NULL

# Añadir las filas predichas al dataframe original
datos <- rbind(datos, df_granvia_pred)


# Filtrar solo las filas predichas Jarama, Henares y Hospital del Henares
df_predicciones <- subset(df_predicciones, AÑO == "2023-12-31")

# Eliminar la variable AÑO_NUM creada unicamente para el proceso de predicción
df_predicciones$AÑO_NUM <- NULL

# Añadir las filas predichas al dataframe original
datos <- rbind(datos, df_predicciones)




#-------------------------------------------------------------------------------
#                                DATOS ATÍPICOS
#-------------------------------------------------------------------------------

# En este apartado se va a estudiar, de distintas maneras, la posible existencia
# de datos atípicos en las entradas a las estaciones

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~BOXPLOT~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Boxplot para visualizar valores atípicos de entradas
ggplot(datos, aes(y = Entradas)) +
  geom_boxplot(outlier.colour = met.brewer("Paquin")[8], outlier.shape = 16, outlier.size = 3) +
  labs(title = "Boxplot de Entradas", y = "Número de Entradas") +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 21),
    legend.text = element_text(size = 20)
  )

#~~~~~~~~~~~~~~~~~~~~~~~~Z-SCORE (Puntuación estándar)~~~~~~~~~~~~~~~~~~~~~~~~~~

# Mide cuántas desviaciones estándar se aleja de la media el valor

# Calcular la media y la desviación estándar de entradas
media_entradas <- mean(datos$Entradas, na.rm = TRUE)
sd_entradas <- sd(datos$Entradas, na.rm = TRUE)

# Calcular el Z-score
datos_zscore <- datos %>%
  mutate(Z_score = (Entradas - media_entradas) / sd_entradas)

# Filtrar valores atípicos, es decir, con Z-score mayor a 3 o menor a -3
valores_atipicos_z <- datos_zscore %>%
  filter(abs(Z_score) > 3)

# Mostrar los valores atípicos
print(valores_atipicos_z)

# Se estudias cuántos valores atípicos tiene cada estación según este método
frecuencias <- valores_atipicos_z %>%
  count(valores_atipicos_z$NOMBRE, name = "frecuencia") %>%
  arrange(desc(frecuencia))  # Ordenar de mayor a menor

print(frecuencias)




#-------------------------------------------------------------------------------
#                           ANÁLISIS DE TENDENCIAS
#-------------------------------------------------------------------------------

# Generación de gráficos de tendencia para estudiar el comportamiento de las
# entradas por línea o estación

#~~~~~~~~~~~~~~~~~~~~~~~~~Tendencia anual por estación~~~~~~~~~~~~~~~~~~~~~~~~~~

# Primer gráfico de tendencias por estación, muy caótico

ggplot(datos, aes(x = AÑO, y = Entradas, group = NOMBRE)) +
  geom_line() +
  labs(title = "Tendencia anual por estación")+ 
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 21),
    legend.text = element_text(size = 20)
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~GRÁFICO INTERACTIVO~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Gráfico de tendencias por estación interactivo, en el que pueden seleccionarse
# las estaciones de las que se desea obtener la información

p <- ggplot(datos, aes(x = AÑO, y = Entradas, color = NOMBRE)) +
  geom_line(size = 1.2) +
  labs(title = "Tendencia anual interactiva") +
  theme_minimal()+ 
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 21),
    legend.text = element_text(size = 20)
  )

grafico <- ggplotly(p)

# Guardamos como HTML
saveWidget(grafico, "grafico_tendencia_anual_estaciones.html", selfcontained = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~GRÁFICO INTERACTIVO POR LÍNEAS~~~~~~~~~~~~~~~~~~~~~~~~~~

# Ahora, se van a agrupar las estaciones por línea para poder representarlas
# gráficamente de una manera más clara. Para ello, nos ayudamos de una base de
# datos que asocia las estaciones a sus líneas

# Cargamos la base de datos que contiene la información de las líneas
lineas <- read_xlsx("DF_Generico.xlsx")

# Este df contiene columnas con los nombres de las líneas, atribuyendo un 1 si
# la estación correspondiente pertenece a dicha línea. Sin embargo, este formato
# no es el preferido para trabajar con los datos del proyecto, por lo que se
# decidetransformar de formato ancho a largo (una fila por línea por estación)
lineas_limpio <- lineas %>%
  pivot_longer(cols = starts_with("L"), names_to = "Linea", values_to = "Presencia") %>%
  filter(Presencia == 1) %>%
  select(Nombre, Linea)


# Cruzamos los dos dfs
datos_con_linea <- datos %>%
  left_join(lineas_limpio, by = c("NOMBRE" = "Nombre"))

# Observamos cuántos datos faltantes hay, por si se ha perdido información en
# el cruce
observaciones_na <- datos_con_linea %>%
  filter(is.na(Linea))
observaciones_na %>%
  distinct(NOMBRE)


# Añadimos a los valores con NAs su linea correspondiente
# Aprovechamos para atribuirle a las Tablas y Pinar de Chamartin con ID distinto
# el valor ML1, pues el df de usado líneas no contempla el metro ligero
datos_con_linea <- datos_con_linea %>%
  mutate(Linea = case_when(
    NOMBRE == "Pío XII"        & is.na(Linea) ~ "L9",
    NOMBRE == "Álvarez de Villaamil"       & is.na(Linea) ~ "ML1",
    NOMBRE == "Antonio Saura"          & is.na(Linea) ~ "ML1",
    NOMBRE == "Blasco Ibáñez"   & is.na(Linea) ~ "ML1",
    NOMBRE == "Fuente de la Mora"   & is.na(Linea) ~ "ML1",
    NOMBRE == "María Tudor"   & is.na(Linea) ~ "ML1",
    NOMBRE == "Palas de Rey"   & is.na(Linea) ~ "ML1",
    NOMBRE == "Virgen del Cortijo"   & is.na(Linea) ~ "ML1",
    ID_NUMERICA==5119 ~ "ML1",
    ID_NUMERICA==5111 ~ "ML1",
    TRUE ~ Linea  # mantener el valor original en los demás casos
  ))

nrow(datos_con_linea)

# Eliminamos los duplicados que se han creado para Pinar de Chamartín
datos_con_linea <- datos_con_linea[!duplicated(datos_con_linea), ]
nrow(datos_con_linea)

# Definimos el orden correcto de las líneas
orden_lineas <- c("L1", "L2", "L3", "L4", "L5", "L6", "L7",
                  "L8", "L9", "L10", "L11", "L12","ML1")

# Convertir 'Linea' en un factor con ese orden
datos_con_linea <- datos_con_linea %>%
  mutate(Linea = factor(Linea, levels = orden_lineas))

# Asociamos cada línea a su color en la red de Metro de Madrid
colores_lineas <- c(
  "L1"  = "#00A9E0",
  "L2"  = "#E30613",
  "L3"  = "#FFD200",
  "L4"  = "#8C6239",
  "L5"  = "#6DC067",
  "L6"  = "#A7A9AC",
  "L7"  = "#F28C28",
  "L8"  = "#EC6EA8",
  "L9"  = "#92278F",
  "L10" = "#00205B",
  "L11" = "#007A33",
  "L12" = "#B59A2B",
  "ML1" = "#00649E"
)

# Creamos el gráfico interactivo con los colores oficiales
gg <- ggplot(datos_con_linea, aes(x = AÑO, y = Entradas, color = Linea, group = NOMBRE)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = colores_lineas) +
  labs(title = "Tendencia anual de entradas por estación y línea",
       x = "Año", y = "Entradas", color = "Línea de Metro") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 18, face = "bold"),     # Título del gráfico
    axis.title = element_text(size = 14),                    # Títulos de los ejes
    axis.text = element_text(size = 12),                     # Números de los ejes
    legend.title = element_text(size = 13),                  # Título de la leyenda
    legend.text = element_text(size = 12)                    # Texto de la leyenda
  )


grafico2 <- ggplotly(gg)

# Guardamos como HTML
saveWidget(grafico2, "grafico_tendencia_anual_estacionesylineas.html", selfcontained = TRUE)


#-------------------REPRESENTAMOS LAS LÍNEAS EN SU CONJUNTO---------------------

# Hasta ahora, se ha representado la tendencia de cada estación, pero no se ha
# tenido en cuenta la tendencia de las líneas como conjunto de datos de varias
# estaciones

# Agrupamos los datos por línea y año. Sumamos entradas
tendencia_linea_total <- datos_con_linea %>%
  group_by(Linea, AÑO) %>%
  summarise(Entradas_total = sum(Entradas, na.rm = TRUE), .groups = "drop")

# Creamos el gráfico interactivo
gg_lineas_total <- ggplot(tendencia_linea_total, aes(x = AÑO, y = Entradas_total, color = Linea)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = colores_lineas) +
  labs(title = "Tendencia de entradas totales por línea",
       x = "Año", y = "Entradas totales", color = "Línea de Metro") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 21),
    legend.text = element_text(size = 20)
  )

grafico3 <- ggplotly(gg_lineas_total)

# Guardamos como HTML
saveWidget(grafico3, "grafico_tendencia_anual_lineas.html", selfcontained = TRUE)





#-------------------------------------------------------------------------------
#                            CRECIMIENTO PROMEDIO
#-------------------------------------------------------------------------------

# Calculamos el crecimiento anual de las estaciones
crecimiento_estaciones <- datos %>%
  group_by(NOMBRE) %>%
  arrange(AÑO) %>%
  mutate(crecimiento = (Entradas - lag(Entradas)) / lag(Entradas)) %>%
  summarise(crecimiento_promedio = mean(crecimiento, na.rm = TRUE))


# Top 20 con mayor crecimiento
top20 <- crecimiento_estaciones %>%
  arrange(desc(crecimiento_promedio)) %>%
  slice_head(n = 20)

# Representamos gráficamente estos datos
ggplot(top20, aes(x = reorder(NOMBRE, crecimiento_promedio), y = crecimiento_promedio)) +
  geom_bar(stat = "identity", fill = met.brewer("Paquin")[9]) +
  coord_flip() +
  labs(title = "Top 20 Estaciones por Crecimiento Promedio Anual",
       x = "Estación",
       y = "Crecimiento Promedio (%)") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal()+ 
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 21),
    legend.text = element_text(size = 20)
  )



# Tomamos las 10 estaciones con mayor crecimiento negativo
negativas <- crecimiento_estaciones %>%
  filter(crecimiento_promedio < 0) %>%
  arrange(crecimiento_promedio) %>%
  head(10)

# Representamos gráficamente estos valores
ggplot(negativas, aes(x = reorder(NOMBRE, crecimiento_promedio), y = crecimiento_promedio)) +
  geom_bar(stat = "identity", fill = met.brewer("Paquin")[5]) +
  coord_flip() +
  labs(title = "Top 10 Estaciones con Crecimiento Promedio Negativo",
       x = "Estación",
       y = "Crecimiento Promedio (%)") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal()+ 
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 21),
    legend.text = element_text(size = 20)
  )


#-------------------------------------------------------------------------------
#              HOMOGENEIDAD EN EL CRECIMIENTO PROMEDIO POR LÍNEAS
#-------------------------------------------------------------------------------

# El primer paso es determinar cuál es la línea más homogénea, es decir,
# la que menos desviacion tenga

# Calculamos el crecimiento interanual de cada estación, para agruparlas después
# por línea y año. Se calcula la desviación estandar de cada estación,
# agrupando después por línea tomando la media de estos valores
homogeneidad_linea <- datos_con_linea %>%
  arrange(NOMBRE, AÑO) %>%
  group_by(NOMBRE) %>%
  mutate(crecimiento = (Entradas - lag(Entradas)) / lag(Entradas)) %>%
  ungroup() %>%
  group_by(Linea, AÑO) %>%
  summarise(sd_crecimiento = sd(crecimiento, na.rm = TRUE), .groups = "drop") %>%
  group_by(Linea) %>%
  summarise(media_sd = mean(sd_crecimiento, na.rm = TRUE)) %>%
  arrange(media_sd)

print(homogeneidad_linea)

#-------------------------------------------------------------------------------
#         HOMOGENEIDAD EN EL CRECIMIENTO PROMEDIO POR ZONA GEOGRÁFICA
#-------------------------------------------------------------------------------

# Ahora, vamos a ver si existe mas homogeneidad en el crecimiento dependiendo
# de la zona en la que se sitúe la estación geográficamente

# Unimos los datos con la base de datos que contiene información de las
# coordenadas de las estaciones
datos_geo <- datos_con_linea %>%
  left_join(lineas %>% select(Nombre, Longitud, Latitud), 
            by = c("NOMBRE" = "Nombre"))

# Comprobamos si hay alguna estación para la que no se tiene esta información
datos_geo %>%
  filter(is.na(Latitud) | is.na(Longitud)) %>%
  distinct(NOMBRE)

# Introducimos a mano los valores de Latitud y Longitud faltantes
datos_geo <- datos_geo %>%
  mutate(
    Latitud = case_when(
      NOMBRE == "Pinar de Chamartín ML" & is.na(Latitud) ~ 40.48028,
      NOMBRE == "Fuente de la Mora" & is.na(Latitud) ~ 40.4851807434942,
      NOMBRE == "Virgen del Cortijo" & is.na(Latitud) ~ 40.4874868829328,
      NOMBRE == "Antonio Saura" & is.na(Latitud) ~ 40.48543547012686,
      NOMBRE == "Álvarez de Villaamil" & is.na(Latitud) ~ 40.489031036691166,
      NOMBRE == "Blasco Ibáñez" & is.na(Latitud) ~ 40.493631255385445,
      NOMBRE == "María Tudor" & is.na(Latitud) ~ 40.497084525743475,
      NOMBRE == "Palas de Rey" & is.na(Latitud) ~ 40.50549570595704,
      NOMBRE == "Las Tablas ML" & is.na(Latitud) ~ 40.508274849701436,
      TRUE ~ Latitud
    ),
    Longitud = case_when(
      NOMBRE == "Pinar de Chamartín ML" & is.na(Longitud) ~ -3.666667,
      NOMBRE == "Fuente de la Mora" & is.na(Longitud) ~ -3.6630417431754623,
      NOMBRE == "Virgen del Cortijo" & is.na(Longitud) ~ -3.6610603420067473,
      NOMBRE == "Antonio Saura" & is.na(Longitud) ~ -3.653746269623128,
      NOMBRE == "Álvarez de Villaamil" & is.na(Longitud) ~ -3.6512614075541237,
      NOMBRE == "Blasco Ibáñez" & is.na(Longitud) ~ -3.655730302335781,
      NOMBRE == "María Tudor" & is.na(Longitud) ~ -3.6590377647219574,
      NOMBRE == "Palas de Rey" & is.na(Longitud) ~ -3.665048171158425,
      NOMBRE == "Las Tablas ML" & is.na(Longitud) ~ -3.6693855273657054,
      TRUE ~ Longitud
    )
  )


# Creamos zonas geográficas según las coordenadas disponibles
datos_geo <- datos_geo %>%
  mutate(
    Zona = case_when(
      Latitud >= median(Latitud, na.rm = TRUE) & Longitud < median(Longitud, na.rm = TRUE) ~ "Noroeste",
      Latitud >= median(Latitud, na.rm = TRUE) & Longitud >= median(Longitud, na.rm = TRUE) ~ "Noreste",
      Latitud < median(Latitud, na.rm = TRUE) & Longitud < median(Longitud, na.rm = TRUE) ~ "Suroeste",
      Latitud < median(Latitud, na.rm = TRUE) & Longitud >= median(Longitud, na.rm = TRUE) ~ "Sureste"
    )
  )

# Calculamos el crecimiento para cada estación y año
datos_geo <- datos_geo %>%
  arrange(NOMBRE, AÑO) %>%
  group_by(NOMBRE) %>%
  mutate(crecimiento = (Entradas - lag(Entradas)) / lag(Entradas)) %>%
  ungroup()

# Agrupamos las estaciones por zona y calculamos su desviación estándar
sd_por_zona_anual <- datos_geo %>%
  group_by(Zona, AÑO) %>%
  summarise(sd_crecimiento = sd(crecimiento, na.rm = TRUE), .groups = "drop")

# Calculamos la homogeneidad de cada zona como la media de las desviaciones 
# estándar de las estaciones que la componen
homogeneidad_zona <- sd_por_zona_anual %>%
  group_by(Zona) %>%
  summarise(media_sd = mean(sd_crecimiento, na.rm = TRUE)) %>%
  arrange(media_sd)

homogeneidad_zona


# Representamos en un gráfico de barras la homogeneidad de las cuatro zonas
ggplot(homogeneidad_zona, aes(x = reorder(Zona, media_sd), y = media_sd, fill = Zona)) +
  geom_col() +
  labs(title = "Homogeneidad del crecimiento por zona",
       x = "Zona",
       y = "Desviación estándar media del crecimiento") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))+ 
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 21),
    legend.text = element_text(size = 20)
  )



#-------------------------------------------------------------------------------
#           REPRESENTACIÓN DE LA MEDIA Y DESVIACIÓN DEL CRECIMIENTO
#                                 EN EL MAPA
#-------------------------------------------------------------------------------

# Creamos un df de crecimiento de las estaciones que guarde la media y la
# desviación típica de cada estación, así como la latitud y longitud
crecimiento_estaciones <- datos_geo %>%
  arrange(NOMBRE, AÑO) %>%
  group_by(NOMBRE) %>%
  mutate(crecimiento = (Entradas - lag(Entradas)) / lag(Entradas)) %>%
  summarise(
    media_crecimiento = mean(crecimiento, na.rm = TRUE),
    sd_crecimiento = sd(crecimiento, na.rm = TRUE),
    Latitud = first(Latitud),
    Longitud = first(Longitud),
    .groups = "drop"
  )


#-----------------------------MEDIA DE CRECIMIENTO------------------------------

# Definimos una paleta de colores para los puntos del mapa
paleta_media <- colorNumeric(
  palette = "RdYlBu",
  domain = crecimiento_estaciones$media_crecimiento
)

# Representamos el mapa de la ciudad de madrid con puntos sobre cada estación
# coloreados según el valor de media de crecimiento que tengan
mapa <- leaflet(crecimiento_estaciones) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~Longitud,
    lat = ~Latitud,
    radius = 6,
    color = ~paleta_media(media_crecimiento),
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste(
      "<b>Estación:</b>", NOMBRE, "<br>",
      "<b>Media crecimiento:</b>", round(media_crecimiento, 3), "<br>",
      "<b>SD crecimiento:</b>", round(sd_crecimiento, 3)
    )
  ) %>%
  addLegend(
    pal = paleta_media,
    values = ~media_crecimiento,
    position = "bottomright",
    title = "Media crecimiento"
  )

# Guardamos como HTML
saveWidget(mapa, "mapa_media_crecimiento.html", selfcontained = TRUE)

#-------------------------VARIABILIDAD DE CRECIMIENTO---------------------------

# Definimos una paleta de colores para los puntos del mapa
paleta_sd <- colorNumeric(
  palette = "Spectral",
  domain = crecimiento_estaciones$sd_crecimiento
)

# Representamos el mapa de la ciudad de madrid con puntos sobre cada estación
# coloreados según el valor de desviación estándar de crecimiento que tengan
mapa2 <- leaflet(crecimiento_estaciones) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~Longitud,
    lat = ~Latitud,
    radius = 6,
    color = ~paleta_sd(sd_crecimiento),
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste(
      "<b>Estación:</b>", NOMBRE, "<br>",
      "<b>Media crecimiento:</b>", round(media_crecimiento, 3), "<br>",
      "<b>SD crecimiento:</b>", round(sd_crecimiento, 3)
    )
  ) %>%
  addLegend(
    pal = paleta_sd,
    values = ~sd_crecimiento,
    position = "bottomright",
    title = "SD crecimiento"
  )

# Guardamos como HTML
saveWidget(mapa2, "mapa_desviación_crecimiento.html", selfcontained = TRUE)


#-------------------------------------------------------------------------------
#                                  CLUSTERING
#-------------------------------------------------------------------------------

# En este apartado se estudia la posibilidad de hacer agrupamientos de
# estaciones según su crecimiento promedio. Se trata con tres métodos: Ward,
# k-means y DBSCAN.


# Creamos un df con los datos de crecimiento anual de todas las estaciones
datos_crecimiento <- datos %>%
  arrange(NOMBRE, AÑO) %>%
  group_by(NOMBRE) %>%
  mutate(crecimiento = (Entradas - lag(Entradas)) / lag(Entradas)) %>%
  ungroup()

# Convertimos a matriz estos datos, donde las filas son las estaciones, las
# columnas son los años y los valores son los datos de crecimiento interanual
matriz_crecimiento <- datos_crecimiento %>%
  select(NOMBRE, AÑO, crecimiento) %>%
  pivot_wider(names_from = AÑO, values_from = crecimiento) %>%
  column_to_rownames("NOMBRE") %>%
  as.matrix()

# Cambiamos los valores faltantes por 0
matriz_crecimiento[is.na(matriz_crecimiento)] <- 0 

# Escalamos los valores para evitar que dominen los valores con mayor varianza
matriz_crecimiento <- scale(matriz_crecimiento)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~WARD~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calulamos la distancia euclídea de entre cada par de filas de la matriz de
# crecimiento
distancia <- dist(matriz_crecimiento, method = "euclidean")

# Aplicamos un algoritmo de clustering que emplea el método de Ward y lo
# representamos gráficamente con un dendograma
hc <- hclust(distancia, method = "ward.D2")
plot(hc, main = "Clustering jerárquico por patrón de crecimiento")


# Viendo el dendograma, decidimos cortar en 4 grupos, es decir, 4 clusters
grupos <- cutree(hc, k = 4)

# Vemos las estaciones que han quedado agrupadas
tabla_grupos <- data.frame(Estacion = names(grupos), Grupo = grupos)
head(tabla_grupos)

#Vemos cuántas estaciones hay en cada grupo
table(tabla_grupos$Grupo)


# Calculamos el índice de silhouette
sil_hc <- silhouette(grupos, dist(matriz_crecimiento))

# Lo representamos gráficamente
fviz_silhouette(sil_hc)
mean(sil_hc[, 3])  # Promedio de silhouette score



# Representamos gráficamente la tendencia de los grupos a lo largo de los años

# Añadimos la información de los grupos a los datos originales
datos_cluster <- datos %>%
  left_join(tabla_grupos, by = c("NOMBRE" = "Estacion"))

# Calculamos el crecimiento promedio por grupo
media_grupo <- datos_cluster %>%
  group_by(Grupo, AÑO) %>%
  summarise(media_entradas = mean(Entradas), .groups = "drop")

# Representación gráfica de la tendencia por grupos
ggplot(media_grupo, aes(x = AÑO, y = media_entradas, color = as.factor(Grupo))) +
  geom_line(size = 1) +
  labs(title = "Tendencia temporal promedio por grupo", color = "Grupo") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 21),
    legend.text = element_text(size = 20)
  )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~K-MEANS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Ahora, aplicamos otro método de clustering: k-means

# Borramos la columna del año 2000 porque el crecimiento es un valor ausente
matriz_crecimiento <- matriz_crecimiento[, !(colnames(matriz_crecimiento) == "2000-12-31")]

# Vamos a intentar encontrar el número de agrupaciones que aporte un mejor índice
# de Silhouette, con un máximo de 10 grupos
silhouette_scores <- c()
max_k <- 10

# Proceso iterativo que prueba las distintas agrupaciones y aplica el modelo k-means
for (k in 2:max_k) {
  set.seed(123)  # para reproducibilidad
  km <- kmeans(matriz_crecimiento, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(matriz_crecimiento))
  silhouette_scores[k] <- mean(ss[, 3])
}

# Se guarda la info de número de agrupaciones e índice obtenido
df <- data.frame(
  k = 2:max_k,
  silhouette = silhouette_scores[2:max_k])

# Representamos gráficamente esta información
ggplot(df, aes(x = k, y = silhouette)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Número de clústeres (k)",
    y = "Silhouette promedio",
    title = "Selección del número óptimo de clústeres con k-means"
  ) +
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 21),
    legend.text = element_text(size = 20),
    panel.background = element_blank(),       # Elimina el fondo gris
    panel.grid.major = element_blank(),       # Elimina la cuadrícula mayor
    panel.grid.minor = element_blank(),       # Elimina la cuadrícula menor
    axis.line = element_line(color = "black") # Añade línea en ejes
  )

# Se decide que la mejor opción son 3 clusters, y se aplica el método
set.seed(123)
kmeans_final <- kmeans(matriz_crecimiento, centers = 3, nstart = 25)

# Añadimos las etiquetas de grupo a los datos originales
resultados_kmeans <- datos_crecimiento %>%
  distinct(NOMBRE) %>%
  mutate(cluster_kmeans = kmeans_final$cluster[NOMBRE])


# Unimos los resultados de clustering con los datos originales de crecimiento
datos_con_cluster <- datos_crecimiento %>%
  left_join(resultados_kmeans, by = "NOMBRE")

# Calculamos la media de crecimiento por año y por clúster
crecimiento_medio_cluster <- datos_con_cluster %>%
  group_by(cluster_kmeans, AÑO) %>%
  summarise(crecimiento_medio = mean(crecimiento, na.rm = TRUE), .groups = "drop")

# Representamos gráficamente la evolución de este crecimiento promedio en el tiempo
ggplot(crecimiento_medio_cluster, aes(x = AÑO, y = crecimiento_medio, color = factor(cluster_kmeans))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Evolución temporal del crecimiento medio por clúster",
       x = "Año", y = "Crecimiento medio (%)", color = "Clúster") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 21),
    legend.text = element_text(size = 20)
  )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~DBSCAN~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Volvemos a calcular la matriz de crecimiento, porque le habíamos eliminado una
# fila con el método anterior

matriz_crecimiento <- datos_crecimiento %>%
  select(NOMBRE, AÑO, crecimiento) %>%
  pivot_wider(names_from = AÑO, values_from = crecimiento) %>%
  column_to_rownames("NOMBRE") %>%
  as.matrix()


set.seed(123)

# Reemplazamos los NAs por 0 
matriz_crecimiento[is.na(matriz_crecimiento)] <- 0

# Escalamos
matriz_crecimiento <- scale(matriz_crecimiento)

# Reemplazar posibles NAs generados tras escalar (por columnas constantes)
matriz_crecimiento[is.na(matriz_crecimiento)] <- 0

# Creamos un abanico de valores posibles, para ver qué combinación de parámetros
# nos dan un mejor resultado a la hora de aplicar el algoritmo

eps_values <- seq(0.5, 3, by = 0.2)
minPts_values <- 3:10

# Para guardar los resultados
resultados <- data.frame()

# Proceso iterativo que prueba varias combinaciones de parámetros y aplica el
# modelo dbscan
for (eps in eps_values) {
  for (minPts in minPts_values) {
    
    # Aplicar DBSCAN
    db <- dbscan(matriz_crecimiento, eps = eps, minPts = minPts)
    clusters <- db$cluster
    
    # Ignorar si solo hay 1 grupo (o todos son ruido)
    n_clusters <- length(unique(clusters[clusters != 0]))
    if (n_clusters <= 1) next
    
    # Calcular índice de silhouette (solo para los que no son ruido)
    sil_score <- mean(silhouette(clusters[clusters != 0],
                                 dist(matriz_crecimiento[clusters != 0, ]))[, 3])
    
    # Guardar resultados
    resultados <- rbind(resultados, data.frame(
      eps = eps,
      minPts = minPts,
      n_clusters = n_clusters,
      silhouette = sil_score
    ))
  }
}

# Vemos los mejores resultados ordenados
resultados_ordenados <- resultados %>% arrange(desc(silhouette))
print(resultados_ordenados)

# El mejor de todos ellos, el primero
mejor <- resultados_ordenados[1, ]
print(mejor)
eps_optimo <- mejor$eps
print(eps_optimo)
minPts_optimo <- mejor$minPts
print(minPts_optimo)


#Podemos visualizar los que mejores resultados dan en un gráfico
ggplot(resultados, aes(x = eps, y = minPts, fill = silhouette)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C", name = "Silhouette") +
  labs(title = "Evaluación de parámetros DBSCAN",
       x = "eps", y = "minPts") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 26),
    legend.title = element_text(size = 26),
    legend.text = element_text(size = 26)
  )

# Aplicamos el modelo con los parámetros óptimos
db_optimo <- dbscan(matriz_crecimiento, eps = eps_optimo, minPts = minPts_optimo)
# Resumen
print(db_optimo)

# Ver cuántas estaciones hay por grupo
table(db_optimo$cluster)  # 0 son "ruido" (estaciones que no encajan en ningún cluster)
# Añadimos los resultados al dataframe
tabla_grupos_dbscan <- data.frame(Estacion = rownames(matriz_crecimiento),
                                  Grupo = db_optimo$cluster)
# Unimos a los datos originales
datos_cluster_db <- datos %>%
  left_join(tabla_grupos_dbscan, by = c("NOMBRE" = "Estacion"))

# Tendencia promedio por grupo
media_grupo_db <- datos_cluster_db %>%
  filter(Grupo != 0) %>%  # Quitamos las estaciones ruido
  group_by(Grupo, AÑO) %>%
  summarise(media_entradas = mean(Entradas), .groups = "drop")

# Gráfico de evolución por grupo
ggplot(media_grupo_db, aes(x = AÑO, y = media_entradas, color = as.factor(Grupo))) +
  geom_line(size = 1.1) +
  labs(title = "Tendencia de entradas por grupos (DBSCAN)",
       color = "Grupo") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 26, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    legend.title = element_text(size = 21),
    legend.text = element_text(size = 20)
  )

