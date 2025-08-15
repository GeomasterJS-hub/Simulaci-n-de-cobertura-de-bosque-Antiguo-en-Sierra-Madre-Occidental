# Intalar paquetes
install.packages("tidyverse")
install.packages("gganimate")
install.packages("raster")
install.packages("rasterVis")
install.packages("sf")
install.packages("transformr")
install.packages("av")

# Cargar librerias
library(tidyverse)
library(gganimate)
library(raster)
library(rasterVis)
library(sf)
library(transformr)
library(conflicted)
library(lubridate) # para manejo consistente de fechas
library(av) # video MP4

# 1. Simulación de datos NDVI para Chihuahua
# Crear datos simulados de NDVI para tres años
set.seed(123)
years <- c(2000,2015,2020)

# Coordenadas aproximadas de Chihuahua
Chihuahua_bbox <- st_bbox(c(xmin = -109, xmax = -103, ymin = 25, ymax = 31),
                          crs = st_crs(4326))

# Función para generar datos NDVI simulados por año
generate_ndvi <- function(year) {
  #crear un raster vacio
  r <- raster(ncol = 100, nrows = 100,
              xmn = -109, xmx = -103, ymn = 25, ymx = 31,
              crs = "+proj=longlat +datum=WGS84")
  
  # valores base NDVI según el año (simulando degradacion)
 if (year == 2000) {
   base_ndvi <- 0.6
   noise <- 0.2
 } else if (year == 2015 ) {
   base_ndvi <- 0.5
   noise <- 0.25
 } else {
   base_ndvi <- 0.4
   noise <- 0.3
 }
  # Generar valores NDVI con patrones espaciales y ruido
  values(r) <- base_ndvi +
    noise * cos(3 * coordinates(r)[,1]) * sin(2 * coordinates(r)[,2]) +
    rnorm(ncell(r), 0, 0.1)
  # Asegurar que NDVI esté entre -1 y 1
  values(r) <- pmin(pmax(values(r), -1), 1)
  
  # Convertir a data.frame para ggplot
  ndvi_df <- as.data.frame(r, xy =TRUE)
  names(ndvi_df)[3] <- "ndvi"
  ndvi_df$year <- year
  
  return(ndvi_df)
  }
# Generar datos para los tres años
ndvi_data <- map_dfr(years,generate_ndvi)

# Visualización estática del NDVI
ggplot(ndvi_data, aes(x = x, y = y, fill = ndvi)) +
  geom_raster() +
  scale_fill_gradientn(colors = c("brown","yellow","darkgreen"),
                       limits = c(-1, 1),
                       name = "NDVI") +
  facet_wrap(~year, nrow = 1) +
  labs(title = "NDVI simulado en Chihuahua",
       subtitle = "Simulación de la cobertura de Bosque Antiguo",
       x = "Longitud", y = "Latitud") +
  theme_minimal()+
  coord_fixed()

# 2. Simulación de datos de ploblación de cotorras serranas

# Crear datos de población simulados que correlacionen con NDVI
set.seed(456)

# Función para generar datos de población basados en NDVI promedio
generate_birds <- function(ndvi_data) {
  ndvi_data %>%
    group_by(year) %>%
    summarise(mean_ndvi = mean(ndvi, na.rm = TRUE)) %>%
    mutate(
      # Población base correlacionada con NDVI
      population = 5000 * (mean_ndvi / 0.6) + rnorm(n(), 0, 200),
      population = round(pmax(population, 1000)), # Mínimo de 1000 individuos
      year = as.integer(year)
    )
}

bird_data <- generate_birds(ndvi_data)

# Gráfico estático de la población
ggplot(bird_data, aes(x = year, y = population)) +
  geom_line(color = "blue", size = 1.5) +
  geom_point(color = "red", size =3) +
  labs(title = "Población simulada de cotorras serranas",
       subtitle = "Relación con Bosque Antiguo (NDVI)",
       x = "Año", y = "Población estimada") +
  theme_minimal()

# 3. Visualización animada combinada

# Primero, crear un gráfico combinado
combined_data <-  ndvi_data %>%
  group_by(year) %>%
  mutate(mean_ndvi = mean(ndvi)) %>%
    left_join(bird_data, by = "year")

# Gráfico de NDVI con población como inseto
main_plot <- ggplot(combined_data, aes(x = x, y = y)) +
  geom_raster(aes(fill = ndvi)) +
  scale_fill_gradientn(colors = c ("brown", "yellow","darkgreen"),
                       limits = c(-1 , 1),
                       name = "NDVI") +
  labs(title = "Relación entre NDVI y Población de cotorras serranas",
       subtitle = "Año:{frame_time}",
       x = "Longitud", y = "Latitud") +
  theme_minimal() +
  coord_fixed() +
  theme(legent.position = "bottom")


# Añadir el inseto con la poblacion
final_plot <- main_plot +
  geom_line(data = bird_data,
            aes(x = -107, y = 30.5,
                group = 1,
                size = population / 1000),
            color = "blue", show.legend = FALSE) +
  geom_point(data = bird_data,
             aes(x = -107, y = 30.5,
                 size = population / 500),
             color = "red", show.legend = FALSE) +
  annotate("text", x = -107, y = 30.7,
           label = "Población de cotorras",
           size =3, color = "black") +
  scale_size_identity() +
  transition_time(year) +
  ease_aes('linear')

# Asegurar que el año sea numerico en todos los datos
bird_data <- bird_data %>%
  mutate(year = as.numeric(year))

ndvi_data <- ndvi_data %>%
  mutate(year = as.numeric(year))

# modificar la transicion en final_plot
final_plot <- ggplot(combined_data, aes(x = x, y = y)) +
  geom_raster(aes(fill = ndvi)) +
  scale_fill_gradientn(colors = c("brown", "yellow","darkgreen"),
                       limits = c(-1, 1),
                       name = "NDVI") +
  labs(title = "Relación entre NDVI y Población de cotorras serranas",
       subtitle = "Año: {round(frame_time)}", # Asegurar número entero
       x = "longitud", y = "Latitud") +
  theme_minimal() +
  coord_fixed() +
  theme(legend.position = "bottom") +
  
  # Capa de población de aves
  geom_line(data = bird_data, 
            aes(x = -107, y = 30.5, 
                group = 1), 
            color = "blue", size = 1.5, show.legend = FALSE) +
  geom_point(data = bird_data, 
             aes(x = -107, y = 30.5, 
                 size = population/1000), 
             color = "red", show.legend = FALSE) +
  annotate("text", x = -107, y = 30.7, 
           label = "Población de cotorras (miles)", 
           size = 3, color = "black") +
  
  # Transición modificada
  transition_time(year) +
  ease_aes('linear')

# 3. Generar la animacion con parametros ajustados
anim <- animate(
  final_plot,
  nframes = length(unique(combined_data$year)) *10, # 10 frames por año
  fps = 5, # Velocidad más lenta para mejor visualización
  width = 800,
  height = 600,
  renderer = gifski_renderer(),
  duration = 10 # 10 segundo en total
)

# mostrar animacion
anim

# Guardar la animacion como GIF
gganimate::anim_save(
  filename = "ndvi_cotorras_chihuahua_mejorado.gif", # nombre del archivo
  animation = anim,
  path = "C:/CGRF/CotorraSerrana",
  width = 800,
  height = 600,
  fps = 10,
  renderer = gganimate::gifski_renderer()
)

# Guardar como video MP4 (REQUIERE el paquete av)
gganimate::anim_save("videocotorras.mp4", anim, reder = gganimate::av_renderer())




