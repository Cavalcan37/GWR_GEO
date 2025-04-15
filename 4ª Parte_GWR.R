# 4ª PARTE: Análise Geoestatística da Previsão de Valores para Arrendamentos de Espaços Comerciais em Recife
# REGRESSÃO ESPACIAL GWR (PONTOS)

# 1. Carregar Pacotes
if(!require(dplyr))install.packages("dplyr")
library(dplyr)
if(!require(ggplot2))install.packages("ggplot2")
library(ggplot2)
if(!require(openxlsx))install.packages("openxlsx")
library(openxlsx)
if(!require(readxl))install.packages("readxl")
library(readxl)
if(!require(ggthemes))install.packages("ggthemes")
library(ggthemes)
if(!require(sf))install.packages("sf")
library(sf)
if(!require(spgwr))install.packages("spgwr")
library(spgwr)
if(!require(tmap))install.packages("tmap")
library(tmap)
if(!require(geobr))install.packages("geobr")
library(geobr)

##
# Planilha de Dados:

# 2. Ler os dados do Excel (valores de arrendamento por rua)

dados_ruas <- read_excel("RECIFE_400.xlsx",
                         sheet = 3)
dados_ruas

# 3. Transformar os dados em um objeto espacial
# Certifique-se de que a planilha tenha colunas para latitude e longitude das ruas.
ruas_sf <- st_as_sf(dados_ruas, coords = c("LONG", "LAT"), crs = 4326)
str(ruas_sf) # aqui acusou que os preços não estão no formato

ruas_sf$VALOR <- as.numeric(ruas_sf$VALOR)
str(ruas_sf)
summary(ruas_sf)

ruas_sf <- na.omit(ruas_sf) # Elimina possíveis NA's
summary(ruas_sf)

# 4. Preparar os dados para a GWR
# Definir a fórmula da regressão (por exemplo, arrendamento ~ área + banheiros + vagas)
gwr_formula <- VALOR ~ AREA + WC + VAGA

# 5. Rodar a GWR com raio fixo
# Antes definir coordenadas como matriz
coordenadas_ruas <- as.matrix(st_coordinates(ruas_sf))
head(coordenadas_ruas)

# 5.1. Seleção da largura de banda (raio fixo) para a GWR
raioF_ruas <- gwr.sel(gwr_formula, data = as.data.frame(ruas_sf), coords = coordenadas_ruas)
raioF_ruas

# 5.2. Executar a regressão GWR com raio fixo
ruas_gwrf <- gwr(gwr_formula, data = as.data.frame(ruas_sf), coords = coordenadas_ruas, bandwidth = raioF_ruas)
head(ruas_gwrf$SDF@data)

# 5.3. Ver resultados e calcular o RMSE
summary(ruas_gwrf)
RMSE_F_ruas <- sqrt(mean(ruas_gwrf$SDF$gwr.e^2))
RMSE_F_ruas

# 6. Rodar a GWR com raio adaptativo/variável
# 6.1. Seleção da largura para o raio adaptativo/variável
raioV_ruas <- gwr.sel(gwr_formula, data = as.data.frame(ruas_sf), coords = coordenadas_ruas, adapt = TRUE)
raioV_ruas

# 6.2. Executar a GWR com raio adaptativo
ruas_gwrv <- gwr(gwr_formula, data = as.data.frame(ruas_sf), coords = coordenadas_ruas, adapt = raioV_ruas)
head(ruas_gwrv$SDF@data)

# 6.3. Ver resultados e calcular o RMSE para o modelo adaptativo
summary(ruas_gwrv)
RMSE_V_ruas <- sqrt(mean(ruas_gwrv$SDF$gwr.e^2))
RMSE_V_ruas

# 7. Mapear os resultados com GGPLOT2

# 7.1. Mapear 'R²' SEM os bairros
# Carregar os pacotes necessários
library(geobr)
library(ggplot2)
library(ggspatial) # Acoplar Seta Norte
library(sf) # Para lidar com dados espaciais
# Carregar a geometria do município de Recife
recife_sf <- read_municipality(code_muni = 2611606, year = 2022)
# Verificar se 'ruas_gwrv$SDF' é um objeto sf, caso contrário, convertê-lo
if (!inherits(ruas_gwrf$SDF, "sf")) {
  ruas_gwrf$SDF <- st_as_sf(ruas_gwrf$SDF)
}
# Verificar se o CRS está ausente e definir um CRS apropriado (EPSG:4326)
if (is.na(st_crs(ruas_gwrf$SDF))) {
  ruas_gwrf$SDF <- st_set_crs(ruas_gwrf$SDF, 4326) # Define o CRS como WGS 84
}
# Agora você pode continuar com o plot
ggplot() +
  # Mapa do município de Recife
  geom_sf(data = recife_sf, fill = "gray90", color = "black") +
  # Adicionar os pontos da análise GWR (ruas_gwrv$SDF)
  geom_sf(data = ruas_gwrf$SDF, aes(color = localR2), size = 2, alpha = 0.5) +
  # Paleta de cores para localR2
  scale_color_gradientn(colors = c("lightblue", "blue", "darkblue"),
                        name = expression(R^2)) +
  # Tema
  theme_bw() +
  # Adicionar título e ajustar layout
  # labs(title = "R² - GWR Porto do Recife") +
  # Adicionar seta de norte e escala
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2)

# 7.2. Mapear 'R²' COM os bairros
# Carregar os pacotes necessários
library(geobr)
library(ggplot2)
library(ggspatial) # Acoplar Seta Norte
library(sf) # Para lidar com dados espaciais
# Carregar a geometria do município de Recife
recife_sf <- read_municipality(code_muni = 2611606, year = 2022)
# Carregar a geometria dos bairros de Recife (ano 2010)
bairros_recife <- read_neighborhood(year = 2010)
# Filtrar para o município de Recife
bairros_recife <- subset(bairros_recife, code_muni == 2611606)
# Verificar se 'ruas_gwrv$SDF' é um objeto sf, caso contrário, convertê-lo
if (!inherits(ruas_gwrf$SDF, "sf")) {
  ruas_gwrf$SDF <- st_as_sf(ruas_gwrf$SDF)
}
# Verificar se o CRS está ausente e definir um CRS apropriado (EPSG:4326)
if (is.na(st_crs(ruas_gwrf$SDF))) {
  ruas_gwrf$SDF <- st_set_crs(ruas_gwrf$SDF, 4326) # Define o CRS como WGS 84
}
# Agora você pode continuar com o plot
ggplot() +
  # Mapa do município de Recife
  geom_sf(data = recife_sf, fill = "gray90", color = "black") +
  # Adicionar os bairros de Recife
  geom_sf(data = bairros_recife, fill = NA, color = "black", linetype = "solid", size = 0.5) +
  # Adicionar os pontos da análise GWR (ruas_gwrv$SDF)
  geom_sf(data = ruas_gwrf$SDF, aes(color = localR2), size = 2, alpha = 0.5) +
  # Paleta de cores para localR2
  scale_color_gradientn(colors = c("lightblue", "blue", "darkblue"),
                        name = expression(R^2)) +
  # Tema
  theme_bw() +
  # Adicionar título e ajustar layout
  # labs(title = "R² - GWR Porto do Recife") +
  # Adicionar seta de norte e escala
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2)

# 7.3. Mapear 'AREA' SEM os bairros
# Carregar os pacotes necessários
library(geobr)
library(ggplot2)
library(ggspatial) # Acoplar Seta Norte
library(sf) # Para lidar com dados espaciais
# Carregar a geometria do município de Recife
recife_sf <- read_municipality(code_muni = 2611606, year = 2022)
# Verificar se 'ruas_gwrv$SDF' é um objeto sf, caso contrário, convertê-lo
if (!inherits(ruas_gwrf$SDF, "sf")) {
  ruas_gwrf$SDF <- st_as_sf(ruas_gwrf$SDF)
}
# Verificar se o CRS está ausente e definir um CRS apropriado (EPSG:4326)
if (is.na(st_crs(ruas_gwrf$SDF))) {
  ruas_gwrf$SDF <- st_set_crs(ruas_gwrf$SDF, 4326) # Define o CRS como WGS 84
}
# Agora você pode continuar com o plot para a variável 'AREA'
ggplot() +
  # Mapa do município de Recife
  geom_sf(data = recife_sf, fill = "gray90", color = "black") +
  # Adicionar os pontos da análise GWR com base na variável 'AREA'
  geom_sf(data = ruas_gwrf$SDF, aes(color = AREA), size = 2, alpha = 0.5) +
  # Paleta de cores para a variável 'AREA'
  scale_color_gradientn(colors = c("yellow", "orange", "red"),
                        name = "Área (m²)") +
  # Tema minimalista
  theme_bw() +
  # Adicionar título e ajustar layout
  # labs(title = "Área - GWR Porto do Recife") +
  # Adicionar seta de norte e escala
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2)

# 7.4. Mapear 'AREA' COM os bairros
# Carregar os pacotes necessários
library(geobr)
library(ggplot2)
library(ggspatial) # Acoplar Seta Norte
library(sf) # Para lidar com dados espaciais
# Carregar a geometria do município de Recife
recife_sf <- read_municipality(code_muni = 2611606, year = 2022)
# Carregar a geometria dos bairros de Recife (ano 2010)
bairros_recife <- read_neighborhood(year = 2010)
# Filtrar para o município de Recife
bairros_recife <- subset(bairros_recife, code_muni == 2611606)
# Verificar se 'ruas_gwrv$SDF' é um objeto sf, caso contrário, convertê-lo
if (!inherits(ruas_gwrf$SDF, "sf")) {
  ruas_gwrf$SDF <- st_as_sf(ruas_gwrf$SDF)
}
# Verificar se o CRS está ausente e definir um CRS apropriado (EPSG:4326)
if (is.na(st_crs(ruas_gwrf$SDF))) {
  ruas_gwrf$SDF <- st_set_crs(ruas_gwrf$SDF, 4326) # Define o CRS como WGS 84
}
# Agora você pode continuar com o plot para a variável 'AREA'
ggplot() +
  # Mapa do município de Recife
  geom_sf(data = recife_sf, fill = "gray90", color = "black") +
  # Adicionar os bairros de Recife
  geom_sf(data = bairros_recife, fill = NA, color = "black", linetype = "solid", size = 0.5) +
  # Adicionar os pontos da análise GWR com base na variável 'AREA'
  geom_sf(data = ruas_gwrf$SDF, aes(color = AREA), size = 2, alpha = 0.5) +
  # Paleta de cores para a variável 'AREA'
  scale_color_gradientn(colors = c("yellow", "orange", "red"),
                        name = "Área (m²)") +
  # Tema minimalista
  theme_bw() +
  # Adicionar título e ajustar layout
  # labs(title = "Beta Área - Análise GWR") +
  # Adicionar seta de norte e escala
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2)

# 7.5. Mapear 'WC' SEM os bairros
# Carregar os pacotes necessários
library(geobr)
library(ggplot2)
library(ggspatial) # Acoplar Seta Norte
library(sf) # Para lidar com dados espaciais
# Carregar a geometria do município de Recife
recife_sf <- read_municipality(code_muni = 2611606, year = 2022)
# Verificar se 'ruas_gwrv$SDF' é um objeto sf, caso contrário, convertê-lo
if (!inherits(ruas_gwrf$SDF, "sf")) {
  ruas_gwrf$SDF <- st_as_sf(ruas_gwrf$SDF)
}
# Verificar se o CRS está ausente e definir um CRS apropriado (EPSG:4326)
if (is.na(st_crs(ruas_gwrf$SDF))) {
  ruas_gwrf$SDF <- st_set_crs(ruas_gwrf$SDF, 4326) # Define o CRS como WGS 84
}
# Agora você pode continuar com o plot para a variável 'WC'
ggplot() +
  # Mapa do município de Recife
  geom_sf(data = recife_sf, fill = "gray90", color = "black") +
  # Adicionar os pontos da análise GWR com base na variável 'WC'
  geom_sf(data = ruas_gwrf$SDF, aes(color = WC), size = 2, alpha = 0.5) +
  # Paleta de cores para a variável 'WC'
  scale_color_gradientn(colors = c("lightblue", "blue", "darkblue"),
                        name = "WC") +
  # Tema minimalista
  theme_bw() +
  # Adicionar título e ajustar layout
  # labs(title = "BETA WC - Análise GWR") +
  # Adicionar seta de norte e escala
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2)

# 7.6. Mapear 'WC' COM os bairros
# Carregar os pacotes necessários
library(geobr)
library(ggplot2)
library(ggspatial) # Acoplar Seta Norte
library(sf) # Para lidar com dados espaciais
# Carregar a geometria do município de Recife
recife_sf <- read_municipality(code_muni = 2611606, year = 2022)
# Carregar a geometria dos bairros de Recife (ano 2010)
bairros_recife <- read_neighborhood(year = 2010)
# Filtrar para o município de Recife
bairros_recife <- subset(bairros_recife, code_muni == 2611606)
# Verificar se 'ruas_gwrv$SDF' é um objeto sf, caso contrário, convertê-lo
if (!inherits(ruas_gwrf$SDF, "sf")) {
  ruas_gwrf$SDF <- st_as_sf(ruas_gwrf$SDF)
}
# Verificar se o CRS está ausente e definir um CRS apropriado (EPSG:4326)
if (is.na(st_crs(ruas_gwrf$SDF))) {
  ruas_gwrf$SDF <- st_set_crs(ruas_gwrf$SDF, 4326) # Define o CRS como WGS 84
}
# Agora você pode continuar com o plot para a variável 'WC'
ggplot() +
  # Mapa do município de Recife
  geom_sf(data = recife_sf, fill = "gray90", color = "black") +
  # Adicionar os bairros de Recife
  geom_sf(data = bairros_recife, fill = NA, color = "black", linetype = "solid", size = 0.5) +
  # Adicionar os pontos da análise GWR com base na variável 'WC'
  geom_sf(data = ruas_gwrf$SDF, aes(color = WC), size = 2, alpha = 0.5) +
  # Paleta de cores para a variável 'WC'
  scale_color_gradientn(colors = c("lightblue", "blue", "darkblue"),
                        name = "WC") +
  # Tema minimalista
  theme_bw() +
  # Adicionar título e ajustar layout
  # labs(title = " BETA WC - Análise GWR ") +
  # Adicionar seta de norte e escala
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2)

# 7.7. Mapear 'VAGA' SEM os bairros
# Carregar os pacotes necessários
library(geobr)
library(ggplot2)
library(ggspatial) # Acoplar Seta Norte
library(sf) # Para lidar com dados espaciais
# Carregar a geometria do município de Recife
recife_sf <- read_municipality(code_muni = 2611606, year = 2022)
# Verificar se 'ruas_gwrv$SDF' é um objeto sf, caso contrário, convertê-lo
if (!inherits(ruas_gwrf$SDF, "sf")) {
  ruas_gwrf$SDF <- st_as_sf(ruas_gwrf$SDF)
}
# Verificar se o CRS está ausente e definir um CRS apropriado (EPSG:4326)
if (is.na(st_crs(ruas_gwrf$SDF))) {
  ruas_gwrf$SDF <- st_set_crs(ruas_gwrf$SDF, 4326) # Define o CRS como WGS 84
}
# Agora você pode continuar com o plot para a variável 'WC'
ggplot() +
  # Mapa do município de Recife
  geom_sf(data = recife_sf, fill = "gray90", color = "black") +
  # Adicionar os pontos da análise GWR com base na variável 'WC'
  geom_sf(data = ruas_gwrf$SDF, aes(color = VAGA), size = 2, alpha = 0.5) +
  # Paleta de cores para a variável 'WC'
  scale_color_gradientn(colors = c("lightgreen", "green", "darkgreen"),
                        name = "Vagas") +
  # Tema minimalista
  theme_bw() +
  # Adicionar título e ajustar layout
  # labs(title = "BETA Vagas - Análise GWR") +
  # Adicionar seta de norte e escala
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2)

# 7.8. Mapear 'WC' COM os bairros
# Carregar os pacotes necessários
library(geobr)
library(ggplot2)
library(ggspatial) # Acoplar Seta Norte
library(sf) # Para lidar com dados espaciais
# Carregar a geometria do município de Recife
recife_sf <- read_municipality(code_muni = 2611606, year = 2022)
# Carregar a geometria dos bairros de Recife (ano 2010)
bairros_recife <- read_neighborhood(year = 2010)
# Filtrar para o município de Recife
bairros_recife <- subset(bairros_recife, code_muni == 2611606)
# Verificar se 'ruas_gwrv$SDF' é um objeto sf, caso contrário, convertê-lo
if (!inherits(ruas_gwrf$SDF, "sf")) {
  ruas_gwrf$SDF <- st_as_sf(ruas_gwrf$SDF)
}
# Verificar se o CRS está ausente e definir um CRS apropriado (EPSG:4326)
if (is.na(st_crs(ruas_gwrf$SDF))) {
  ruas_gwrf$SDF <- st_set_crs(ruas_gwrf$SDF, 4326) # Define o CRS como WGS 84
}
# Agora você pode continuar com o plot para a variável 'WC'
ggplot() +
  # Mapa do município de Recife
  geom_sf(data = recife_sf, fill = "gray90", color = "black") +
  # Adicionar os bairros de Recife
  geom_sf(data = bairros_recife, fill = NA, color = "black", linetype = "solid", size = 0.5) +
  # Adicionar os pontos da análise GWR com base na variável 'WC'
  geom_sf(data = ruas_gwrf$SDF, aes(color = VAGA), size = 2, alpha = 0.5) +
  # Paleta de cores para a variável 'WC'
  scale_color_gradientn(colors = c("lightgreen", "green", "darkgreen"),
                        name = "Vagas") +
  # Tema minimalista
  theme_bw() +
  # Adicionar título e ajustar layout
  # labs(title = " BETA Vagas - Análise GWR ") +
  # Adicionar seta de norte e escala
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2)

# 7.9. Mapear 'gwr.e' SEM os bairros
# Carregar os pacotes necessários
library(geobr)
library(ggplot2)
library(ggspatial) # Acoplar Seta Norte
library(sf) # Para lidar com dados espaciais
# Carregar a geometria do município de Recife
recife_sf <- read_municipality(code_muni = 2611606, year = 2022)
# Verificar se 'ruas_gwrv$SDF' é um objeto sf, caso contrário, convertê-lo
if (!inherits(ruas_gwrf$SDF, "sf")) {
  ruas_gwrf$SDF <- st_as_sf(ruas_gwrf$SDF)
}
# Verificar se o CRS está ausente e definir um CRS apropriado (EPSG:4326)
if (is.na(st_crs(ruas_gwrf$SDF))) {
  ruas_gwrf$SDF <- st_set_crs(ruas_gwrf$SDF, 4326) # Define o CRS como WGS 84
}
# Agora você pode continuar com o plot para a variável 'gwr.e'
ggplot() +
  # Mapa do município de Recife
  geom_sf(data = recife_sf, fill = "gray60", color = "black") +
  # Adicionar os pontos da análise GWR com base na variável 'gwr.e'
  geom_sf(data = ruas_gwrf$SDF, aes(color = gwr.e), size = 2, alpha = 0.5) +
  # Paleta de cores para a variável 'gwr.e', considerando valores negativos e positivos
  scale_color_gradient2(low = "red", mid = "white", high = "blue",
                        midpoint = 0, name = "Erro") +
  # Tema minimalista
  theme_bw() +
  # Adicionar título e ajustar layout
  # labs(title = "Erro de Previsão - Análise GWR") +
  # Adicionar seta de norte e escala
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2)

# 7.10. Mapear 'gwr.e' COM os bairros
# Carregar os pacotes necessários
library(geobr)
library(ggplot2)
library(ggspatial) # Acoplar Seta Norte
library(sf) # Para lidar com dados espaciais
# Carregar a geometria do município de Recife
recife_sf <- read_municipality(code_muni = 2611606, year = 2022)
# Carregar a geometria dos bairros de Recife (ano 2010)
bairros_recife <- read_neighborhood(year = 2010)
# Filtrar para o município de Recife
bairros_recife <- subset(bairros_recife, code_muni == 2611606)
# Verificar se 'ruas_gwrv$SDF' é um objeto sf, caso contrário, convertê-lo
if (!inherits(ruas_gwrf$SDF, "sf")) {
  ruas_gwrf$SDF <- st_as_sf(ruas_gwrf$SDF)
}
# Verificar se o CRS está ausente e definir um CRS apropriado (EPSG:4326)
if (is.na(st_crs(ruas_gwrf$SDF))) {
  ruas_gwrf$SDF <- st_set_crs(ruas_gwrf$SDF, 4326) # Define o CRS como WGS 84
}
# Agora você pode continuar com o plot para a variável 'gwr.e'
ggplot() +
  # Mapa do município de Recife
  geom_sf(data = recife_sf, fill = "gray60", color = "black") +
  # Adicionar os bairros de Recife
  geom_sf(data = bairros_recife, fill = NA, color = "black", linetype = "solid", size = 0.5) +
  # Adicionar os pontos da análise GWR com base na variável 'gwr.e'
  geom_sf(data = ruas_gwrf$SDF, aes(color = gwr.e), size = 2, alpha = 0.5) +
  # Paleta de cores para a variável 'gwr.e', considerando valores negativos e positivos
  scale_color_gradient2(low = "red", mid = "white", high = "blue",
                        midpoint = 0, name = "Erro") +
  # Tema minimalista
  theme_bw() +
  # Adicionar título e ajustar layout
  # labs(title = "Erro de Previsão - Análise GWR") +
  # Adicionar seta de norte e escala
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2)

# 8. Previsão (Recife Antigo)
# AREA = 250
# WC = 2
# VAGA = 5
# LOG = -34.8703833
# LAT = -8.0511000
# 8.1. Definir as coordenadas do ponto de interesse
ponto_interesse <- st_sf(st_sfc(st_point(c(-34.8703833, -8.0511000))), crs = 4326)

# 8.2. Encontrar o ponto mais próximo no conjunto 'ruas_gwrv'
distancias <- st_distance(ponto_interesse, st_as_sf(ruas_gwrf$SDF))
indice_mais_proximo <- which.min(distancias)

# 8.3. Extrair os coeficientes da regressão GWR para o ponto mais próximo
coef_intercepto <- ruas_gwrf$SDF$`(Intercept)`[indice_mais_proximo]
coef_area <- ruas_gwrf$SDF$AREA[indice_mais_proximo]
coef_wc <- ruas_gwrf$SDF$WC[indice_mais_proximo]
coef_vaga <- ruas_gwrf$SDF$VAGA[indice_mais_proximo]

# 8.4. Fazer a previsão com base nos valores fornecidos para 'AREA' e 'WC'
AREA_valor <- 473
WC_valor <- 5
VAGA_valor <- 10
PRECO_predito_rec <- coef_intercepto + coef_area * AREA_valor + coef_wc * WC_valor + coef_vaga*VAGA_valor
PRECO_predito_rec

# 9. Previsão (Imbiribeira)
# AREA = 250
# WC = 2
# VAGA = 5
# LOG = -34,9107750
# LAT = -8,1092611
# 9.1. Definir as coordenadas do ponto de interesse
ponto_interesse <- st_sf(st_sfc(st_point(c(-34.9107750, -8.1092611))), crs = 4326)
# 9.2. Encontrar o ponto mais próximo no conjunto 'ruas_gwrv'

distancias <- st_distance(ponto_interesse, st_as_sf(ruas_gwrf$SDF))
indice_mais_proximo <- which.min(distancias)

# 9.3. Extrair os coeficientes da regressão GWR para o ponto mais próximo
coef_intercepto <- ruas_gwrf$SDF$`(Intercept)`[indice_mais_proximo]
coef_area <- ruas_gwrf$SDF$AREA[indice_mais_proximo]
coef_wc <- ruas_gwrf$SDF$WC[indice_mais_proximo]
coef_vaga <- ruas_gwrf$SDF$VAGA[indice_mais_proximo]

# 9.4. Fazer a previsão com base nos valores fornecidos para 'AREA' e 'WC'
AREA_valor <- 473
WC_valor <- 5
VAGA_valor <- 10
PRECO_predito_imb <- coef_intercepto + coef_area * AREA_valor + coef_wc * WC_valor + coef_vaga*VAGA_valor
PRECO_predito_imb

# 10. Previsão (Várzea)
# AREA = 250
# WC = 2
# VAGA = 5
# LOG = -34,9597166
# LAT = -8,0484388
# 10.1. Definir as coordenadas do ponto de interesse
ponto_interesse <- st_sf(st_sfc(st_point(c(-34.9597166, -8.0484388))), crs = 4326)

# 10.2. Encontrar o ponto mais próximo no conjunto 'ruas_gwrv'
distancias <- st_distance(ponto_interesse, st_as_sf(ruas_gwrf$SDF))
indice_mais_proximo <- which.min(distancias)

# 10.3. Extrair os coeficientes da regressão GWR para o ponto mais próximo
coef_intercepto <- ruas_gwrf$SDF$`(Intercept)`[indice_mais_proximo]
coef_area <- ruas_gwrf$SDF$AREA[indice_mais_proximo]
coef_wc <- ruas_gwrf$SDF$WC[indice_mais_proximo]
coef_vaga <- ruas_gwrf$SDF$VAGA[indice_mais_proximo]

# 10.4. Fazer a previsão com base nos valores fornecidos para 'AREA' e 'WC'
AREA_valor <- 473
WC_valor <- 5
VAGA_valor <- 10
PRECO_predito_var <- coef_intercepto + coef_area * AREA_valor + coef_wc * WC_valor + coef_vaga*VAGA_valor
PRECO_predito_var

# 11. Mapas das Inferências
# 11.1. Carregar a geometria do município de Recife e Bairros
recife_sf <- read_municipality(code_muni = 2611606, year = 2022)
# Verificar se 'ruas_gwrv$SDF' é um objeto sf, caso contrário, convertê-lo
if (!inherits(ruas_gwrf$SDF, "sf")) {
  ruas_gwrf$SDF <- st_as_sf(ruas_gwrf$SDF)
}
# Verificar se o CRS está ausente e definir um CRS apropriado (EPSG:4326)
if (is.na(st_crs(ruas_gwrf$SDF))) {
  ruas_gwrf$SDF <- st_set_crs(ruas_gwrf$SDF, 4326) # Define o CRS como WGS 84
}

# 11.2. Criar um data frame com as previsões
previsoes <- data.frame(
  bairro = c("Recife Antigo", "Imbiribeira", "Várzea"),
  log = c(-34.8703833, -34.9107750, -34.9597166),
  lat = c(-8.0511000, -8.1092611, -8.0484388),
  preco_predito = c(PRECO_predito_rec, PRECO_predito_imb, PRECO_predito_var) # Use os valores previstos
)

# 11.3. Transformar o data frame em um objeto sf
previsoes_sf <- st_as_sf(previsoes, coords = c("log", "lat"), crs = 4326)

# 11.4. Plotar o mapa com as previsões
ggplot() +
  geom_sf(data = recife_sf, fill = "lightgray", color = "black") + # Mapa de Recife
  geom_sf(data = previsoes_sf, aes(size = preco_predito), shape = 21, fill = "red") + # Pontos das previsões
  geom_text(data = previsoes, aes(x = log, y = lat, label = round(preco_predito, 2)), 
            vjust = -1, color = "darkblue", size = 4) + # Adiciona os rótulos das previsões
  labs(size = "Preço Predito")+ # Renomeia a legenda de tamanho
  # Adicionar seta de norte e escala
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2)+
  theme_bw()

# 12. Mapas das Inferências c/ Bairros
# 12.1. Carregar a geometria do município de Recife e Bairros
recife_sf <- read_municipality(code_muni = 2611606, year = 2022)
# Carregar a geometria dos bairros de Recife (ano 2010)
bairros_recife <- read_neighborhood(year = 2010)
# Filtrar para o município de Recife
bairros_recife <- subset(bairros_recife, code_muni == 2611606)
# Verificar se 'ruas_gwrv$SDF' é um objeto sf, caso contrário, convertê-lo
if (!inherits(ruas_gwrf$SDF, "sf")) {
  ruas_gwrf$SDF <- st_as_sf(ruas_gwrf$SDF)
}
# Verificar se o CRS está ausente e definir um CRS apropriado (EPSG:4326)
if (is.na(st_crs(ruas_gwrf$SDF))) {
  ruas_gwrf$SDF <- st_set_crs(ruas_gwrf$SDF, 4326) # Define o CRS como WGS 84
}

# 12.2. Criar um data frame com as previsões
previsoes <- data.frame(
  bairro = c("Recife Antigo", "Imbiribeira", "Várzea"),
  log = c(-34.8703833, -34.9107750, -34.9597166),
  lat = c(-8.0511000, -8.1092611, -8.0484388),
  preco_predito = c(PRECO_predito_rec, PRECO_predito_imb, PRECO_predito_var) # Use os valores previstos
)

# 12.3. Transformar o data frame em um objeto sf
previsoes_sf <- st_as_sf(previsoes, coords = c("log", "lat"), crs = 4326)

# 12.4. Plotar o mapa com as previsões
ggplot() +
  geom_sf(data = recife_sf, fill = "lightgray", color = "black") + # Mapa de Recife
  geom_sf(data = bairros_recife, fill = NA, color = "black") + # Bairros de Recife
  geom_sf(data = previsoes_sf, aes(size = preco_predito), shape = 21, fill = "red") + # Pontos das previsões
  geom_text(data = previsoes, aes(x = log, y = lat, label = round(preco_predito, 2)), 
            vjust = -1, color = "darkblue", size = 4) + # Adiciona os rótulos das previsões
  labs(size = "Preço Predito")+ # Renomeia a legenda de tamanho
  # Adicionar seta de norte e escala
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2)+
  theme_bw()






