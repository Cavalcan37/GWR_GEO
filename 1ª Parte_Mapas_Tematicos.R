# 1ª PARTE: Análise Prévia - Mapas Temáticos do Recife
# Combinando Excel + Shapefile 'geobr'
# 1. Carregar Pacotes
if(!require(dplyr))install.packages("dplyr")
library(dplyr)
if(!require(ggplot2))install.packages("ggplot2")
library(ggplot2)
if(!require(corrplot))install.packages("corrplot")
library(corrplot)
if(!require(PerformanceAnalytics))install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
if(!require(ggthemes))install.packages("ggthemes")
library(ggthemes)
if(!require(readxl))install.packages("readxl")
library(readxl)
if(!require(sf))install.packages("sf")
library(sf)
if(!require(spgwr))install.packages("spgwr")
library(spgwr)
if(!require(tmap))install.packages("tmap")
library(tmap)
if(!require(geobr))install.packages("geobr")
library(geobr)
if(!require(ggspatial))install.packages("ggspatial")
library(ggspatial)
##
if(!require(plyr))install.packages("plyr")
library(plyr) # Interfere no 'dplyr'
##
# Planilha de Dados:

# 2. Ler os dados do Excel (valores de arrendamento por rua)
dados_bairros <- read_excel("RECIFE_400.xlsx", sheet = 4)
dados_bairros

# 3. Agrupar por bairro
dados_bairros <- dados_bairros %>%
  group_by(BAIRRO) %>%
  summarise(COD_M = mean(COD,na.rm = T), n=n(), VALOR_M = mean(VALOR, na.rm = T),
            AREA_M = mean(AREA, na.rm = T), M2_M = mean(M2, na.rm = T), WC_M = mean(WC,na.rm = T),
            VAGA_M = mean(VAGA, na.rm = T)) %>%
  arrange(desc(n))
dados_bairros # Vamos agora adicionar as poligonais dos bairros para poder plotar a planilha num mapa

# 4. Shapefile dos bairros do Recife e Poligonal
# 4.1. Carregar a geometria do município de Recife - Pacote 'geobr'
recife_sf <- read_municipality(code_muni = 2611606, year = 2022)
# 4.2. Carregar a geometria de todos os bairros do Brasil (ano 2010)
bairros_recife <- read_neighborhood(year = 2010)
# 4.3. Filtrar para os bairros do município de Recife
bairros_recife <- subset(bairros_recife, code_muni == 2611606)
head(bairros_recife)
View(bairros_recife)

# 5. Verificar e corrigir estruturas e fazer o 'left join' entre 'dados_bairros' e 'bairros_recife'
# 5.1. Vefificar e fazer possíveis correções nas estruturas dos arquivos
str(dados_bairros) # dados da planilha
str(bairros_recife) # dados do geobr
# Como vamos unir o 'dados_bairros' (excel) e 'bairros_recife' (shape do 'geobr') pela 
# coluna comum 'COD_M' e 'code_neighborhood', elas têm que estar na mesma estrutura (para o caso até estão)

bairros_recife$code_neighborhood <- as.numeric(bairros_recife$code_neighborhood)
str(bairros_recife) 
# Aqui convertendo 'code_neighborhood' para numérico.

# 5.2. Agora sim, vamos para a junção
dados_bairros <- bairros_recife %>%
  left_join(dados_bairros, by = c("code_neighborhood" = "COD_M"))
head(dados_bairros)

# 6. Plotagem de Mapas Temáticos
# 6.1. Mapa da distribuição espacial do valor médio do aluguel (R$/unid)
library(ggspatial)

ggplot() +
  geom_sf(data = dados_bairros, aes(fill = VALOR_M))+
  scale_fill_distiller(palette = "Reds", direction = 1, name = "R$/imóvel",
                       limits = c())+ # relaciona-se à distribuição de cores
  # labs(title = "Aluguel Médio (R$/imóvel)")+
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2) +
  theme_bw()

# 6.2. Mapa da distribuição espacial do valor médio do m² (R$/m²)
ggplot() +
  geom_sf(data = dados_bairros, aes(fill = M2_M))+
  scale_fill_distiller(palette = "Blues", direction = 1, name = "R$/m²",
                       limits = c())+ # relaciona-se à distribuição de cores
  # labs(title = "Aluguel Médio (R$/m²)")+
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2) +
  theme_bw()

# 6.3. Mapa da distribuição espacial dos tamanhos médios dos imóveis (m²)
ggplot() +
  geom_sf(data = dados_bairros, aes(fill = AREA_M))+
  scale_fill_distiller(palette = "Greens", direction = 1, name = "Área (m²)",
                       limits = c())+ # relaciona-se à distribuição de cores
  # labs(title = "Tamanhos Médios (m²)")+
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2) +
  theme_bw()

# 6.4. Mapa da distribuição espacial das vagas
ggplot() +
  geom_sf(data = dados_bairros, aes(fill = VAGA_M))+
  scale_fill_distiller(palette = "Oranges", direction = 1, name = "Vagas",
                       limits = c())+ # relaciona-se à distribuição de cores
  # labs(title = "Nº Médio de Vagas")+
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2) +
  theme_bw()

# 6.5. Mapa da distribuição espacial dos WC
ggplot() +
  geom_sf(data = dados_bairros, aes(fill = WC_M))+
  scale_fill_distiller(palette = "Purples", direction = 1, name = "WC",
                       limits = c())+ # relaciona-se à distribuição de cores
  # labs(title = "Nº Médio de WC")+
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2) +
  theme_bw()

# 7. Juntando os gráficos
# Pacotes Extras (para ladear gráficos ou um em baixo do outro)
if(!require(patchwork))install.packages("patchwork")
library(patchwork)
if(!require(magrittr))install.packages("magrittr")
library(magrittr)
G_valor <- ggplot() +
  geom_sf(data = dados_bairros, aes(fill = VALOR_M))+
  scale_fill_distiller(palette = "Reds", direction = 1, name = "R$/imóvel.",
                       limits = c())+
  # labs(title = "Aluguel Médio (R$/imóvel)")+
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2) +
  theme_bw()
G_valor
G_m2 <- ggplot() +
  geom_sf(data = dados_bairros, aes(fill = M2_M))+
  scale_fill_distiller(palette = "Reds", direction = 1, name = "R$/m²",
                       limits = c())+
  # labs(title = "Aluguel Médio (R$/m²)")+
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2) +
  theme_bw()
G_m2
G_area <- ggplot() +
  geom_sf(data = dados_bairros, aes(fill = AREA_M))+
  scale_fill_distiller(palette = "Greens", direction = 1, name = "Área (m²)",
                       limits = c())+
  # labs(title = "Tamanhos Médios (m²)")+
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2) +
  theme_bw()
G_area
G_vaga <- ggplot() +
  geom_sf(data = dados_bairros, aes(fill = VAGA_M))+
  scale_fill_distiller(palette = "Oranges", direction = 1, name = "Vagas",
                       limits = c())+
  # labs(title = "Nº Médio de Vagas")+
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2) +
  theme_bw()
G_vaga
G_wc <- ggplot() +
  geom_sf(data = dados_bairros, aes(fill = WC_M))+
  scale_fill_distiller(palette = "Purples", direction = 1, name = "WC",
                       limits = c())+
  # labs(title = "Nº Médio de WC")+
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2) +
  theme_bw()
G_wc
# Combinando os gráficos num único arcabouço
(G_vaga + G_wc)/(G_area + G_valor)











