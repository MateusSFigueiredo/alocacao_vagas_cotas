# ==============================================================================
# Arquivo: est_desc_08_ordena_cursos.R
#
# Modificado em: 2024-03-31
# Autor: Mateus Silva Figueiredo

# Ordenar cursos por preenchimento extra de pub bxa ppi pcd 

# ESCREVENDO
# ==============================================================================
# Preparação
library(dplyr) # talvez desnecessario
library(tidyr)
library(ggplot2)

getwd()
list.files("produtos")

# Carregar dados
vagas_c2 <- read.csv2("produtos/Compila vagas ord por curso c2.csv")
vagas_c3 <- read.csv2("produtos/Compila vagas ord por curso c3.csv")
vagas_c4 <- read.csv2("produtos/Compila vagas ord por curso c4.csv")
vagas_c5 <- read.csv2("produtos/Compila vagas ord por curso c5.csv")

# Remover (n=40) etc. entre parêntesis
vagas_c2$conjunto <- gsub("\\(.*?\\)", "", vagas_c2$conjunto)
vagas_c3$conjunto <- gsub("\\(.*?\\)", "", vagas_c3$conjunto)
vagas_c4$conjunto <- gsub("\\(.*?\\)", "", vagas_c4$conjunto)
vagas_c5$conjunto <- gsub("\\(.*?\\)", "", vagas_c5$conjunto)

# Remover SISU
vagas_c2$conjunto <- gsub("SISU", "", vagas_c2$conjunto)
vagas_c3$conjunto <- gsub("SISU", "", vagas_c3$conjunto)
vagas_c4$conjunto <- gsub("SISU", "", vagas_c4$conjunto)
vagas_c5$conjunto <- gsub("SISU", "", vagas_c5$conjunto)

# Cria coluna soma
vagas_c2$soma <- sum(vagas_c2$pub,vagas_c2$ppi,vagas_c2$bxa)
vagas_c3$soma <- sum(vagas_c3$pub,vagas_c3$ppi,vagas_c3$bxa)
vagas_c4$soma <- sum(vagas_c4$pub,vagas_c4$ppi,vagas_c4$bxa)
vagas_c5$soma <- sum(vagas_c5$pub,vagas_c5$ppi,vagas_c5$bxa)

# ==============================================================================
# omitir linhas NA de c5
# talvez não precise
vagas_c5 <- na.omit(vagas_c5)

# ==============================================================================

# Sample data
# data <- vagas_c2
concorrencia <- "c3"
eval(parse(text=(paste0("data <- vagas_",concorrencia))))
data

# Reshape data into long format
data_long <- pivot_longer(data, cols = c(pub, bxa, ppi, pcd, soma), names_to = "type", values_to = "value")

# Reorder the factor levels based on "soma" values
data_long$conjunto <- factor(data_long$conjunto, levels = data[order(data$soma), "conjunto"])

# Remove coluna soma
data_long <- data_long[data_long$type != "soma", ]

# Título
titulo <- paste0("Ingressantes a mais sob ",concorrencia); titulo

# Create scatter plot - conjunto on y-axis - 4 types without jitter
grafico <- ggplot(data_long, aes(x = value, y = conjunto, color = type)) +
  geom_point(size = 1.5) +  # Set point size to 1.5
  labs(title = titulo, x = "Vagas", y = "Conjunto", color = "Grupo") +
  theme(axis.text.y = element_text(size = 5)); grafico

  # ------------------------------------------------------------------------------
  # Exportar gráfico
  if(T){
    tempo_atual <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S"); texto_hora<-paste0("Documento gerado em ",format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    
    ggsave(
      filename = paste0("graf_ordena_cursos_",concorrencia,"_",tempo_atual,".png"),
      plot = grafico,
      path = NULL,
      scale = 1,
      width = 7,
      height = 6*0.75,
      dpi = 300,
      limitsize = TRUE,
      bg = NULL)
    
    print("imagem está salva")
  }

# ==============================================================================
# # Rascunho abaixo
# 
# # Carrega data a partir de vagas_c2 até c5
# data <- vagas_c2
# 
# # Altera posição pra ficar visivel os pontos
# data$pub <- data$pub+0.15
# data$bxa <- data$bxa+0.075
# data$ppi <- data$ppi-0.075
# data$pcd <- data$pcd-0.15
# 
# # Alternativa 2
# data$pub <- data$pub+0.2
# data$bxa <- data$bxa+0.0
# data$ppi <- data$ppi-0.2
# data$pcd <- data$pcd-0.4
# 
# 
# # Reshape data into long format
# data_long <- pivot_longer(data, cols = c(pub, bxa, ppi, pcd), names_to = "type", values_to = "value")
# # data_long <- pivot_longer(data, cols = c(pub, pcd, ppi, bxa), names_to = "type", values_to = "value")
# 
# 
# # Reorder the factor levels based on "pub" values
# data$conjunto <- factor(data$conjunto, levels = data[order(data$pub), "conjunto"])
# 
# # Create scatter plot - conjunto on y-axis - 4 types without overlapping points
# ggplot(data_long, aes(x = value, y = conjunto, color = type)) +
#   geom_point(size = 1.5) +  # Add points
#   labs(title = "Scatter Plot", x = "Value", y = "Conjunto") +
#   theme(axis.text.y = element_text(size = 5))
# 
# 
# 
# # --- outros graficos
# 
# # ====
# 
# 
# 
# # ====
# 
#   # Create bar plot - conjunto no x
# ggplot(data, aes(y = pub, x = conjunto)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Bar Plot", x = "Conjunto", y = "Value") +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1, size=5)  # Rotate x-axis labels by 45 degrees
#   )
# 
# # Create bar plot - conjunto no y
# ggplot(data, aes(x = pub, y = conjunto)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Bar Plot", x = "Category", y = "Value") +
#   theme(axis.text.y = element_text(size=5)
#   )
# 
# # Create bar plot - conjunto on y-axis - 4 tipos
# ggplot(data_long, aes(x = value, y = conjunto, fill = type)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Bar Plot", x = "Value", y = "Conjunto") +
#   theme(axis.text.y = element_text(size = 5))
# 
# # Create scatter plot - conjunto on y-axis - 4 tipos
# ggplot(data_long, aes(x = value, y = conjunto, color = type)) +
#   geom_point(size = 2) +
#   labs(title = "Scatter Plot", x = "Value", y = "Conjunto") +
#   theme(axis.text.y = element_text(size = 5))


# ==============================================================================
# Referências

# Draw Stacked Bars within Grouped Barplot in R 
# https://statisticsglobe.com/draw-stacked-bars-within-grouped-barplot-r

# Save a ggplot (or other grid object) with sensible defaults
# https://ggplot2.tidyverse.org/reference/ggsave.html
