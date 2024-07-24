# ==============================================================================
# Arquivo: est_desc_08_ordena_cursos.R
#
# Modificado em: 2024-07-24
# Autor: Mateus Silva Figueiredo

# Ordenar cursos por preenchimento extra de pub bxa ppi pcd 

# ESCREVENDO
# ==============================================================================
# Preparação
library(dplyr) # talvez desnecessario
library(tidyr)
library(ggplot2)
setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")

# getwd()
list.files("produtos")

# Carregar dados
vagas_c2 <- read.csv2("produtos/Compila vagas ord por curso c2.csv")
vagas_c3 <- read.csv2("produtos/Compila vagas ord por curso c3.csv")
vagas_c4 <- read.csv2("produtos/Compila vagas ord por curso c4.csv")
vagas_c5 <- read.csv2("produtos/Compila vagas ord por curso c5.csv")

# omitir linhas NA de c5
vagas_c5 <- na.omit(vagas_c5)

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

# Sample data
# data <- vagas_c2
concorrencia <- "c4" # mudar de acordo com grafico desejado

{ # produzir e salvar gráfico

eval(parse(text=(paste0("data <- vagas_",concorrencia))))
data

# Reshape data into long format
data_long <- pivot_longer(data, cols = c(pub, bxa, ppi, pcd, soma), names_to = "type", values_to = "value")

# Reorder the factor levels based on "soma" values in the opposite order
data_long$conjunto <- factor(data_long$conjunto, levels = rev(data[order(data$soma), "conjunto"]))

# Remove coluna soma
data_long <- data_long[data_long$type != "soma", ]

# Set the order of 'type' factor levels
data_long$type <- factor(data_long$type, levels = c("pcd", "ppi", "bxa", "pub"))
# data_long$type <- factor(data_long$type, levels = c("pub", "bxa", "ppi", "pcd"))

# Substituir concorrencia pelo nome descritivo
if (concorrencia == "c2") {(concorrencia <- "CC-A")}
if (concorrencia == "c3") {(concorrencia <- "CC-C")}
if (concorrencia == "c4") {(concorrencia <- "CC-CT")}
if (concorrencia == "c5") {(concorrencia <- "CC-AT")}

# Título
titulo <- paste0("Ingressantes a mais sob ",concorrencia); titulo

# Create bar plot - conjunto on y-axis - 4 tipos
grafico <- ggplot(data_long, aes(x = value, y = conjunto, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = titulo, x = "Vagas", y = "Conjunto") +
  # by 2 = CC-C e CC-CT
  scale_x_continuous(breaks = seq(0, max(data_long$value), by = 2)) + # grid line by 2
  # by 4 = CC-A e CC-AT
#  scale_x_continuous(breaks = seq(0, max(data_long$value), by = 4)) + # grid line by 4
  theme(axis.text.y = element_text(size = 10)) +
  labs(fill = "Grupo social")

grafico

  # ------------------------------------------------------------------------------
  # Exportar gráfico
  if(T){
    tempo_atual <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S"); texto_hora<-paste0("Documento gerado em ",format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    
    ggsave(
      filename = paste0("graf_ordena_cursos_",concorrencia,"_",tempo_atual,".png"),
      plot = grafico,
      path = NULL,
      scale = 1,
      width = 8.27,
      height = 11.69, # para CC-C, CC-CT-, CC-A
#      height = 8.69,   # para CC-AT
      dpi = 300,
      limitsize = TRUE,
      bg = NULL)
    
    
    print("imagem está salva")
  }

} # fim de produzir e salvar gráfico

# ==============================================================================
# # Rascunho abaixo

# ---
# Versão anterior do grafico:

# Create scatter plot - conjunto on y-axis - 4 types without jitter
# grafico <- ggplot(data_long, aes(x = value, y = conjunto, color = type)) +
#   geom_point(size = 1.5) +  # Set point size to 1.5
#   labs(title = titulo, x = "Vagas", y = "Conjunto", color = "Grupo") +
#   theme(axis.text.y = element_text(size = 5)); grafico

# ---

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
