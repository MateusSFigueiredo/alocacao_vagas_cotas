# ==============================================================================
# Arquivo: est_desc_06_1_nota_min_c234.R
#
# Modificado em: 2024-07-22
# Autor: Mateus Silva Figueiredo

# diff: mudar nome de c2 c3 c4 c5 para CC-A CC-C CC-CT CC-AT

# Cria gráfico comparando CC-A CC-C CC-CT CC-AT
# expondo variação de nota média para geral, A0 e cotas
# Em quantos conjuntos aumentou, manteve igual ou diminuiu
# ==============================================================================
# Preparação
library(dplyr)
setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")

# definir limite entre "diminuiu pouco" e "diminuiu muito"
muito <- -5

# se F (padrão): script analysis_03_2 não gera novo documento excel
quero_imprimir_03_2 <- F

# se não tiver todado analysis_03_2, rodar
if (!exists("compila_concs_dif_min_c2")) source("analysis_03_2_compila_conc_notas.R")

# ==============================================================================
# Cria dataframe para receber valores
# coluna facet com as sistemáticas c2, c3, c4
# coluna grupo com geral, A0 e cotas
# coluna efeito com aumentou, igual, diminuiu
# valores a serem preenchidos com base em compila_concs_

data <- data.frame(facet = rep(c("c2","c3","c4","c5"), each = 12),
                   grupo = rep(c("geral", "A0","cotas"),each = 4),
                   efeito = c("aumentou","igual","diminuiu pouco","diminuiu muito"),
                   n_conjuntos = NA)
#                    n_conjuntos = round(abs(rnorm(27)), 2)) # dados fictíticos

data
# ==============================================================================
# omitir linhas NA de c5
# se a coluna "geral" for NA, remover
# objetivo: manter linhas que tem NaN em subcota L13

compila_concs_dif_min_c5 <- subset(compila_concs_dif_min_c5, !is.na(geral))
compila_concs_dif_min_c5 <- subset(compila_concs_dif_min_c5, !is.na(geral))

# ==============================================================================
# Preencher valores

# -----------
# Preenchendo com ChatGPT

# --------------------------------------------------
# ----- aumentou
# para cada linha
# quando for c2, c3, c4; efeito aumentou
# preencher com compila_concs_dif_min c2 c3 ou c4 número de conjuntos maior que 0
# grupo = geral
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4","c5") & grupo == "geral" & efeito == "aumentou" ~
        sum(get(paste0("compila_concs_dif_min_", facet))$geral > 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ----
# grupo = A0
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4","c5") & grupo == "A0" & efeito == "aumentou" ~
        sum(get(paste0("compila_concs_dif_min_", facet))$A0 > 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ---
# grupo = cotas
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4","c5") & grupo == "cotas" & efeito == "aumentou" ~
        sum(get(paste0("compila_concs_dif_min_", facet))$cotas > 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# --------------------------------------------------
# ----- igual
# para cada linha
# quando for c2, c3, c4; efeito igual
# preencher com compila_concs_dif_min c2 c3 ou c4 número de conjuntos maior que 0
# grupo = geral
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4","c5") & grupo == "geral" & efeito == "igual" ~
        sum(get(paste0("compila_concs_dif_min_", facet))$geral == 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ----
# grupo = A0
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4","c5") & grupo == "A0" & efeito == "igual" ~
        sum(get(paste0("compila_concs_dif_min_", facet))$A0 == 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ---
# grupo = cotas
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4","c5") & grupo == "cotas" & efeito == "igual" ~
        sum(get(paste0("compila_concs_dif_min_", facet))$cotas == 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# --------------------------------------------------
# ----- diminuiu pouco
# para cada linha
# quando for c2, c3, c4; efeito diminuiu pouco
# preencher com compila_concs_dif_min c2 c3 ou c4 número de conjuntos entre 0 e -10
# grupo = geral
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4","c5") & grupo == "geral" & efeito == "diminuiu pouco" ~
        sum(get(paste0("compila_concs_dif_min_", facet))$geral > muito 
            & get(paste0("compila_concs_dif_min_", facet))$geral < 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ----
# grupo = A0
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4","c5") & grupo == "A0" & efeito == "diminuiu pouco" ~
        sum(get(paste0("compila_concs_dif_min_", facet))$A0 > muito & 
              get(paste0("compila_concs_dif_min_", facet))$A0 < 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ---
# grupo = cotas
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4","c5") & grupo == "cotas" & efeito == "diminuiu pouco" ~
        sum(get(paste0("compila_concs_dif_min_", facet))$cotas > muito & 
              get(paste0("compila_concs_dif_min_", facet))$cotas < 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# --------------------------------------------------
# ----- diminuiu muito
# para cada linha
# quando for c2, c3, c4; efeito diminuiu muito
# preencher com compila_concs_dif_min c2 c3 ou c4 número de conjuntos entre 0 e -10
# grupo = geral
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4","c5") & grupo == "geral" & efeito == "diminuiu muito" ~
        sum(get(paste0("compila_concs_dif_min_", facet))$geral < muito),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ----
# grupo = A0
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4","c5") & grupo == "A0" & efeito == "diminuiu muito" ~
        sum(get(paste0("compila_concs_dif_min_", facet))$A0 < muito),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ----
# grupo = cotas
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4","c5") & grupo == "cotas" & efeito == "diminuiu muito" ~
        sum(get(paste0("compila_concs_dif_min_", facet))$cotas < muito),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ---
data
print ("data está preenchida")

# ==============================================================================
# Cria gráfico com base em data

# Mudar de c2 c3 c4 c5 para CC-A CC-C CC-CT CC-AT
data
# data$facet
data <- data %>%  mutate(facet = ifelse(facet == "c2", "CC-A", facet))
data <- data %>%  mutate(facet = ifelse(facet == "c3", "CC-C", facet))
data <- data %>%  mutate(facet = ifelse(facet == "c4", "CC-CT", facet))
data <- data %>%  mutate(facet = ifelse(facet == "c5", "CC-AT", facet))
# data

# Reorder the levels of the 'facet' factor
data$facet <- factor(data$facet, levels = c("CC-C","CC-CT","CC-A","CC-AT"))

# Reorder the levels of the 'efeito' factor
data$efeito <- factor(data$efeito,
                      levels = c("aumentou", "igual",
                                 "diminuiu pouco","diminuiu muito"))

# Reorder the levels of the 'grupo' factor
data$grupo <- factor(data$grupo, levels = c("geral", "A0", "cotas"))

# Specify colors for each level
colors <- c("aumentou" = "#00BA38", "igual" = "#619CFF", "diminuiu pouco" = "#F8766D",
            "diminuiu muito"="red")

# Labels na legenda lateral
## informando o valor entre "diminuiu pouco" e "diminuiu muito" com base em 'muito'
efeitos <- c("aumentou","igual",
             paste("diminuiu <", abs(muito)),paste("diminuiu >", abs(muito)))
## Com termos genéricos "diminuiu pouco" e "diminuiu muito
# efeitos <- c("aumentou","igual","diminuiu pouco","diminuiu muito")

# Plotting
grafico_c2345_min <- ggplot(data, aes(x = grupo, y = n_conjuntos, fill = efeito)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = colors, labels = efeitos) +  # Assign colors manually
  facet_grid(~ facet) +
  ggtitle("Efeito da sistemática na nota mínima por grupo") +
  ylab("Número de conjuntos") +
  xlab("Modalidade de convocação") +
  labs(fill = "Efeito na
nota mínima")

grafico_c2345_min

# ------------------------------------------------------------------------------
# Exportar gráfico
if(T){
  tempo_atual <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S"); texto_hora<-paste0("Documento gerado em ",format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  
  ggsave(
    filename = paste0("grafico_c2345_min_",tempo_atual,".png"),
    plot = grafico_c2345_min,
    path = NULL,
    scale = 1,
    width = 6,
    height = 6*0.75,
    dpi = 300,
    limitsize = TRUE,
    bg = NULL)
  
  print("imagem está salva")
}
# ==============================================================================
# Referências

# Draw Stacked Bars within Grouped Barplot in R 
# https://statisticsglobe.com/draw-stacked-bars-within-grouped-barplot-r

# Save a ggplot (or other grid object) with sensible defaults
# https://ggplot2.tidyverse.org/reference/ggsave.html

