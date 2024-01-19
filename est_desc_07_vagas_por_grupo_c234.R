# ==============================================================================
# Arquivo: est_desc_07_vagas_por_grupo_c234.R
#
# Modificado em: 2024-01-19
# Autor: Mateus Silva Figueiredo

# Cria gráfico comparando c2, c3 e c4
# expondo variação de ingresso de pub, bxa, ppi, pcd
# Em quantos conjuntos aumentou, manteve igual ou diminuiu

# ESCREVENDO
# ==============================================================================
# Preparação
library(dplyr)

# se F (padrão): script analysis_02_3_compila_concs.R não gera novo documento
quero_imprimir <- F

# se não tiver todado analysis_02_3, rodar
if (!exists("compila_concs_c1")) source("analysis_02_3_compila_concs.R")

# ==============================================================================
# Para uma sistemática
sum(compila_concs_c2$tot==0) # 65 conjuntos

sum(compila_concs_c2$pub>0)  # 60 aumentou pub
sum(compila_concs_c2$pub==0) #  5 mantem pub
sum(compila_concs_c2$pub<0)  #  0 diminuiu pub

# ==============================================================================
# Cria dataframe para receber valores
# coluna facet com as sistemáticas c2, c3, c4
# coluna grupo com geral, A0 e cotas
# coluna efeito com aumentou, igual, diminuiu
# valores a serem preenchidos com base em compila_concs_

data <- data.frame(facet = rep(c("c2","c3","c4"), each = 12),
                   grupo = rep(c("pub", "bxa","ppi","pcd"),each = 3),
                   efeito = c("aumentou","igual","diminuiu"),
                   n_conjuntos = NA)
#                    n_conjuntos = round(abs(rnorm(27)), 2)) # dados fictíticos

data$n_conjuntos

# ==============================================================================
# Preencher valores

# Script gerado com ChatGPT
# Pouco otimizado

# --------------------------------------------------
# ----- aumentou
# para cada linha
# quando for c2, c3, c4; efeito aumentou
# preencher com compila_concs_ c2 c3 ou c4 número de conjuntos maior que 0
# grupo = pub
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4") & grupo == "pub" & efeito == "aumentou" ~
        sum(get(paste0("compila_concs_", facet))$pub > 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ----
# grupo = bxa
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4") & grupo == "bxa" & efeito == "aumentou" ~
        sum(get(paste0("compila_concs_", facet))$bxa > 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ----
# grupo = ppi
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4") & grupo == "ppi" & efeito == "aumentou" ~
        sum(get(paste0("compila_concs_", facet))$ppi > 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ----
# grupo = pcd
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4") & grupo == "pcd" & efeito == "aumentou" ~
        sum(get(paste0("compila_concs_", facet))$pcd > 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# --------------------------------------------------
# ----- igual
# para cada linha
# quando for c2, c3, c4; efeito igual
# preencher com compila_concs_dif_med c2 c3 ou c4 número de conjuntos maior que 0

# ----
# grupo = pub
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4") & grupo == "pub" & efeito == "igual" ~
        sum(get(paste0("compila_concs_", facet))$pub == 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ----
# grupo = bxa
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4") & grupo == "bxa" & efeito == "igual" ~
        sum(get(paste0("compila_concs_", facet))$bxa == 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ----
# grupo = ppi
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4") & grupo == "ppi" & efeito == "igual" ~
        sum(get(paste0("compila_concs_", facet))$ppi == 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ----
# grupo = pcd
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4") & grupo == "pcd" & efeito == "igual" ~
        sum(get(paste0("compila_concs_", facet))$pcd == 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# --------------------------------------------------
# ----- diminuiu
# para cada linha
# quando for c2, c3, c4; efeito aumentou
# preencher com compila_concs_dif_med c2 c3 ou c4 número de conjuntos maior que 0

# ----
# grupo = pub
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4") & grupo == "pub" & efeito == "diminuiu" ~
        sum(get(paste0("compila_concs_", facet))$pub < 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ----
# grupo = bxa
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4") & grupo == "bxa" & efeito == "diminuiu" ~
        sum(get(paste0("compila_concs_", facet))$bxa < 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ----
# grupo = ppi
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4") & grupo == "ppi" & efeito == "diminuiu" ~
        sum(get(paste0("compila_concs_", facet))$ppi < 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ----
# grupo = pcd
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4") & grupo == "pcd" & efeito == "diminuiu" ~
        sum(get(paste0("compila_concs_", facet))$pcd < 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
data$n_conjuntos
print("data está preenchido")
data_backup <- data
# ==============================================================================

# ==============================================================================
# Cria gráfico com base em data

# Reorder the levels of the 'efeito' factor
data$efeito <- factor(data$efeito, levels = c("aumentou", "igual", "diminuiu"))

# Reorder the levels of the 'grupo' factor
data$grupo <- factor(data$grupo, levels = c("pub","bxa","ppi","pcd"))

# Specify colors for each level
colors <- c("aumentou" = "#00BA38", "igual" = "#619CFF", "diminuiu" = "#F8766D")


# Plotting
grafico_c234_vagas <- ggplot(data, aes(x = grupo, y = n_conjuntos, fill = efeito)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = colors) +  # Assign colors manually
  facet_grid(~ facet) +
  ggtitle("Efeito da sistemática na alocação das vagas por grupo") +
  ylab("Número de conjuntos") +
  xlab("Características informadas na inscrição") +
  labs(fill = "Efeito na
alocação das vagas")

grafico_c234_vagas

# ------------------------------------------------------------------------------
# Exportar gráfico
if(T){
  tempo_atual <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S"); texto_hora<-paste0("Documento gerado em ",format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  
  ggsave(
    filename = paste0("grafico_c234_vagas_",tempo_atual,".png"),
    plot = grafico_c234_vagas,
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
