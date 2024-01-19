# ==============================================================================
# Arquivo: est_desc_06_nota_med_c234.R
#
# Modificado em: 2024-01-19 12h
# Autor: Mateus Silva Figueiredo

# diff: separa "diminuiu pouco" e "diminuiu muito"

# Cria gráfico comparando c2, c3 e c4
# expondo variação de nota média para geral, A0 e cotas
# Em quantos conjuntos aumentou, manteve igual ou diminuiu
# ==============================================================================
# Preparação
library(dplyr)

# definir limite entre "diminuiu pouco" e "diminuiu muito"
muito <- -10

# se F (padrão): script analysis_03_2 não gera novo documento excel
quero_imprimir_03_2 <- F

# se não tiver todado analysis_03_2, rodar
if (!exists("compila_concs_dif_med_c2")) source("analysis_03_2_compila_conc_notas.R")

# ==============================================================================
# Cria dataframe para receber valores
# coluna facet com as sistemáticas c2, c3, c4
# coluna grupo com geral, A0 e cotas
# coluna efeito com aumentou, igual, diminuiu
# valores a serem preenchidos com base em compila_concs_

data <- data.frame(facet = rep(c("c2","c3","c4"), each = 12),
                   grupo = rep(c("geral", "A0","cotas"),each = 4),
                   efeito = c("aumentou","igual","diminuiu pouco","diminuiu muito"),
                   n_conjuntos = NA)
#                    n_conjuntos = round(abs(rnorm(27)), 2)) # dados fictíticos

data

# ==============================================================================
# Preencher valores

# -----------
# Preenchendo com ChatGPT

# --------------------------------------------------
# ----- aumentou
# para cada linha
# quando for c2, c3, c4; efeito aumentou
# preencher com compila_concs_dif_med c2 c3 ou c4 número de conjuntos maior que 0
# grupo = geral
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4") & grupo == "geral" & efeito == "aumentou" ~
        sum(get(paste0("compila_concs_dif_med_", facet))$geral > 0),
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
      facet %in% c("c2", "c3", "c4") & grupo == "A0" & efeito == "aumentou" ~
        sum(get(paste0("compila_concs_dif_med_", facet))$A0 > 0),
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
      facet %in% c("c2", "c3", "c4") & grupo == "cotas" & efeito == "aumentou" ~
        sum(get(paste0("compila_concs_dif_med_", facet))$cotas > 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# --------------------------------------------------
# ----- igual
# para cada linha
# quando for c2, c3, c4; efeito igual
# preencher com compila_concs_dif_med c2 c3 ou c4 número de conjuntos maior que 0
# grupo = geral
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4") & grupo == "geral" & efeito == "igual" ~
        sum(get(paste0("compila_concs_dif_med_", facet))$geral == 0),
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
      facet %in% c("c2", "c3", "c4") & grupo == "A0" & efeito == "igual" ~
        sum(get(paste0("compila_concs_dif_med_", facet))$A0 == 0),
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
      facet %in% c("c2", "c3", "c4") & grupo == "cotas" & efeito == "igual" ~
        sum(get(paste0("compila_concs_dif_med_", facet))$cotas == 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# --------------------------------------------------
# ----- diminuiu pouco
# para cada linha
# quando for c2, c3, c4; efeito diminuiu pouco
# preencher com compila_concs_dif_med c2 c3 ou c4 número de conjuntos entre 0 e -10
# grupo = geral
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4") & grupo == "geral" & efeito == "diminuiu pouco" ~
        sum(get(paste0("compila_concs_dif_med_", facet))$geral > muito 
            & get(paste0("compila_concs_dif_med_", facet))$geral < 0),
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
      facet %in% c("c2", "c3", "c4") & grupo == "A0" & efeito == "diminuiu pouco" ~
        sum(get(paste0("compila_concs_dif_med_", facet))$A0 > muito & 
              get(paste0("compila_concs_dif_med_", facet))$A0 < 0),
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
      facet %in% c("c2", "c3", "c4") & grupo == "cotas" & efeito == "diminuiu pouco" ~
        sum(get(paste0("compila_concs_dif_med_", facet))$cotas > muito & 
              get(paste0("compila_concs_dif_med_", facet))$cotas < 0),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# --------------------------------------------------
# ----- diminuiu muito
# para cada linha
# quando for c2, c3, c4; efeito diminuiu muito
# preencher com compila_concs_dif_med c2 c3 ou c4 número de conjuntos entre 0 e -10
# grupo = geral
data <- data %>%
  rowwise() %>%
  mutate(
    n_conjuntos = case_when(
      facet %in% c("c2", "c3", "c4") & grupo == "geral" & efeito == "diminuiu muito" ~
        sum(get(paste0("compila_concs_dif_med_", facet))$geral < muito),
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
      facet %in% c("c2", "c3", "c4") & grupo == "A0" & efeito == "diminuiu muito" ~
        sum(get(paste0("compila_concs_dif_med_", facet))$A0 < muito),
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
      facet %in% c("c2", "c3", "c4") & grupo == "cotas" & efeito == "diminuiu muito" ~
        sum(get(paste0("compila_concs_dif_med_", facet))$cotas < muito),
      TRUE ~ n_conjuntos
    )
  ) %>%
  ungroup()

# ---
data
print ("data está preenchida")

# ==============================================================================
# Cria gráfico com base em data

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
grafico_c234_med <- ggplot(data, aes(x = grupo, y = n_conjuntos, fill = efeito)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = colors, labels = efeitos) +  # Assign colors manually
  facet_grid(~ facet) +
  ggtitle("Efeito das sistemáticas c2, c3 e c4 na nota média por grupo") +
  ylab("Número de conjuntos") +
  xlab("Modalidade de convocação") +
  labs(fill = "Efeito na
nota média")

grafico_c234_med

# ------------------------------------------------------------------------------
# Exportar gráfico
if(T){
tempo_atual <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S"); texto_hora<-paste0("Documento gerado em ",format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

ggsave(
  filename = paste0("grafico_c234_med_",tempo_atual,".png"),
  plot = grafico_c234_med,
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
