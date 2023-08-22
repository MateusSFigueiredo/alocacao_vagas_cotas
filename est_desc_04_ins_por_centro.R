# ==============================================================================
# Arquivo: est_desc_04_ins_por_centro.R
#
# Quantos inscritos tem por ano, por centro e campus?
# Gera gráfico de barras empilhadas

# Modificado em: 2023-05-23.
# Autor: Mateus Silva Figueiredo

# Ideia: excluir Medicina e Med Vet.

# ==============================================================================
# Preparação
library(ggplot2)# gráficos

# ==============================================================================
# Carregar dados_ufv

setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")

# Carregar dados com data_04_carregar_dados_UFV.R
por_curso <- T   # deseja separar candidatos por curso? obrigatório
por_ano   <- T   # deseja separar candidatos por ano? opcional
source("data_04_carregar_dados_UFV.R") # cria ~10 objetos

# ==============================================================================
# objetos necessários

sisu_anos<-paste0("SISU",c(2013:2022))

# ==============================================================================

# Cria ins_centros = data.frame com n de inscritos por centro por ano
ins_centros <- data.frame(sisu_anos=sisu_anos,
                          anos=2013:2022,
                         ins_cca=NA,
                         ins_ccb=NA,
                         ins_cce=NA,
                         ins_cch=NA,
                         ins_crp=NA,
                         ins_caf=NA,
                         ins_ufv=NA
                         )

# Coloca nome nas linhas
row.names(ins_centros) <- c(2013:2022)

# ------------------------------------------------------------------------------

# Preenche para um centro, todos os anos
# for (i in 2013:2022){
# dados_ufv[Curso %in% cursos_cca][Processo_Seletivo == paste0("SISU",i)] %>%
#   nrow() -> ins_centros[paste(i),"ins_cca"]
# }

# Preenche para UFV inteira, todos os anos
for (i in 2013:2022){
dados_ufv[Processo_Seletivo == paste0("SISU",i)] %>%
  nrow() -> ins_centros[paste(i),"ins_ufv"]
}

# ------------------------------------------------------------------------------
# Preenche para todos os centros, todos os anos

for (centro in c("cursos_cca", "cursos_ccb", "cursos_cce", "cursos_cch", "cursos_caf", "cursos_crp")) {
  for (i in 2013:2022) {
    dados_ufv[Curso %in% get(centro)][Processo_Seletivo == paste0("SISU", i)] %>%
      nrow() -> ins_centros[paste(i), paste("ins_", substr(centro, 8, nchar(centro)), sep = "")]
  }
}

# ------------------------------------------------------------------------------

# Confere somatória. Deve ser tudo TRUE.
ins_centros$ins_ufv == (
  ins_centros$ins_cca +
  ins_centros$ins_ccb +
  ins_centros$ins_cce +
  ins_centros$ins_cch +
  ins_centros$ins_crp +
  ins_centros$ins_caf)

# ------------------------------------------------------------------------------

# Deletar colunas desnecessárias e renomear, para facilitar gráfico
ins_centros$ins_ufv <- NULL
ins_centros$sisu_anos <- NULL
colnames(ins_centros) <- c("anos","CCA","CCB","CCE","CCH","CRP","CAF")

# ----------
# Preparar para gráfico. Obrigado ChatGPT.

# Reshape the data from wide to long format
data_long <- reshape2::melt(ins_centros, id.vars = "anos", variable.name = "Centro", value.name = "Count")

#---

colors <- c("#B66638","#BBBBAA","#E41A1C","#377EB8","#65bb64","#ff9a00")

# Create the stacked bar graph
ggplot(data_long, aes(x = as.factor(anos), y = Count, fill = factor(Centro, levels = c("CAF","CRP","CCH","CCE","CCB","CCA")))) +
  geom_bar(stat = "identity") +
  labs(x = "Edição do SISU", y = "Inscritos por Centro/Campus", fill = "Centro") +
  scale_fill_manual(values = colors) +
  theme_bw() +
  ggtitle("Inscritos na UFV 2013-2022") +
  theme(legend.position = "right")

# Fim do código

# ==============================================================================
# Referências
# R Color Brewer's palettes https://r-graph-gallery.com/38-rcolorbrewers-palettes.html

# ==============================================================================

# Anotações # Possibilidades de cores

# library(RColorBrewer)

# colors <- brewer.pal(7, "Set1")  # Change the number to match the number of Centro categories
# colors <- brewer.pal(7, "Accent")  # Change the number to match the number of Centro categories

# colors <- c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#A65628",
#             "#FFFF33","#F781BF","#999999") #Set1
# 
# colors <- c("#7FC97F","#BEAED4","#FDC086","#FFFF99","#386CB0","#F0027F",
#             "#BF5B17","#666666") # Accent

# Minhas escolhas
# colors <- c("#876137","#BE8ED4","#E41A1C","#377EB8","#65bb64","#ff9a00")
# colors <- c("#B66638","#BBBBAA","#E41A1C","#377EB8","#65bb64","#ff9a00")