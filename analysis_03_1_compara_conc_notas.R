# # ==============================================================================
# Arquivo: analysis_03_1_compara_conc_notas.R ESCREVENDO

# Usa source analysis_03_0_compila_meta_n.R

# Compara cinco modelos de concorrencia:
# 1 = concorrencia separada (aka listas multiplas)
# 2 = concorrencia concomitante, AC primeiro
# 3 = concorrencia concomitante, cotas primeiro
# 4 = concorrencia segundo Bó e Senkevics, 2023
# 5 = concorrencia adaptado de Bó e Senkevics, 2023

# Input:
# meta_n_ CURSO ANO

# Output:
# meta_n_ CURSO ANO comparativo a c1
# Gera dfs comparando_vagas
# Exporta arquivo Excel com formatação condicional
# e com hora de exportação

# Limitação:
# Valores NA são exportados como células vazias

# Modificado em 2024-01-03.
# Autor: Mateus Silva Figueiredo

# ==============================================================================
# Preparação
library(tidyverse)

# Para exportar para Excel
# Load the writexl package
# install.packages("openxlsx")
library(openxlsx)

# ==============================================================================
# Carregar dados a partir de analysis_03_0
if (!exists("meta_n_DIREITO_SISU2019")) source("analysis_03_0_compila_meta_n.R") 
# cria ~200 objetos

# ------------------------------------------------------------------------------
# definir inicio e fim, para depois rodar for loop

# para um conjunto
# inicio<-1; fim<-5

# apenas alguns conjuntos
# inicio<-29; fim<-31

# para todos os conjuntos:
inicio<-1; fim<-nrow(df_so_concorridos)


# ================ ABRE FOR LOOP ============================================
# Iniciar alocação

# Create a new Excel workbook
wb <- createWorkbook()
options("openxlsx.numFmt" = "0.00") # 2 decimal cases formating

for (i in inicio:fim){ # abre loop i, um para cada conjunto 

# eval parse para carregar meta_n CURSO ANO em meta_n
eval(parse(text=(paste0(
"meta_n <- meta_n_",df_so_concorridos[i,1],"_",df_so_concorridos[i,2] 
))))

df_so_concorridos[i,]
meta_n[2,]

# Cria comparação entre c2:c5 - c1
# Notas serão em relação a c1
dif_med_c2<-(meta_n["mean_c2",]-meta_n["mean_c1",]) %>% round(3)
dif_med_c3<-(meta_n["mean_c3",]-meta_n["mean_c1",]) %>% round(3)
dif_med_c4<-(meta_n["mean_c4",]-meta_n["mean_c1",]) %>% round(3)
dif_med_c5<-(meta_n["mean_c5",]-meta_n["mean_c1",]) %>% round(3)
dif_min_c2<-meta_n["min_c2",]-meta_n["min_c1",] %>% round(3)
dif_min_c3<-meta_n["min_c3",]-meta_n["min_c1",] %>% round(3)
dif_min_c4<-meta_n["min_c4",]-meta_n["min_c1",] %>% round(3)
dif_min_c5<-meta_n["min_c5",]-meta_n["min_c1",] %>% round(3)

# para arredondar c1 também:
meta_n["mean_c1",] <- meta_n["mean_c1",] %>% round(3)
meta_n["min_c1",]  <- meta_n["min_c1",] %>% round(3)

# cria dataframe dif_n com valor de c1 e todas as comparações
dif_n <- rbind(
  meta_n["mean_c1",],meta_n["min_c1",],
  dif_med_c2, dif_min_c2,
  dif_med_c3, dif_min_c3,
  dif_med_c4, dif_min_c4,
  dif_med_c5, dif_min_c5)

# nome para linhas de dif_n
rownames(dif_n) <- c("med_c1","min_c1",
                     "dif med_c2","dif min_c2",
                     "dif med_c3","dif min_c3",
                     "dif med_c4","dif min_c4",
                     "dif med_c5","dif min_c5")

# eval parse para salvar dif_n em dif_n_ CURSO ANO
eval(parse(text=(paste0(
  "dif_n -> dif_n_",df_so_concorridos[i,1],"_",df_so_concorridos[i,2] 
))))

# produto final: dif_n_ CURSO ANO

  # ------------------------------------------------------------------------------
  # verificar resultados
eval(parse(text=(paste0(
  "print((dif_n_",df_so_concorridos[i,1],"_",df_so_concorridos[i,2],")[1,])" 
))))

# ==============================================================================
# Exportação para Excel, feat. Chat GPT

# ------------------------------------------------------
# prepara palavras

# obtém nome do curso
curso <- df_so_concorridos[i,1]

# caso curso tenha nome comprido, resumir assim:
if(nchar(curso)>16){
  curso<-paste0(substr(curso,1,5),
                "_",
                substr(curso, nchar(curso) - 9, nchar(curso)))
}

# obtém ano
ano <- substr(df_so_concorridos[i,2],5,8)

titulo_planilha <- paste0(curso,"_",ano)

# Definir título da tabela e da planilha
titulo_tabela <- paste0("Notas de ", df_so_concorridos[i,1],"_",df_so_concorridos[i,2])

# ------------------------------------------------------
# cria planilha e preenche

# Add data to separate sheets with titles
addWorksheet(wb, titulo_planilha)
writeData(wb, titulo_planilha, titulo_tabela, startCol = 1, startRow = 1)
writeData(wb, titulo_planilha, dif_n, startCol = 1, startRow = 2, rowNames = TRUE)

# ---------------------------------------------------------
# Formatação condicional nas linhas de c2 até c5 - cores
# min_value <- min(dif_n[3:10,])-1
# max_value <- max(dif_n[3:10,])+1

negStyle <- createStyle(fontColour = "black", bgFill = "pink")
posStyle <- createStyle(fontColour = "black", bgFill = "lightgreen")
# 
conditionalFormatting(wb, sheet = titulo_planilha,
                      type = "expression",
                      rule = "<0",
                      style = negStyle,
                      rows = 5:12, cols = 2:12)

conditionalFormatting(wb, sheet = titulo_planilha,
                      type = "expression",
                      rule = ">0",
                      style = posStyle,
                      rows = 5:12, cols = 2:12)

# ---------------------------------------------------------
# anota data e hora atual
# gera texto_hora ao mesmo tempo
tempo_atual <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S"); texto_hora<-paste0("Documento gerado em ",format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

} # fim do loop i, de cada conjunto

# Add ultima planilha com informações e data
addWorksheet(wb, "hora")
writeData(wb, "hora", texto_hora, startCol = 1, startRow = 1)

# ==============================================================================

# Save the Excel workbook
if(T){
save_path <- paste0("output_analysis_03_1_comp_notas_", tempo_atual, ".xlsx")
saveWorkbook(wb, save_path, overwrite = TRUE)
}

# ==============================================================================
# Limpeza
if(F){
  # remover objetos 
  rm(list = ls(pattern = "^analise_"))
  rm(list = ls(pattern = "^meta_"))
  rm(list = ls(pattern = "^nvagas_"))
  rm(df)
  rm(list = ls(pattern = "^dados_")) # remover objetos dados_
  #  rm("candidatos")
}

