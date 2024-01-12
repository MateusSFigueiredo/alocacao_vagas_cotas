# ==============================================================================
# Arquivo: analysis_03_2_compila_conc_notas.R

# Modificado em 2024-01-11.
# Autor: Mateus Silva Figueiredo

# diff: documentação. ordem das planilhas exportadas p excel.
# diff: formatação condicional.

# -------------------

# Objetivos:
#  juntar nota média de c1 de todos os conjuntos
#  o mesmo para c2, c3, c4, c5
#  juntar nota mínima de c1 de todos os conjuntos
#  o mesmo para c2, c3, c4, c5

# Semelhante a analysis_02_3_compila_concs.R

# Input: dif_n_CURSO ANO, carregado com source abaixo

# Output: compila_concs CURSO ANO

# ==============================================================================

# se F (padrão): script analysis_03_1 não gera novo documento excel
quero_imprimir_excel <- F

# Carregar dados a partir de analysis_03_1
if (!exists("dif_n_AGRONOMIA_SISU2018")) source("analysis_03_1_compara_conc_notas.R") 
# cria muitos objetos

# ==============================================================================

# ------ Cria dataframe e nomeia colunas

# Cria dataframe compila_nota_media_concs, com n de colunas e de linhas já definido
compila_concs <- as.data.frame(matrix(
  nrow=nrow(df_so_concorridos),
  ncol=ncol(dif_n_ADMINISTRACAO_FL_SISU2018)))

compila_concs

# nomeia colunas
colnames(compila_concs) <- colnames(dif_n_ADMINISTRACAO_FL_SISU2018)

# nomeia linhas
rownames(compila_concs) <- paste(df_so_concorridos$curso, df_so_concorridos$year,sep="_")

# -------------------------------------------------------------------------------
# fonte das informações: dif_n_ CURSO ANO
# rownames(dif_n)
rownames(dif_n) <- gsub(" ","_",rownames(dif_n)) # espaço vira underline
# usado para nomear objetos compila_conc MEDIDA CURSO ANO

# ========== começa preenchimento de linhas ====================================
# ------------------------------------------------------------------------------
# preenche linhas para c1 med
medida <- 1
# medida usado para obter textos "med_c1", "min_c2", etc.

for (medida in 1:nrow(dif_n)){

for (i in 1:nrow(df_so_concorridos)){
  eval(parse(text = paste0("compila_concs[",i,",] <- dif_n_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "[",medida,",]")))
}

# compila_concs
# compila_concs_(med_c1 ou similar) <- compila_concs
eval(parse(text = paste0("compila_concs_",rownames(dif_n)[medida], "<- compila_concs")))

# ver primeira linha
eval(parse(text = paste0("compila_concs_",rownames(dif_n)[medida]))) %>% head(1)
}

# produto = compila_concs_dif_med_c2 até c5, dif_min_c2 até c5

# ==============================================================================
# Exportação para Excel, feat. Chat GPT
library(openxlsx)

# Prepara formatação condicional 
negStyle <- createStyle(fontColour = "black", bgFill = "pink")
posStyle <- createStyle(fontColour = "black", bgFill = "lightgreen")

# cria planilha e preenche

# Create a new Excel workbook
wb <- createWorkbook()

# abre loop medida

# para um valor de medida
# medida <- 1

# ordem das planilhas: med_c1 min_c1 med_c2 min_c2...
# for (medida in 1:nrow(dif_n)){ 

# ordem das planilhas: med_c1 med_c2 ... min_c1 min_c2 ...
for (medida in c(1,3,5,7,9,2,4,6,8,10)){ 
  
  
# -----------------------------------

# prepara palavras
# cria titulo_planilha <- compila_concs_dif_ medida
titulo_planilha <- paste0("compila_concs_",row.names(dif_n)[medida])

# Definir título da tabela e da planilha
titulo_tabela <- paste0("Compilação da comparação das notas ", row.names(dif_n)[medida])

# -----------------------------------
# prepara dados a serem escritos
# compila_concs <- compila_concs_dif medida de med_c1 até dif_min_c5
eval(parse(text = paste0("compila_concs <- compila_concs_",row.names(dif_n)[medida])))

# -----------------------------------
# Add data to separate sheets with titles
# cria planilha da medida==1 em diante
addWorksheet(wb, titulo_planilha)
# linha 1 do excel:
writeData(wb, titulo_planilha, titulo_tabela, startCol = 1, startRow = 1)
# linha 2 do excel:
writeData(wb, titulo_planilha, compila_concs, startCol = 1, startRow = 2, rowNames = TRUE)

# ---------------------------------------------------------
# Aplica formatação condicional 
# 
conditionalFormatting(wb, sheet = titulo_planilha,
                      type = "expression",
                      rule = "<0",
                      style = negStyle,
                      rows = 3:69, cols = 2:12)

conditionalFormatting(wb, sheet = titulo_planilha,
                      type = "expression",
                      rule = ">0",
                      style = posStyle,
                      rows = 3:69, cols = 2:12)

} # fecha loop medida

# ----

# anota data e hora atual
# gera texto_hora ao mesmo tempo
tempo_atual <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S"); texto_hora<-paste0("Documento gerado em ",format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

# Add ultima planilha com informações e data
addWorksheet(wb, "hora")
writeData(wb, "hora", texto_hora, startCol = 1, startRow = 1)

# Save the Excel workbook
if(T){
  save_path <- paste0("output_analysis_03_2_compila_conc_notas_", tempo_atual, ".xlsx")
  saveWorkbook(wb, save_path, overwrite = TRUE)
  print("arquivo Excel salvo")
}

# ==============================================================================
# Limpeza
if(F){
  # remover objetos 
  # rm(list = ls(pattern = "^compila_"))
  rm(list = ls(pattern = "^meta_"))
  rm(list = ls(pattern = "^nvagas_"))
  rm(list = ls(pattern = "^analise_"))
  # rm(df)
  rm(list = ls(pattern = "^dados_")) # remover objetos dados_
  #  rm("candidatos")
}
