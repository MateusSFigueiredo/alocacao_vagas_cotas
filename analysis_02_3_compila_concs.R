# # ==============================================================================
# Arquivo: analysis_02_3_compila_concs.R

# Compila VAGAS

# Compila a análise das vagas da concorrência c2 de todos os cursos
# idem pra c3, c4, c5

# Exporta para Excel
# com formatação condicional e largura de colunas pre-definida

# Modificado em 2024-01-12
# Autor: Mateus Silva Figueiredo

# diff: converte "+3" em "3" com as.data.frame(sapply(compila_concs, as.numeric))

# =============================================================================
# Preparação
quero_imprimir <- F # não quero que analysis_02_2_output gere novo documento word

setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")

# rodar analysis_02_2_output_comp_vagas.
if(!exists("comp_vagas_ZOOTECNIA_SISU2018")) {source("analysis_02_2_output_comp_vagas.R")} # caso não tenha rodado ainda

# ==============================================================================
# Cria dataframe compila_concs, com n de colunas e de linhas já definido
compila_concs <- as.data.frame(matrix(
  nrow=nrow(df_so_concorridos),
  ncol=ncol(comp_vagas_ADMINISTRACAO_FL_SISU2018)))

# nomeia colunas
colnames(compila_concs) <- colnames(comp_vagas_ADMINISTRACAO_FL_SISU2018)

# nomeia linhas
rownames(compila_concs) <- paste(df_so_concorridos$curso, df_so_concorridos$year,sep="_")

# ========== começa preenchimento de linhas ====================================
# ------------------------------------------------------------------------------
{
# preenche linhas para c1
for (i in 1:nrow(df_so_concorridos)){
  eval(parse(text = paste0("compila_concs[",i,",] <- comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "[1,]")))
}

compila_concs_c1 <- as.data.frame(sapply(compila_concs, as.numeric)) # converte "+3" em "3"
rownames(compila_concs_c1) <- rownames(compila_concs)

# ------------------------------------------------------------------------------
# preenche linhas para c2
for (i in 1:nrow(df_so_concorridos)){
eval(parse(text = paste0("compila_concs[",i,",] <- comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "[2,]")))
}

compila_concs_c2 <- as.data.frame(sapply(compila_concs, as.numeric)) # converte "+3" em "3"
rownames(compila_concs_c2) <- rownames(compila_concs)
# ------------------------------------------------------------------------------
# preenche linhas para c3
for (i in 1:nrow(df_so_concorridos)){
  eval(parse(text = paste0("compila_concs[",i,",] <- comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "[3,]")))
}

compila_concs_c3 <- as.data.frame(sapply(compila_concs, as.numeric)) # converte "+3" em "3"
rownames(compila_concs_c3) <- rownames(compila_concs)

# ------------------------------------------------------------------------------
# preenche linhas para c4
for (i in 1:nrow(df_so_concorridos)){
  eval(parse(text = paste0("compila_concs[",i,",] <- comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "[4,]")))
}

compila_concs_c4 <- as.data.frame(sapply(compila_concs, as.numeric)) # converte "+3" em "3"
rownames(compila_concs_c4) <- rownames(compila_concs) # conserta nome das linhas

# ------------------------------------------------------------------------------
# preenche linhas para c5
for (i in 1:nrow(df_so_concorridos)){
  eval(parse(text = paste0("compila_concs[",i,",] <- comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "[5,]")))
}
compila_concs_c5 <- as.data.frame(sapply(compila_concs, as.numeric)) # converte "+3" em "3"
rownames(compila_concs_c5) <- rownames(compila_concs)
}
# ==============================================================================
# Exportação para Excel, zero otimizado
library(openxlsx)

options("openxlsx.numFmt" = "0") # 2 decimal cases formating

# cria planilha e preenche

# Create a new Excel workbook
wb <- createWorkbook()
{
# Add data to separate sheets with titles
# cria vagas c1
addWorksheet(wb, "vagas c1")
writeData(wb, "vagas c1", "Compila vagas da concorrência c1", startCol = 1, startRow = 1)
writeData(wb, "vagas c1", compila_concs_c1, startCol = 1, startRow = 2, rowNames = TRUE)

# cria vagas c2
addWorksheet(wb, "vagas c2")
writeData(wb, "vagas c2", "Compila vagas da concorrência c2 relativo a c1", startCol = 1, startRow = 1)
writeData(wb, "vagas c2", compila_concs_c2, startCol = 1, startRow = 2, rowNames = TRUE)

# cria vagas c3
addWorksheet(wb, "vagas c3")
writeData(wb, "vagas c3", "Compila vagas da concorrência c3 relativo a c1", startCol = 1, startRow = 1)
writeData(wb, "vagas c3", compila_concs_c3, startCol = 1, startRow = 2, rowNames = TRUE)

# cria vagas c4
addWorksheet(wb, "vagas c4")
writeData(wb, "vagas c4", "Compila vagas da concorrência c4 relativo a c1", startCol = 1, startRow = 1)
writeData(wb, "vagas c4", compila_concs_c4, startCol = 1, startRow = 2, rowNames = TRUE)

# cria vagas c5
addWorksheet(wb, "vagas c5")
writeData(wb, "vagas c5", "Compila vagas da concorrência c5 relativo a c1", startCol = 1, startRow = 1)
writeData(wb, "vagas c5", compila_concs_c5, startCol = 1, startRow = 2, rowNames = TRUE)
}
# ----

# anota data e hora atual
# gera texto_hora ao mesmo tempo
tempo_atual <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S"); texto_hora<-paste0("Documento gerado em ",format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

# Add ultima planilha com informações e data
addWorksheet(wb, "hora")
writeData(wb, "hora", texto_hora, startCol = 1, startRow = 1)

# ---------------------------------------------------------
# Prepara formatação condicional 
negStyle <- createStyle(fontColour = "black", bgFill = "pink")
posStyle <- createStyle(fontColour = "black", bgFill = "lightgreen")

# Compila titulo das planilhas
titulo_planilha<-c("vagas c1","vagas c2","vagas c3","vagas c4","vagas c5")

# Aplica formatação condicional com loop t
#
for (t in 1:length(titulo_planilha)){
conditionalFormatting(wb, sheet = titulo_planilha[t],
                      type = "expression",
                      rule = "<0",
                      style = negStyle,
                      rows = 3:69, cols = 2:15)

conditionalFormatting(wb, sheet = titulo_planilha[t],
                      type = "expression",
                      rule = ">0",
                      style = posStyle,
                      rows = 3:69, cols = 2:15)

# define largura das colunas
setColWidths(wb, sheet = titulo_planilha[t], cols = 1, widths = 30.27)
setColWidths(wb, sheet = titulo_planilha[t], cols = 2:15, widths = 8.45)

} # fecha loop t


# ---------------------------------------------------------

# Save the Excel workbook
if(T){
  save_path <- paste0("output_analysis_02_3_compila_vagas_", tempo_atual, ".xlsx")
  saveWorkbook(wb, save_path, overwrite = TRUE)
  print("arquivo Excel está criado")
}
