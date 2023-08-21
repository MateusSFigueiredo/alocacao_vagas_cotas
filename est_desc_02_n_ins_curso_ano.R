# ==============================================================================
# Arquivo: est_desc_02_n_ins_curso_ano.R

#
# Modificado em: 2023-08-21
# Autor: Mateus Silva Figueiredo
#
# Utiliza dados_ufv carregado por data_04_carregar_dados_UFV

# Faz:
# - data.frame df_ins_curso_ano com número total de inscritos 
# para cada curso em cada ano
# - gráfico de linha com inscritos em cada ano,
# um gráfico para cada centro

# Falta:
# - exportar gráficos

# Ideia: omitir valores zero dos gráficos

# ==============================================================================
# Preparação
library(pals) # para cores

# ==============================================================================
# Carregar dados_ufv
setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")
list.files()

# Carregar dados com data_04_carregar_dados_UFV.R
por_curso <- T   # deseja separar candidatos por curso? 
por_ano   <- F   # deseja separar candidatos por ano? opcional
source("data_04_carregar_dados_UFV.R") # cria ~80 objetos

# ==============================================================================

# Criar dataframe df_ins_curso_ano
# Linhas = anos 2013:2022
# Colunas = número de inscritos para cada curso

# Create an empty data frame to store the results
df_ins_curso_ano <- data.frame(Ano = 2013:2022)

# Loop through each course in lista_cursos
# Preencher df_ins_curso_ano
for (curso in lista_cursos) {
  for (ano in 2013:2022) {
    # Filter the data and count rows based on the course and year
    count <- dados_ufv[Curso == curso & Processo_Seletivo == paste0("SISU", ano)][, .N]
    
    # Store the count in the df_ins_curso_ano data frame
    df_ins_curso_ano[df_ins_curso_ano$Ano == ano, curso] <- count
  }
}

head(df_ins_curso_ano)
print("df_ins_curso_ano está criado")

# ------------------------------------------------------------------------------
# Plot graph
# um curso
plot (df_ins_curso_ano$MEDICINA~df_ins_curso_ano$Ano,
      ylab="MEDICINA")


# ---------
# Criar data.frame por centro
centros<-c("cca","ccb","cce","cch","caf","crp")

# apenas para o caf:
# df_ins_caf <- cbind('Ano'=2013:2022,
#                    df_ins_curso_ano[cursos_caf])
# colnames(df_ins_caf) <- gsub("_"," ",colnames(df_ins_caf)) # remove _ do curso


# para todos os seis centros:
for (centro in centros) {
eval(parse(text=(paste("df_ins_",centro,
                       "<- cbind('Ano'=2013:2022,df_ins_curso_ano[cursos_",
                       centro,
                       "])",sep="")))) # criou
  
# remover _ para gráfico ficar bonito
eval(parse(text=(paste("colnames(df_ins_",centro,
                         ") <- gsub('_',' ',colnames(df_ins_",
                         centro,
                         "))",sep="")))) # remove _

}

# --------------------------------------------
# Abrir loop para gerar gráficos
for (centro in centros) {
  
#  Alimentar df_to_plot com df_ins_ centro
  eval(parse(text=(paste("df_to_plot <- df_ins_",
                         centro,sep=""))))
# gerar gráfico:

# Convert the data frame to a longer format for plotting
df_long <- tidyr::pivot_longer(df_to_plot, cols = -Ano, names_to = "Curso", values_to = "Number")

# Create the plot using ggplot2
grafico <- ggplot(df_long, aes(x = as.factor(Ano), y = Number, color = Curso, group = Curso)) +
  geom_line() +
  labs(title = paste("Inscritos por curso -",toupper(centro)),
       x = "Ano", y = "Número de inscritos") +
  scale_x_discrete(breaks = as.factor(2013:2022)) +
  scale_color_manual(values = unname(alphabet2())) +
  theme_minimal(); print(grafico)

} # fim do loop que gera gráfico
# alphabet é paleta do package pals

###
