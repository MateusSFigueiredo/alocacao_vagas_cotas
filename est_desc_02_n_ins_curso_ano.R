# ==============================================================================
# Arquivo: est_desc_02_n_ins_curso_ano.R

#
# Modificado em: 2023-08-22
# Autor: Mateus Silva Figueiredo
#
# Utiliza dados_ufv carregado por data_04_carregar_dados_UFV

# Faz:
# - data.frame df_ins_curso_ano com número total de inscritos 
# para cada curso em cada ano
# - gráfico de linha com inscritos em cada ano,
# um gráfico para cada centro
# - omite valores zero dos gráficos
# - definir cores
# - exportar gráficos

# ==============================================================================
# Preparação
library(ggplot2) # gráfico
library(dplyr)

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

# head(df_ins_curso_ano)
# print("df_ins_curso_ano está criado")

# ------------------------------------------------------------------------------
# Plot graph
# plot(df_ins_caf$`ADMINISTRACAO FL`)

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
# Abrir loop para gerar gráficos e salvar imagens
for (centro in centros) { # para todos os centros
#for (centro in c("crp")){ # para apenas um centro
  
#  Alimentar df_to_plot com df_ins_ centro
  eval(parse(text=(paste("df_to_plot <- df_ins_",
                         centro,sep=""))))
# gerar gráfico:

# Convert the data frame to a longer format for plotting
df_long <- tidyr::pivot_longer(df_to_plot, cols = -Ano, names_to = "Curso", values_to = "Number")

# transforma 0 em NA
 df_long[df_long == 0] <- NA

# Ordenar cursos por número de inscritos em 2013
totals_2013 <- df_long %>%
  filter(Ano == 2013) %>%
  group_by(Curso) %>%
  summarize(total_inscriptions = sum(Number)) %>%
  arrange(desc(total_inscriptions))

# Create a factor variable for Curso based on 2013 totals
df_long$Curso <- factor(df_long$Curso, levels = totals_2013$Curso)

# Define cores para gráfico. Precisa de 17 cores no mínimo.
cores <- c("#0000FF", "#FF0000", "#1CBE4F", "#FF7F00", "#9A4D62",
           "#FF00B6", "#00530E", "#009FFF", "#F8A19F", "#FFD300",
           "#783FC1", "#1F9698", "#886C00", "#F1085C", "#AA0DFE",
           "#B2DF8A", "#DD00FF", "#201A01")

# Create the plot using ggplot2
grafico <- ggplot(df_long, aes(x = as.factor(Ano), y = Number, color = Curso, group = Curso)) +
  geom_line() +
  labs(title = paste("Inscritos por curso -",toupper(centro)),
       x = "Ano", y = "Número de inscritos") +
  scale_x_discrete(breaks = as.factor(2013:2022)) +
  scale_color_manual(values = cores) +
  scale_y_continuous(limits = c(0, NA)) + # mostra 0 no eixo x
  theme_minimal(); 
 print(grafico)

# } # fim do loop que gera gráfico

# ------------------------------------------------------------------------------
# Salvar gráfico de linhas como imagens png

if(F){ # deseja salvar imagens? T = sim. F = não.
  # criar pasta para gráficos
  pasta_imagens <- "imagens_est_desc_02_n_ins_curso_ano" #nome da pasta
  if (!dir.exists(pasta_imagens)) {
    dir.create(pasta_imagens)
  } # cria pasta
  
  # nome do arquivo
  image_filename <- paste0("ins_ano_",centro,".png")

  # salva imagem
  ggsave(file.path(pasta_imagens, image_filename), plot = grafico, 
         width = 8, height = 5, dpi = 300)
} # fim da parte de salvar imagem

} # fim do loop de lista_cursos após salvar imagens de gráficos

# ==============================================================================
# ==============================================================================
# ==============================================================================

# Analisar apenas poucos anos, para comparação mais simples
print("a seguir, plotar apenas 2013, 2015, 2022")

# apenas caf
# df_ins2_caf <- df_ins_caf %>% subset(Ano %in% c(2013,2022)) 

# para todos os seis centros:
for (centro in centros) {
  eval(parse(text=(paste("df_ins2_",centro,
                         "<- df_ins_",centro, # definir anos
                         "%>% subset(Ano %in% c(2013, 2022))",sep="")))) # criou
  
}

# --------------------------------------------
# Abrir loop para gerar gráficos 2013 e 2022
for (centro in centros) { # para todos os centros
#for (centro in c("caf")){ # para apenas um centro
  
  #  Alimentar df_to_plot com df_ins_ centro
  eval(parse(text=(paste("df_to_plot <- df_ins2_",
                         centro,sep=""))))
  # gerar gráfico:
  
  # Convert the data frame to a longer format for plotting
  df_long <- tidyr::pivot_longer(df_to_plot, cols = -Ano, names_to = "Curso", values_to = "Number")
  
  # transforma 0 em NA
  df_long[df_long == 0] <- NA
  
  # Ordenar cursos por número de inscritos no primeiro ano (2013 ou não)
  totals_2013 <- df_long %>%
    filter(Ano == min(Ano)) %>%
    group_by(Curso) %>%
    summarize(total_inscriptions = sum(Number)) %>%
    arrange(desc(total_inscriptions))
  
  # Create a factor variable for Curso based on 2013 totals
  df_long$Curso <- factor(df_long$Curso, levels = totals_2013$Curso)
  
  # Define cores para gráfico. Precisa de 17 cores no mínimo.
  cores <- c("#0000FF", "#FF0000", "#1CBE4F", "#005300", "#FF00B6",
             "#9A4D42", "#00530E", "#F8A19F", "#FFD300", "#009FFF", 
             "#783FC1", "#1F9698", "#AA0DFE", "#886C00", "#F1085C",
             "#FE8F42", "#DD00FF", "#201A01")
  
  # Create the plot using ggplot2
  grafico <- ggplot(df_long, aes(x = as.factor(Ano), y = Number, color = Curso, group = Curso)) +
    geom_line() +
    labs(title = paste("Inscritos por curso -",toupper(centro)),
         x = "Ano", y = "Número de inscritos") +
    scale_x_discrete(breaks = as.factor(2013:2022)) +
    scale_color_manual(values = cores) +
    scale_y_continuous(limits = c(0, NA)) + # mostra 0 no eixo x
    theme_minimal(); 
  print(grafico)
  
   } # fim do loop que gera gráfico


# ==============================================================================

# Referências
# Cores retiradas do pacote pals, usando paletas glasbey, alphabet2, etc.
# Pals package https://cran.r-project.org/web/packages/pals/vignettes/pals_examples.html
