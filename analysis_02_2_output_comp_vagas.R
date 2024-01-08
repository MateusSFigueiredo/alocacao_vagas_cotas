# # ==============================================================================
# Arquivo: analysis_02_2_output_comp_vagas

# Transforma linhas de erro em NA
# Gera documento Word com todos os dataframes
#  comparando as sistemáticas c1 até c5

# Modificado em 2024-01-03
# Autor: Mateus Silva Figueiredo

# diff: setwd() para conseguir rodar source. Documentação.

# ==============================================================================
quero_imprimir <- T # caso queira salvar um novo documento word

setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")

# Rodar após analysis_02_0_compara_conc.R
if(!exists("comparando_vagas")) {source("analysis_02_0_compara_conc.R")} # caso não tenha rodado ainda
# Altera dfs comp_vagas_ CURSO _ ANO, apagando linhas de concorrencias com problema

# ------------------------------------------------------------------------------

# df_so_concorridos gera lista de conjuntos concorridos
# no entanto, em alguns deles a concorrencia c2 e c5 deu problema
# o total de convocados foi menor do que o número de vagas
# provavelmente devido a cotista ir pra AC e não ter cotista pra preencher vaga

# foram 17 conjuntos com esse problema em c2

# solução: apagar linhas de concorrencias que dê tot errado
# problema: isso cria um viés
# decisão: simplificar análise para poder terminar logo o mestrado
# decisão: solução vale a pena, só explicitar o viés depois

# ==============================================================================

# # Para um curso arbitrário
# comp_vagas_ADMINISTRACAO_FL_SISU2018$tot[2] == "+0"

# ------------------------------------------------------------------------------
# Para um curso chamado por i
# i<-1
# 
# df_so_concorridos[i,1] # curso do conjunto i
# df_so_concorridos[i,2] # sisu do conjunto i

# analisa total de c2
# comp_vagas_ZOOTECNIA_SISU2018$tot[2]           # valor de tot c2
# comp_vagas_ZOOTECNIA_SISU2018$tot[2] == '+0'   # compara com '+0'
#tot[2] é o total da concorrência 2. se for '+0' então está ok.

# ------------------------------------------------------------------------------
# para todos os cursos # c2

# --- eu que fiz
# apaga linha 2 caso tot =! '+0', preenche com NA
# para todos os conjuntos

eval(parse(text = paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "$tot[2] == '+0'")))

# ==== rodar for loop c2 ====
for (i in 1:nrow(df_so_concorridos)){

if(eval(parse(text = paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "$tot[2] == '+0'")))){
  "true"
} else {"false"
  eval(parse(text = paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "[2,] <- NA")))
} # fecha if
  }; print("fim loop c2") # fecha for loop
# se já tiver $tot[2]==NA, vai dar erro, mas tudo bem

# ------------------------------------------------------------------------------
# para todos os cursos # c5

# --- eu
# apaga linha 5 caso tot =! '+0', preenche com NA
# para todos os conjuntos

eval(parse(text = paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "$tot[5] == '+0'")))

# ==== rodar for loop c5 ====
for (i in 1:nrow(df_so_concorridos)){
  
if(eval(parse(text = paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "$tot[5] == '+0'")))){
  "true"
} else {"false"
  eval(parse(text = paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2], "[5,] <- NA")))
} # fecha if
}; print("fim for loop c5") # fecha for loop
# se já tiver $tot[2]==NA, vai dar erro, mas tudo bem

# ==============================================================================
# Conferir

(eval(parse(text = paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2]," %>% print"))))

# print todos os 63 conjuntos 
for (i in 1:nrow(df_so_concorridos)){
  # print curso e ano
  print(paste(df_so_concorridos[i,1],df_so_concorridos[i,2]))
  # print comp_vagas CURSO _ ANO
  (eval(parse(text = paste0("comp_vagas_", df_so_concorridos[i, 1], "_", df_so_concorridos[i, 2]," %>% print"))))
}

# =================================== 2023-09-27
# Criar lista dos dataframes a serem exportados, usando ChatGPT

# Create an empty list to store the dataframes
comp_vagas_list <- list()

# Create a vector to store the custom names
custom_names <- character(0)

# Iterate over each row in df_so_concorridos
for (i in 1:nrow(df_so_concorridos)) {
  # Construct the dataframe name based on Curso and Ano
  dataframe_name <- paste0("comp_vagas_",df_so_concorridos[i, 1],"_",df_so_concorridos[i, 2])
  
  # Get the dataframe by its name
  comp_vagas_df <- get(dataframe_name)
  
  # Store the dataframe in the list
  comp_vagas_list[[i]] <- comp_vagas_df
  
  # Store the custom name
  custom_names[i] <- paste0(" ",df_so_concorridos[i, 1],"_",df_so_concorridos[i, 2])
}

# Set the names of the list using the custom names
names(comp_vagas_list) <- custom_names

# Confere
comp_vagas_list

# ==============================================================================


# Exportar como tabelas em documento Word
if(quero_imprimir){ # quero imprimir

# install.packages("officer","flextable")
library(officer)
library(flextable)  # for formatting tables

# Function to replace NAs with "NA"
replace_nas <- function(x) {
  x[is.na(x)] <- "NA"
  x
}

{ # salvar documento Word
# Create a Word document
doc <- read_docx()
  
# Counter to track the number of tables added
table_counter <- 0

# Preencher documento
# Iterate over each dataframe and add it as a table to the Word document
for (name in names(comp_vagas_list)) { # abre for loop
  # Increment the table counter
  table_counter <- table_counter + 1
  
  # Add a section header with the dataframe name
  doc <- doc %>%
    body_add_par(paste0("Alocação do conjunto", name), style = "centered")
  
  # Add row names as an additional column, replace NAs
  comp_vagas_conjunto <- cbind(Modelo=c("c1","dif c2","dif c3","dif c4","dif c5"),comp_vagas_list[[name]])
  comp_vagas_conjunto <- replace_nas(comp_vagas_conjunto)
  
  
  # Convert the dataframe to a flextable and format it
  flex_table <- flextable::flextable(comp_vagas_conjunto) %>%
    autofit() %>%  # Autofit to make sure it fits within the page width
    width(width = 0.45)   # Set a specific width for the table

  # Set a specific width for the first column (adjust the width as needed)
  flex_table <- flex_table %>%
    width(j = 1, width = 0.7)
  
  # Add the formatted table to the Word document
  doc <- doc %>%
    body_add_flextable(value = flex_table) %>%
    body_add_par("")  # Add a blank line between tables
  
  # Check if we've added four tables and insert one blank line
  if (table_counter %% 4 == 0 && table_counter < length(names(comp_vagas_list))) {
    doc <- doc %>%
      body_add_par("")  # Insert a blank line
    
  } # fecha Check if we've added four tables
  
  # Check if we've added four tables and insert one blank line
  if (table_counter  ==  length(names(comp_vagas_list))) {
    doc <- doc %>%
      body_add_par(paste("Documento gerado em", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))  # Insert a blank line
    
  } # fecha Check if we've added four tables
} # fecha for loop

# Wait
print("doc is done, ready to export")

# obtém tempo atual, sem segundos, para nome do arquivo
tempo_atual<-format(Sys.time(), "%Y-%m-%d-%H-%M")

# Nome do documento
save_path <- paste0("output_analysis_02_2_comp_vagas_", tempo_atual, ".docx")

# Save the Word document
print(doc, target = save_path); print("Saved the Word document.")
} # Fim de salvar documento word
} # Fim de quero imprimir

# Depois de gerar documento Word, colorir células usando
# macro Word AlterarCoresv09mais10 de output_analysis_02_2_comp_vagas.txt
# ==============================================================================
# Rascunho

# styles:
# c('Normal', 'heading 1', 'heading 2', 'heading 3','centered', 'Image Caption', 'Table Caption', 'toc 1', 'toc 2', 'Balloon Text', 'graphic title', 'table title')
