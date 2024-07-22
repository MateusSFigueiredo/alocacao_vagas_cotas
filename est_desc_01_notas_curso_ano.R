# ==============================================================================
# Arquivo: est_desc_01_notas_curso_ano.R
#
# Modificado em: 2024-05-14

# Autor: Mateus Silva Figueiredo
# diff 2024-05-14: boxplot.stats no finalzinho, para análises
#
# Utiliza dados_ufv carregado por data_04_carregar_dados_ufv

# ==============================================================================

### Função:
# Gera estatística descritiva de cada curso, ano e modalidade
# Cria boxplot com notas dos convocados na 1a chamada de cada curso,
# de cada ano, separando AC e cotas.

# Faz:
# gráfico boxplot na horizontal. cada gráfico para um curso.
# compara ano a ano a nota mínima, mediana e máxima de cada curso
# compara AC com cotas
# exporta imagens. Largura da barra dos boxplots está ok.

# Observação:
# cada boxplot tem uma amplitude de notas diferente.

# ==============================================================================
# Pacotes
library(ggplot2)
# library(RColorBrewer)

# ==============================================================================

# Preparação
# ------------------------------------------------------------------------------
setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")

# Carregar dados com data_04_carregar_dados_UFV.R
por_curso <- T   # deseja separar candidatos por curso? obrigatório
por_ano   <- F   # deseja separar candidatos por ano? opcional
source("data_04_carregar_dados_UFV.R") # cria ~10 objetos
# 
# ==============================================================================

dados_ufv %>% colnames() -> colunas

# ------------------------------------------------------------------------------

# Identificar lista de lista_cursos.
length(lista_cursos) # 70 cursos
# lista_cursos[5]

# ==============================================================================
# Apenas convocados em 1a chamada = dados_convocados

dados_convocados <- dados_ufv[Numero_Chamada_Convocacao == 1]

# Selecionar colunas
colunas<-c("id","Processo_Seletivo","nota",
"Modalidade_Inscrita","Numero_Chamada_Convocacao" ,"Modalidade_Convocada",
"Curso","Centro",
"mod_ins","mod_con",
"pub","bxa","ppi","pcd")

dados_convocados <- dados_convocados %>% select(all_of(colunas))

# group = ano + escola pública ou não
dados_convocados$group <- paste0(dados_convocados$Processo_Seletivo,dados_convocados$pub)

dados_convocados$ano <- paste0(dados_convocados$Processo_Seletivo,dados_convocados$pub)


# transforma group em apenas ano + grupo (cota ou AC) 
dados_convocados$group<- gsub("FALSE"," AC",dados_convocados$group)
dados_convocados$group<- gsub("TRUE"," cota",dados_convocados$group)
dados_convocados$group<- gsub("SISU","",dados_convocados$group)

# transforma coluna pub em algo bonito pro gráfico plotar
dados_convocados$pub<- gsub("TRUE","Cota",dados_convocados$pub)
dados_convocados$pub<- gsub("FALSE","Ampla Concorrência",dados_convocados$pub)


# ==============================================================================
# =========================== Gráficos==========================================
# ==============================================================================

# ==============================================================================
# Boxplot
# conferir se vai salvar imagens

# Opção 1 - Selecionar um curso
# dados_curso <- dados_convocados[Curso == "EDUCACAO_FISICA"] # com _ no nome
 dados_curso <- dados_convocados[Curso == lista_cursos[8]]
# i <- 25
# {
  
# # Opção 2 - fazer loop com todos os cursos, gerar imagem e salvar
#  for (i in 1:length(lista_cursos)){
# # # a cada loop, pegar um curso diferente
#  dados_curso <- dados_convocados[Curso == lista_cursos[i]]

# # Opção 3 - fazer loop com cursos listados, gerar imagem e salvar
# lista_cursos2 <- c("EDUCACAO_FISICA","EDUCACAO_FISICA_BACHARELADO","EDUCACAO_FISICA_LICENCIATURA")
# for (i in 1:length(lista_cursos2)){
# # # # a cada loop, pegar um curso diferente
# dados_curso <- dados_convocados[Curso == lista_cursos2[i]]
   
   
# --------------------------------------------------------------------
# após escolher opção 1 ou 2 ou 3, rodar tudo:
# remover escrito SISU do Processo_Seletivo e deixar apenas ano
dados_curso$Processo_Seletivo <- gsub("SISU", "", dados_curso$Processo_Seletivo)

# ==============================================================================
# número de convocados em 1a chamada em cada ano # usando Chat GPT
# objetivo: colocar n de convocados a cada ano do lado de cada boxplot

library(dplyr)

# Initialize an empty list to store the results
results_list <- list()

# Iterate through each year from 2013 to 2022
for (year in 2013:2022) {
  # Replace the year in the condition and calculate the number of rows
  num_convocations <- dados_curso[Numero_Chamada_Convocacao == 1 & Processo_Seletivo == year, ] %>% nrow()
  
  # Store the result in the list
  results_list[[as.character(year)]] <- num_convocations
}

# Convert the list to a dataframe n_convocados
n_convocados <- data.frame(Year = as.numeric(names(results_list)), Convocations = unlist(results_list))

# ==============================================================================
# Boxplot no ggplot

titulo_grafico <- gsub("_"," ",dados_curso$Curso[1]) # remove _ do curso

# boxplot horizontal funcionando ok
grafico <<- ggplot(dados_curso, aes(y = fct_rev(Processo_Seletivo), x = nota, fill = pub)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("red", "blue")) +
  guides(fill = guide_legend(reverse = TRUE)) +  # Reverses the order of the legend
    labs(
    title = titulo_grafico, 
       x = "Nota (convocados em 1ª chamada)", y = "Ano") +
#  scale_y_discrete(labels = year_labels_skip) +
  #  stat_summary(fun="mean", color="white", shape=16,size=0.1)+ # ponto branco = média
  theme_classic() + # fundo cinza com grade, ou fundo branco
  coord_cartesian(xlim = range(dados_curso$nota)) + # automático
#  coord_cartesian(xlim = c(397, 733)) +  # arbitrário, para Educação Física
  theme(axis.ticks.y = element_blank()) +
  labs(fill = "Escola pública?") 

print(grafico)

# } # fim do loop de lista_cursos se não quiser salvar gráficos
#
# ------------------------------------------------------------------------------
# salvar grafico boxplot como imagem png

if(F){ # deseja salvar imagens? T = sim. F = não.
# criar pasta para gráficos
pasta_imagens <- "imagens_est_desc_01_notas_curso_ano" #nome da pasta
if (!dir.exists(pasta_imagens)) {
  dir.create(pasta_imagens)
} # cria pasta

# nome do arquivo
# image_filename <- paste0(titulo_grafico, "_notas_ano.png") # alfabético
image_filename <- paste0(dados_curso$Centro[1],"_",
                         titulo_grafico, "_notas_ano.png") # por centro

# determina altura do gráfico
n_anos <- dados_curso$Processo_Seletivo%>%unique()%>%length() # numero de anos
# altura <- n_anos/6+(4/3) # ajusta altura
# 10 anos => altura 3. 4 anos => altura 2

# altura <- n_anos/5+1 # ajusta altura
# 5 anos => altura 2. 3 anos pouco maior que o 10 anos.

altura <- n_anos*2/9+7/9 # ajusta altura de cada barra do boxplot
# 5.5 anos => altura 2. 3 anos mesma largura que 10 anos. Perfeito.

# salva imagem
ggsave(file.path(pasta_imagens, image_filename), plot = grafico, width = 8, height = altura, dpi = 300)
} # fim da parte de salvar imagem

# } # fim do loop de lista_cursos após salvar imagens de gráficos

# ==============================================================================
# Analisar dados de cada boxplot
# boxplot.stats

dados_curso[group=="2014 AC"]$nota |> sort() # notas de determinado grupo, crescente
dados_curso[group=="2014 AC"]$nota |> boxplot.stats()

# ==============================================================================
# Referências

# Boxplot in Base R # Boxplot no base R
# https://statisticsglobe.com/boxplot-in-r

# boxplot.stats: Box Plot Statistics
# https://rdrr.io/r/grDevices/boxplot.stats.html