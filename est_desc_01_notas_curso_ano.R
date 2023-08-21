# ==============================================================================
# Arquivo: est_desc_01_notas_curso_ano.R

#
# Modificado em: 2023-08-21.
# Autor: Mateus Silva Figueiredo
#
# Utiliza dados_ufv carregado por data_04_carregar_dados_UFV

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
# data_04 faz setwd da pasta /privado
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

# Selecionar um curso - opção 1
# dados_curso <- dados_convocados[Curso == "FISICA"]
# dados_curso <- dados_convocados[Curso == lista_cursos[8]]
# i <- 25
# {
  
# # fazer loop com todos os cursos, gerar imagem e salvar - opção 2
 for (i in 1:length(lista_cursos)){
# # a cada loop, pegar um curso diferente
 dados_curso <- dados_convocados[Curso == lista_cursos[i]]

# --------------------------------------------------------------------
# após escolher opção 1 ou 2, rodar tudo:
# remover escrito SISU do Processo_Seletivo e deixar apenas ano
dados_curso$Processo_Seletivo <- gsub("SISU", " ", dados_curso$Processo_Seletivo)


# ------------------------------------------------------------------------------
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
  coord_cartesian(xlim = range(dados_curso$nota)) +
  theme(axis.ticks.y = element_blank()) +
  labs(fill = "Escola pública?"); print(grafico)

# } # fim do loop de lista_cursos se não quiser salvar gráficos

#
# ------------------------------------------------------------------------------
# salvar grafico boxplot como imagem png

if(T){ # deseja salvar imagens? T = sim. F = não.
# criar pasta para gráficos
pasta_imagens <- "imagens_boxplot_notas_curso_ano" #nome da pasta
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

} # fim do loop de lista_cursos após salvar imagens de gráficos

# ==============================================================================
# Referências

# Boxplot in Base R # Boxplot no base R
# https://statisticsglobe.com/boxplot-in-r

# Introduction to ggridges # Ridgelines
# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html