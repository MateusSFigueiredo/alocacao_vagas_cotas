# ==============================================================================
# Arquivo: est_desc_01.R

#
# Modificado em: 2023-04-12.
# Autor: Mateus Silva Figueiredo
#
# Utiliza dados_ufv carregado por data_04_carregar_dados_UFV

### OBJETIVO A FAZER:
# Gera estatística descritiva de cada curso, ano e modalidade

# Faz:
# gráfico boxplot na horizontal. cada gráfico para um curso.
# compara ano a ano a nota mínima, mediana e máxima de cada curso
# compara AC com cotas

# Falta:
# exportar imagens

# ==============================================================================
# Pacotes
library(ggplot2)
library(RColorBrewer)

# ==============================================================================

# Preparação
# ------------------------------------------------------------------------------
setwd("C:/Users/Mateus/Desktop/R/alocacao_vagas_cotas")

# Carregar dados com data_04_carregar_dados_UFV.R
por_curso <- F   # deseja separar candidatos por curso? obrigatório
por_ano   <- F   # deseja separar candidatos por ano? opcional
source("data_04_carregar_dados_UFV.R") # cria ~10 objetos
# data_04 faz setwd da pasta /privado
# 
# ==============================================================================

dados_ufv %>% colnames() -> colunas

# ------------------------------------------------------------------------------

# Identificar lista de lista_cursos.
length(lista_cursos) # 70 cursos
lista_cursos[5]

# ==============================================================================
# Apenas convocados em 1a chamada = dados_convocados

dados_ufv[Numero_Chamada_Convocacao == 1] -> dados_convocados

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
dados_convocados$pub<- gsub("TRUE","cota",dados_convocados$pub)
dados_convocados$pub<- gsub("FALSE","AC",dados_convocados$pub)


# ==============================================================================
# =========================== Gráficos==========================================
# ==============================================================================

# ==============================================================================
# Boxplot

# Selecionar um curso
# dados_curso <- dados_convocados[Curso == "MEDICINA"]
# dados_curso <- dados_convocados[Curso == lista_cursos[8]]
# i <- 25


# fazer loop com todos os cursos
 for (i in 1:length(lista_cursos)){

# a cada loop, pegar um curso diferente
dados_curso <- dados_convocados[Curso == lista_cursos[i]]

# remover escrito SISU do Processo_Seletivo e deixar apenas ano
dados_curso$Processo_Seletivo <- gsub("SISU", " ", dados_curso$Processo_Seletivo)


# ------------------------------------------------------------------------------
# Boxplot no ggplot

# boxplot horizontal funcionando ok
grafico <<- ggplot(dados_curso, aes(y = fct_rev(Processo_Seletivo), x = nota, fill = pub)) +
  geom_boxplot() + 
  scale_fill_manual(values = c("red", "blue")) +
  labs(
    title = gsub("_"," ",dados_curso$Curso[1]), # remove _ do curso
       x = "Nota", y = "Ano") +
#  scale_y_discrete(labels = year_labels_skip) +
  #  stat_summary(fun="mean", color="white", shape=16,size=0.1)+ # ponto branco = média
  theme_classic() + # fundo cinza com grade, ou fundo branco
  coord_cartesian(xlim = range(dados_curso$nota)) +
  theme(axis.ticks.y = element_blank()) +
  labs(fill = "Escola pública?"); print(grafico)

 } # fim do loop de lista_cursos

# ==============================================================================


# ==============================================================================
# Referências

# Boxplot in Base R # Boxplot no base R
# https://statisticsglobe.com/boxplot-in-r

# Introduction to ggridges # Ridgelines
# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html