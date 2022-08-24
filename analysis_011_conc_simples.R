# ==============================================================================
# Arquivo: analysis_011_conc_simples.R
# Separa candidatos seguindo concorrência simples = modelo 1
# Cada candidato só preenche vaga da modalidade de inscrição
# 
# Modificado em 2022-08-13-23-__.
# Autor: Mateus Silva Figueiredo
# ==============================================================================
#
# Dicionário
# Inputs:
#
# Outputs:
# analise_n1 = nota máxima, média e mínima de cada categoria com modelo 1
# analise_v1 = número de candidatos convocados por modalidade com modelo 1
#
# ==============================================================================
#
# Carregar funções necessárias
library(dplyr)
belch3 <- function(x, y, z) 
  {eval(parse(text=(paste(x, y, z,sep=""))))}
belch5 <- function(x, y, z, a, b) 
  {eval(parse(text=(paste(x, y, z, a, b,sep=""))))}
#
#
# Utiliza função gera_candidatos() para gerar candidatos de todas as mod
# 
#set.seed(4)
#set.seed(NULL)
# ==============================================================================
# 
# Criar candidatos

{ #criar data.frame candidatos em branco
  candidatos <- data.frame()
  
  # roda função gera_candidatos com for loop
  for (i in 1:length(mod)){gera_candidatos(mod=mod[i])}
  
  # colocar candidatos em ordem decrescente de nota
  candidatos<<-candidatos[order(candidatos[,2],decreasing=T),]
}
# ==============================================================================

### Roda modelo 1 
### Concorrencia simples para qualquer modalidade = sufixo 1

# cria df de aprovados por modalidade, e depois coloca nome nas colunas
belch3 ("aprovados_",mod,"<<-data.frame(matrix(ncol = ncol(candidatos), nrow = 0))")
belch3 ("colnames(aprovados_",mod,")<<- colnames(candidatos)")

# cria df de lista, e depois preenche a lista
belch3 ("lista_",mod,"<<-data.frame()") #df vazio de lista total por modalidade
belch5 ("lista_",mod,"<<- candidatos %>% filter (mod_ins=='",mod,"')") #preenche

#valores nvagas para cada mod
for (i in 1:length(nvagas)){
  belch5 ("nvagas_",mod[i],"<<-nvagas[",i,"]")
}

# ------------------------------------------------------------------------------

###Preencher dfs aprovados de todas as modalidades, concorrencia simples

{eval(parse(text=(paste(
  'for (i in 1:nvagas_', mod, ')',
  '{aprovados_',mod,'[i,]<- lista_',mod,' %>% slice(1);',
  'aprovados_',mod,'[i,4]<-"',mod,'";',
  'lista_',mod,'<<-lista_',mod,'[2:(nrow(lista_',mod,')),]}',
  sep=""))))
  #print(i)
}

# head(aprovados_A0);tail(aprovados_A0) #aprovados em A0
# head(lista_A0) #lista de espera em A0

# ------------------------------------------------------------------------------

# Analise - candidatos aprovados
# Cria data.frame de todos os aprovados para ver numero de cotistas
aprovados<-data.frame()
belch3("aprovados<<-rbind(aprovados,aprovados_",mod,")")
aprovados.1<-aprovados

# Cria data.frame analise_v1 para concorrencia 1 (simples) para as vagas
analise_v1<-data.frame(matrix(ncol = length(mod), nrow = 1),
                       row.names=c("n"))
colnames(analise_v1) <- c(mod)

# preenche as 9 colunas com numero de convocados
{eval(parse(text=(paste(
  'analise_v1[,"',mod,'"]<-aprovados %>% filter (mod_ins=="',mod,'") %>% nrow()',                   
  sep=""))))}

# ------------------------------------------------------------------------------

# Analise - notas
# Cria data.frame analise_n1 para concorrencia 1 (simples) para as notas
analise_n1<-data.frame(matrix(ncol = length(mod)+1, nrow = 3),
                       row.names=c("max","mean","min"))
colnames(analise_n1) <- c("geral",mod)

#preenche as 9 colunas com nota maxima, media e minima
{eval(parse(text=(paste(
  'analise_n1[,"',mod,'"]<-c(max(aprovados_',mod,'$nota),
                        mean(aprovados_',mod,'$nota),
                        min(aprovados_',mod,'$nota))',                   
  sep=""))))}

analise_n1$geral<-c(max(aprovados$nota),
                    mean(aprovados$nota),
                    min(aprovados$nota))
rownames(analise_n1)<-c('max1','mean1','min1')

rm("aprovados_A0","aprovados_L1","aprovados_L2","aprovados_L5","aprovados_L6",
   "aprovados_L10","aprovados_L14")
rm("lista_A0","lista_L1","lista_L2","lista_L5","lista_L6",
   "lista_L10","lista_L14")

analise_n1
