# ==============================================================================
# Arquivo: analysis_013_conc_conc_cotas.R
# Separa candidatos seguindo concorrência concomitante, cotas primeiro = modelo3
# Primeiro preenche as vagas das cotas
# Depois preenche vagas de Ampla Concorrência com todos os candidatos
# 
# Modificado em 2022-08-13-23-__.
# Autor: Mateus Silva Figueiredo
# ==============================================================================
#
# Dicionário
# Inputs:
#
# Outputs:
# analise_n3 = nota máxima, média e mínima de cada categoria com modelo 3
# analise_v3 = número de candidatos convocados por modalidade com modelo 3
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


################################################################################
############## Concorrencia simultanea ##############
#############  preenche cotas primeiro = sufixo 3 #############

#View(candidatos)
lista_todos <- candidatos

#df vazia de aprovados por modalidade, e depois nome nas colunas
belch3 ("aprovados_",mod,"<<-data.frame(matrix(ncol = ncol(candidatos), nrow = 0))")
belch3 ("colnames(aprovados_",mod,")<<- colnames(candidatos)")

#df vazias de lista
belch3 ("lista_",mod,"<<-data.frame()") #df vazio de lista total por modalidade

#vetores nvagas para cada mod
for (i in 1:length(nvagas)){
  belch5 ("nvagas_",mod[i],"<<-nvagas[",i,"]")
}

#preenche listas para cada mod
belch5 ("lista_",mod,"<<- candidatos %>% filter (mod_ins=='",mod,"')") #preenche

#preenche dfs de aprovados para cada mod_cotas
{eval(parse(text=(paste(
  'for (i in 1:nvagas_', mod_cotas, ')',
  '{aprovados_',mod_cotas,'[i,]<- lista_',mod_cotas,' %>% slice(1);',
  'aprovados_',mod_cotas,'[i,4]<-"',mod_cotas,'";', #preenche mod_con
  'lista_',mod_cotas,'<<-lista_',mod_cotas,'[2:(nrow(lista_',mod_cotas,')),]}',                     
  sep=""))))}

#View(lista_todos)

#cria lista_todos2 com todos os A0 e os cotistas nao aprovados ainda
lista_todos2<-lista_A0
for (i in length(mod_cotas)){
  belch3("lista_todos2<<-rbind(lista_todos2,lista_",mod_cotas,")")
}

#coloca lista_todos2 em ordem decrescente
lista_todos2<-lista_todos2[order(lista_todos2[,2],decreasing=T),]

#preenche aprovados_A0 com nvagas_A0 primeiros da lista_todos2
#remove esses estudantes de lista_todos2
for (i in 1:nvagas_A0) #abre o loop
  #linha 1 de lista_todos2 vai para linha i de aprovados_A0
{aprovados_A0[i,] <- lista_todos2 %>% slice(1)
aprovados_A0[i,4]<-"A0"; #preenche mod_con
#linha 1 de candidatos eh deletada, pois candidato selecionado retirado da lista
lista_todos2<-lista_todos2[2:(nrow(lista_todos)),]
#print(i) #verificar se loop esta ok
}



#Analise - inscritos aprovados
# #Cria data.frame de todos os aprovados para ver numero de cotistas
aprovados<-data.frame()
belch3("aprovados<<-rbind(aprovados,aprovados_",mod,")")
aprovados.3<-aprovados

#Cria data.frame analise_v3 para concorrencia 3 (cotas primeiro) para as vagas
analise_v3<-data.frame(matrix(ncol = length(mod), nrow = 1),
                       row.names=c("n?"))
colnames(analise_v3) <- c(mod)

#preenche as 9 colunas com numero de convocados
{eval(parse(text=(paste(
  'analise_v3[,"',mod,'"]<-aprovados %>% filter (mod_ins=="',mod,'") %>% nrow()',                   
  sep=""))))}

#Analise - notas
#Cria data.frame analise_n3 para concorrencia 3 (cotas primeiro) para as notas
analise_n3<-data.frame(matrix(ncol = length(mod)+1, nrow = 3),
                       row.names=c("max","mean","min"))
colnames(analise_n3) <- c("geral",mod)

#preenche as 9 colunas com nota maxima, media e minima
{eval(parse(text=(paste(
  'analise_n3[,"',mod,'"]<-c(max(aprovados_',mod,'$nota),
                        mean(aprovados_',mod,'$nota),
                        min(aprovados_',mod,'$nota))',                   
  sep=""))))}


analise_n3$geral<-c(max(aprovados$nota),
                    mean(aprovados$nota),
                    min(aprovados$nota))
rownames(analise_n3)<-c('max3','mean3','min3')



print(analise_n3[2,1]>=analise_n1[2,1])
print(analise_n3[2,1]>=analise_n2[2,1])

