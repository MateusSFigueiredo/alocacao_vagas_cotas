# ==============================================================================
# Arquivo: analysis_01_todas_conc.R
# Roda três modelos de alocação de vagas
#
# Separa candidatos seguindo concorrência simples = modelo 1
# Cada candidato só preenche vaga da modalidade de inscrição
#
# Separa candidatos seguindo concorrência concomitante, AC primeiro = modelo 2
# Primeiro preenche vagas de Ampla Concorrência com todos os candidatos
# Depois preenche as vagas das cotas
#
# Separa candidatos seguindo concorrência concomitante, cotas primeiro = modelo3
# Primeiro preenche as vagas das cotas
# Depois preenche vagas de Ampla Concorrência com todos os candidatos
#
# Modificado em 2023-08-23.
# Autor: Mateus Silva Figueiredo
# ==============================================================================

# Avisos.
# Dá erro se tiver mais vaga do que candidato para alguma modalidade.

# ==============================================================================
#
# Dicionário
# Inputs:
#
# Outputs:
# analise_n1 = nota máxima, média e mínima de cada categoria com modelo 1
# analise_v1 = número de candidatos convocados por modalidade com modelo 1
#
# analise_n2 = nota máxima, média e mínima de cada categoria com modelo 2
# analise_v2 = número de candidatos convocados por modalidade com modelo 2
#
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
mod <- c("A0","L1","L2","L5","L6", "L9", "L10", "L13", "L14")
mod_cotas <- mod[mod!= "A0"] # cria mod_cotas sem A0
# 
# ==============================================================================
# Utiliza função gera_candidatos() para gerar candidatos de todas as mod
#
#set.seed(4) # caso queira ser determinístico. Qualquer número serve.
#set.seed(NULL) # caso queira ser aleatório.
# 
# Criar candidatos aleatórios # apenas caso ainda não haja df candidatos
# exige vetor mod e função gera_candidatos

# { #criar data.frame candidatos em branco
#   candidatos <- data.frame()
# 
#   # roda função gera_candidatos com for loop
#   for (i in 1:length(mod)){gera_candidatos(mod=mod[i])}
# 
#   # colocar candidatos em ordem decrescente de nota
#   candidatos<<-candidatos[order(candidatos[,2],decreasing=T),]
# }


# ==============================================================================
# # Criar candidatos a partir de dados_[ano] da UFV

# Carregar dados com data_04_carregar_dados_UFV.R
por_curso <- F   # deseja separar candidatos por curso? 
por_ano   <- T   # deseja separar candidatos por ano? obrigatório
source("data_04_carregar_dados_UFV.R") # F,T cria ~20 objetos

#
dados_ano <- dados_2022 # colocar ano que quiser
dados_ano$Curso %>% unique() -> cursos; length(cursos)
ncurso<-66 # colocar número que quiser, até o número de crrsos
cursos[ncurso] 
subset(dados_ano,Curso==cursos[ncurso]
       ,select=c(id,nota,
                 Numero_Chamada_Convocacao,
                 mod_ins,mod_con)
       ) -> candidatos

# gerar nvagas a partir de dados_[ano] # PARECE ERRADO
nvagas<-c(
sum(candidatos[Numero_Chamada_Convocacao==1]$mod_con=="A0"),
sum(candidatos[Numero_Chamada_Convocacao==1]$mod_con=="L01"),
sum(candidatos[Numero_Chamada_Convocacao==1]$mod_con=="L02"),
sum(candidatos[Numero_Chamada_Convocacao==1]$mod_con=="L05"),
sum(candidatos[Numero_Chamada_Convocacao==1]$mod_con=="L06"),
sum(candidatos[Numero_Chamada_Convocacao==1]$mod_con=="L09"),
sum(candidatos[Numero_Chamada_Convocacao==1]$mod_con=="L10"),
sum(candidatos[Numero_Chamada_Convocacao==1]$mod_con=="L13"),
sum(candidatos[Numero_Chamada_Convocacao==1]$mod_con=="L14")
)
nvagas
# para regularizar, fingir que nenhum candidato foi convocado ainda
candidatos$mod_con <- 0
cursos[ncurso]

# ==============================================================================
# Confere candidatos
head(candidatos);tail(candidatos);summary(candidatos)
candidatos %>% count(mod_ins)
candidatos %>% count(mod_con)

# ==============================================================================
# Roda modelo 1
print("Roda modelo 1")
# Concorrencia simples para qualquer modalidade = sufixo 1
# ==============================================================================

# cria df de aprovados por modalidade, e depois coloca nome nas colunas
belch3 ("aprovados_",mod,"<<-data.frame(matrix(ncol = ncol(candidatos), nrow = 0))")
belch3 ("colnames(aprovados_",mod,")<<- colnames(candidatos)")

# cria df de lista, e depois preenche a lista
belch3 ("lista_",mod,"<<-data.frame()") #df vazio de lista total por modalidade
belch5 ("lista_",mod,"<<- candidatos %>% filter (mod_ins=='",mod,"')") #preenche

# cria valores de nvagas para cada mod (ex. nvagas_A0) com base em nvagas
for (i in 1:length(nvagas)){
  belch5 ("nvagas_",mod[i],"<<-nvagas[",i,"]")
}

# ------------------------------------------------------------------------------

# Preenche dfs aprovados de todas as modalidades, concorrencia simples
# (aprovados_A0 até aprovados_L14)

{eval(parse(text=(paste(
  'for (i in 1:nvagas_', mod, ')',
  '{aprovados_',mod,'[i,]<- lista_',mod,' %>% slice(1);',
  'aprovados_',mod,'[i,4]<-"',mod,'";',
  'lista_',mod,'<<-lista_',mod,'[2:(nrow(lista_',mod,')),]}',
  sep=""))))
  #print(i)
}

# ERRO. Error in x[[jj]][iseq] <- vjj : replacement has length zero

 head(aprovados_A0);tail(aprovados_A0) #aprovados em A0
 head(lista_A0) #lista de espera em A0

# Pode dar mensagem de erro. Acho que é caso tenha 0 vagas em alguma modalidade.
# ERRO: Primeiro lugar da lista de espera está igual o último lugar dos aprovados
 
# ------------------------------------------------------------------------------

# Analise - candidatos aprovados
# Cria data.frame de todos os aprovados para ver numero de cotistas
aprovados<-data.frame()
belch3("aprovados<<-rbind(aprovados,aprovados_",mod,")")
aprovados.1<-aprovados # aprovados.1 = aprovados no modelo 1

# Cria data.frame analise_v1 para concorrencia modelo 1 para as vagas
analise_v1<-data.frame(matrix(ncol = length(mod), nrow = 1),
                       row.names=c("n1"))
colnames(analise_v1) <- c(mod)

# preenche as 9 colunas de analise_v1 com numero de convocados
{eval(parse(text=(paste(
  'analise_v1[,"',mod,'"]<-aprovados %>% filter (mod_ins=="',mod,'") %>% nrow()',                   
  sep=""))))}

# total de vagas preenchidas por grupo social
analise_v1$tot <- sum(analise_v1[1:9]) # soma de todas as mod
analise_v1$pub <- sum(analise_v1[2:9]) # soma das mod de cotas
analise_v1$bxa <- sum(analise_v1$L1, analise_v1$L2, # soma de baixa renda
                      analise_v1$L9, analise_v1$L10)
analise_v1$ppi <- sum(analise_v1$L2, analise_v1$L6, # soma das cotas para ppi
                      analise_v1$L10,analise_v1$L14)
analise_v1$pcd <- sum(analise_v1$L9, analise_v1$L10, # soma das cotas para pcd
                      analise_v1$L13,analise_v1$L14)

# ------------------------------------------------------------------------------

# Analise - notas
# Cria data.frame analise_n1 para modelo 1 para as notas
analise_n1<-data.frame(matrix(ncol = length(mod)+1, nrow = 3),
                       row.names=c("max","mean","min"))
colnames(analise_n1) <- c("geral",mod)

# preenche as 9 colunas com nota maxima, media e minima
{eval(parse(text=(paste(
  'analise_n1[,"',mod,'"]<-c(max(aprovados_',mod,'$nota),
                        mean(aprovados_',mod,'$nota),
                        min(aprovados_',mod,'$nota))',                   
  sep=""))))}

analise_n1$geral<-c(max(aprovados$nota),
                    mean(aprovados$nota),
                    min(aprovados$nota))
rownames(analise_n1)<-c('max1','mean1','min1') # pois modelo 1

analise_n1$cotas<-c(
  subset(aprovados.1,aprovados.1$mod_con!="A0")$nota%>%max(),
  subset(aprovados.1,aprovados.1$mod_con!="A0")$nota%>%mean(),
  subset(aprovados.1,aprovados.1$mod_con!="A0")$nota%>%min()
) # cria coluna de todos os cotistas

# remove variáveis desnecessárias
rm("aprovados_A0","aprovados_L1","aprovados_L2","aprovados_L5","aprovados_L6",
   "aprovados_L9","aprovados_L10","aprovados_L13","aprovados_L14","aprovados")
rm("lista_A0","lista_L1","lista_L2","lista_L5","lista_L6",
   "lista_L9","lista_L10","lista_L13","lista_L14")

analise_v1;analise_n1 
# analise_v1 = vagas preenchidas 
# analise_n1 = notas 

# Fim do modelo 1

# ==============================================================================
# Roda modelo 2
print("Roda modelo 2")
# Concorrencia simultanea, preenche AC primeiro = sufixo 2
# ==============================================================================

#View(candidatos)

# cria lista_todos para poder manipular a lista sem mexer nos candidatos
lista_todos <- candidatos

# cria várias dfs vazias de aprovados por modalidade (ex. aprovados_L2)
belch3 ("aprovados_",mod,"<<-data.frame(matrix(ncol = ncol(candidatos),
        nrow = 0))")

# coloca nomes nas colunas dessas dfs
belch3 ("colnames(aprovados_",mod,")<<- colnames(candidatos)")

# cria df vazias de lista de candidatos para manipular listas (ex. lista_L2)
belch3 ("lista_",mod,"<<-data.frame()")

# usa vetores nvagas para criar nvagas_[mod] para cada mod
for (i in 1:length(nvagas)){
  belch5 ("nvagas_",mod[i],"<<-nvagas[",i,"]")
}

# preenche aprovados_A0 com os nvagas_A0 primeiros da lista_todos
# remove esses estudantes de lista_todos
for (i in 1:nvagas_A0) #abre o loop
  # linha 1 de lista_todos vai para linha i de aprovados_A0
{aprovados_A0[i,] <- lista_todos %>% slice(1);
aprovados_A0[i,4]<-"A0"; # preenche mod_con
# linha 1 de candidatos eh deletada, pois candidato selecionado retirado da lista
lista_todos<-lista_todos[2:(nrow(lista_todos)),]
# print(i) #verificar se loop esta ok
}


# preenche listas para cada mod_cotas usando candidatos que ainda estão 
# em lista_todos, pois não foram aprovados em AC
belch5 ("lista_",mod_cotas,"<<- lista_todos %>% filter (mod_ins=='",mod_cotas,"')")

# preenche dfs de aprovados para cada mod_cotas
{eval(parse(text=(paste(
  'for (i in 1:nvagas_', mod_cotas, ')',
  '{aprovados_',mod_cotas,'[i,]<- lista_',mod_cotas,' %>% slice(1);',
  'aprovados_',mod_cotas,'[i,4]<-"',mod_cotas,'";', #preenche mod_con
  'lista_',mod_cotas,'<<-lista_',mod_cotas,'[2:(nrow(lista_',mod_cotas,')),]}',                     
  sep=""))))}

#obs: lista_A0 deve terminar vazio. não é preenchida, não é usada.

# ------------------------------------------------------------------------------
# Analises

# Analise - inscritos aprovados
# Cria data.frame de todos os aprovados para ver numero de cotistas
aprovados<-data.frame()
belch3("aprovados<<-rbind(aprovados,aprovados_",mod,")")
aprovados.2<-aprovados # aprovados.2 = aprovados no modelo 2

# Cria data.frame analise_v1 para concorrencia 1 (simples) para as vagas
analise_v2<-data.frame(matrix(ncol = length(mod), nrow = 1),
                       row.names=c("n2"))
colnames(analise_v2) <- c(mod)

# preenche as 9 colunas com numero de convocados
{eval(parse(text=(paste(
  'analise_v2[,"',mod,'"]<-aprovados %>% filter (mod_ins=="',mod,'") %>% nrow()',                   
  sep=""))))}

# total de vagas preenchidas por grupo social
analise_v2$tot <- sum(analise_v2[1:9]) # soma de todas as mod
analise_v2$pub <- sum(analise_v2[2:9]) # soma das mod de cotas
analise_v2$bxa <- sum(analise_v2$L1,analise_v2$L2, # soma de baixa renda
                      analise_v2$L9,analise_v2$L10)
analise_v2$ppi <- sum(analise_v2$L2,analise_v2$L6, # soma das cotas para ppi
                      analise_v2$L10,analise_v2$L14)
analise_v2$pcd <- sum(analise_v2$L9,analise_v2$L10, # soma das cotas para pcd
                      analise_v2$L13,analise_v2$L14)

# ------------------------------------------------------------------------------

# Analise - notas
# Cria data.frame analise_n2 para concorrencia 2 (AC primeiro) para as notas
analise_n2<-data.frame(matrix(ncol = length(mod)+1, nrow = 3),
                       row.names=c("max","mean","min"))
colnames(analise_n2) <- c("geral",mod)

# preenche as 9 colunas com nota maxima, media e minima
{eval(parse(text=(paste(
  'analise_n2[,"',mod,'"]<-c(max(aprovados_',mod,'$nota),
                        mean(aprovados_',mod,'$nota),
                        min(aprovados_',mod,'$nota))',                   
  sep=""))))}

analise_n2$geral<-c(max(aprovados$nota),
                    mean(aprovados$nota),
                    min(aprovados$nota))
rownames(analise_n2)<-c('max2','mean2','min2')

analise_n2$cotas<-c(
  subset(aprovados.2,aprovados.2$mod_con!="A0")$nota%>%max(),
  subset(aprovados.2,aprovados.2$mod_con!="A0")$nota%>%mean(),
  subset(aprovados.2,aprovados.2$mod_con!="A0")$nota%>%min()
) # cria coluna de todos os cotistas (convocados em modalidade de cotas)

# } #fim do loop (?)

# remove variáveis desnecessárias
rm("aprovados_A0","aprovados_L1","aprovados_L2","aprovados_L5","aprovados_L6",
   "aprovados_L9","aprovados_L10","aprovados_L13","aprovados_L14","aprovados")
rm("lista_A0","lista_L1","lista_L2","lista_L5","lista_L6",
   "lista_L9","lista_L10","lista_L13","lista_L14","lista_todos")

analise_v2;analise_n2 
# analise_v2 = vagas preenchidas 
# analise_n2 = notas 

# Fim do modelo 2

# ==============================================================================
# Roda modelo 3
print("Roda modelo 3")
# Concorrencia simultanea, preenche cotas primeiro = sufixo 3
# ==============================================================================

# View(candidatos)
lista_todos <- candidatos

# cria dfs vazia de aprovados por modalidade, e depois nome nas colunas
belch3 ("aprovados_",mod,"<<-data.frame(matrix(ncol = ncol(candidatos), nrow = 0))")
belch3 ("colnames(aprovados_",mod,")<<- colnames(candidatos)")

# df vazias de lista
belch3 ("lista_",mod,"<<-data.frame()") #df vazio de lista total por modalidade

# cria vetores nvagas para cada mod
for (i in 1:length(nvagas)){
  belch5 ("nvagas_",mod[i],"<<-nvagas[",i,"]")
}

# preenche listas para cada mod, de A0 até L14
belch5 ("lista_",mod,"<<- candidatos %>% filter (mod_ins=='",mod,"')") #preenche

# preenche dfs de aprovados para cada mod_cotas, de L1 até L14
{eval(parse(text=(paste(
  'for (i in 1:nvagas_', mod_cotas, ')',
  '{aprovados_',mod_cotas,'[i,]<- lista_',mod_cotas,' %>% slice(1);',
  'aprovados_',mod_cotas,'[i,4]<-"',mod_cotas,'";', #preenche mod_con
  'lista_',mod_cotas,'<<-lista_',mod_cotas,'[2:(nrow(lista_',mod_cotas,')),]}',                     
  sep=""))))}

# View(lista_todos)

# cria lista_todos2 com todos os A0 + os cotistas nao aprovados ainda
lista_todos2<-lista_A0
for (i in length(mod_cotas)){
  belch3("lista_todos2<<-rbind(lista_todos2,lista_",mod_cotas,")")
}

# coloca lista_todos2 em ordem decrescente
lista_todos2<-lista_todos2[order(lista_todos2[,2],decreasing=T),]

# preenche aprovados_A0 com nvagas_A0 primeiros da lista_todos2
# remove esses estudantes de lista_todos2
for (i in 1:nvagas_A0) #abre o loop
  #linha 1 de lista_todos2 vai para linha i de aprovados_A0
{aprovados_A0[i,] <- lista_todos2 %>% slice(1)
aprovados_A0[i,4]<-"A0"; #preenche mod_con com "A0"
# linha 1 de candidatos eh deletada, pois candidato selecionado retirado da lista
lista_todos2<-lista_todos2[2:(nrow(lista_todos2)),]
# print(i) #verificar se loop esta ok
}

# ------------------------------------------------------------------------------

# Analise - inscritos aprovados
# Cria data.frame de todos os aprovados para ver numero de cotistas
aprovados<-data.frame()
belch3("aprovados<<-rbind(aprovados,aprovados_",mod,")")
aprovados.3<-aprovados

# Cria data.frame analise_v3 para concorrencia 3 (cotas primeiro) para as vagas
analise_v3<-data.frame(matrix(ncol = length(mod), nrow = 1),
                       row.names=c("n3"))
colnames(analise_v3) <- c(mod)

# preenche as 9 colunas com numero de convocados
{eval(parse(text=(paste(
  'analise_v3[,"',mod,'"]<-aprovados %>% filter (mod_ins=="',mod,'") %>% nrow()',                   
  sep=""))))}

# total de vagas preenchidas por grupo social
analise_v3$tot <- sum(analise_v3[1:9]) # soma de todas as mod
analise_v3$pub <- sum(analise_v3[2:9]) # soma das mod de cotas
analise_v3$bxa <- sum(analise_v3$L1, analise_v3$L2, # soma de baixa renda
                      analise_v3$L9, analise_v3$L10)
analise_v3$ppi <- sum(analise_v3$L2, analise_v3$L6, # soma das cotas para ppi
                      analise_v3$L10,analise_v3$L14)
analise_v3$pcd <- sum(analise_v3$L9, analise_v3$L10, # soma das cotas para pcd
                      analise_v3$L13,analise_v3$L14)

# ------------------------------------------------------------------------------

# Analise - notas
# Cria data.frame analise_n3 para concorrencia 3 (cotas primeiro) para as notas
analise_n3<-data.frame(matrix(ncol = length(mod)+1, nrow = 3),
                       row.names=c("max","mean","min"))
colnames(analise_n3) <- c("geral",mod)

# preenche as 9 colunas com nota maxima, media e minima
{eval(parse(text=(paste(
  'analise_n3[,"',mod,'"]<-c(max(aprovados_',mod,'$nota),
                        mean(aprovados_',mod,'$nota),
                        min(aprovados_',mod,'$nota))',                   
  sep=""))))}


analise_n3$geral<-c(max(aprovados$nota),
                    mean(aprovados$nota),
                    min(aprovados$nota))
rownames(analise_n3)<-c('max3','mean3','min3')

analise_n3$cotas<-c(
  subset(aprovados.3,aprovados.3$mod_con!="A0")$nota%>%max(),
  subset(aprovados.3,aprovados.3$mod_con!="A0")$nota%>%mean(),
  subset(aprovados.3,aprovados.3$mod_con!="A0")$nota%>%min()
) # cria coluna de todos os cotistas (convocados em modalidade de cotas)

# fim
# remove variáveis desnecessárias
rm("aprovados_A0","aprovados_L1","aprovados_L2","aprovados_L5","aprovados_L6",
   "aprovados_L9","aprovados_L10","aprovados_L13","aprovados_L14","aprovados")
rm("lista_A0","lista_L1","lista_L2","lista_L5","lista_L6",
   "lista_L9","lista_L10","lista_L13","lista_L14","lista_todos","lista_todos2")

# Testes, não sei pra que servem
# print(analise_n3[2,1]>=analise_n1[2,1])
# print(analise_n3[2,1]>=analise_n2[2,1])


# ==============================================================================
#
# Meta análise
# Criar dfs com resultado compilado dos tres modelos
#
# Criar df meta_n com todas as notas dos tres modelos
meta_n<-rbind(analise_n1,analise_n2,analise_n3)

# Criar df meta_v com todas as alocações de vagas dos tres modelos
meta_v<-rbind(analise_v1,analise_v2,analise_v3)

# ==============================================================================
# Limpeza
print ("ATENÇÃO! LIMPEZA A SEGUIR. TEM CERTEZA QUE QUER APAGAR DADOS INTERMEDIÁRIOS?")
print ("ATENÇÃO! LIMPEZA A SEGUIR. TEM CERTEZA QUE QUER APAGAR DADOS INTERMEDIÁRIOS?")
print ("ATENÇÃO! LIMPEZA A SEGUIR. TEM CERTEZA QUE QUER APAGAR DADOS INTERMEDIÁRIOS?")

# Remover dfs desnecessários
rm("analise_n1","analise_n2","analise_n3",
   "analise_v1","analise_v2","analise_v3", "grupos")
# rm("aprovados.1","aprovados.2","aprovados.3","candidatos")

# Remover valores desnecessários
rm(list = ls.str(mode = 'numeric'))
rm(list = ls.str(mode = 'character'))

# Remover funções desnecessárias
rm (gera_candidatos,gera_distr,gera_nvagas)

print("Fim do arquivo")
# Fim do arquivo
