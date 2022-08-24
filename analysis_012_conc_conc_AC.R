# ==============================================================================
# Arquivo: analysis_012_conc_conc_AC.R
# Separa candidatos seguindo concorrência concomitante, AC primeiro = modelo 2
# Primeiro preenche vagas de Ampla Concorrência com todos os candidatos
# Depois preenche as vagas das cotas
# 
# Modificado em 2022-08-14-13-__.
# Autor: Mateus Silva Figueiredo
# ==============================================================================
#
# Dicionário
# Inputs:
#
# Outputs:
# analise_n2 = nota máxima, média e mínima de cada categoria com modelo 2
# analise_v2 = número de candidatos convocados por modalidade com modelo 2
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
# Cria meta_n para receber dados do loop sobre nota
meta_n<-data.frame()
# Cria meta_v para receber dados do loop sobre vagas
meta_v<-data.frame()

# Coloca todo esse código em um loop
for (r in 1:4){
# ==============================================================================

# Criar candidatos com gera_candidatos() em loop

{ #criar data.frame candidatos em branco
  candidatos <- data.frame()
  
  # roda função gera_candidatos com for loop
  for (i in 1:length(mod)){gera_candidatos(mod=mod[i])}
  
  # colocar candidatos em ordem decrescente de nota
  candidatos<<-candidatos[order(candidatos[,2],decreasing=T),]
}

# ==============================================================================

############## Concorrencia simultanea ##############
##############  preenche AC primeiro = sufixo 2 ##############

#View(candidatos)

# cria lista_todos para poder manipular a lista sem mexer nos candidatos
lista_todos <- candidatos

# cria várias dfs vazias de aprovados por modalidade
belch3 ("aprovados_",mod,"<<-data.frame(matrix(ncol = ncol(candidatos),
        nrow = 0))")

# coloca nomes nas colunas dessas dfs
belch3 ("colnames(aprovados_",mod,")<<- colnames(candidatos)")

# cria df vazias de lista de candidatos para manipular listas
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

# ==============================================================================
# Analises

# Analise - inscritos aprovados
# #Cria data.frame de todos os aprovados para ver numero de cotistas
aprovados<-data.frame()
belch3("aprovados<<-rbind(aprovados,aprovados_",mod,")")
aprovados.2<-aprovados

# Cria data.frame analise_v1 para concorrencia 1 (simples) para as vagas
analise_v2<-data.frame(matrix(ncol = length(mod), nrow = 1),
                       row.names=c("n?"))
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

# ------------------------------------------------------------------------------

# Preencher meta_n = meta analise das notas
# # meta_n<-data.frame()
#i=1
print(analise_n2)
print(r)
cbind(analise_n2,modelo=c(2,2,2),run=c(r,r,r))  %>% rbind(meta_n,.) -> meta_n
print(meta_n)
meta_n<<-meta_n

# ------------------------------------------------------------------------------
# Preencher meta_v = meta analise das vagas
# # meta_v<-data.frame()
print(analise_v2)
print(r)
cbind(analise_v2,modelo=c(2),run=c(r))  %>% rbind(meta_v,.) -> meta_v
print(meta_v)
meta_v<<-meta_v

# ------------------------------------------------------------------------------

} #fim do loop

# ==============================================================================

#fim do código
