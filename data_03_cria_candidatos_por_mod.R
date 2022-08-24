# ==============================================================================
# Arquivo: data_03_cria_candidatos.R
# Cria data frame com candidatos com nota e modalidade de inscrição
# Permite usar média e desvio padrão diferente para cada modalidade
# 
# Modificado em 2022-08-14-12-11.
# Autor: Mateus Silva Figueiredo
# ==============================================================================
#
# Dicionário
# 
# Inputs:
# n (número de estudantes)
# tot (número de vagas)
# função gera_nvagas()
# função gera_distr()
#
#
# Outputs:
#
# função gera_candidatos()
# data frame candidatos com nota e modalidade de inscrição
#
# ==============================================================================


######## Criando dados simulados de candidatos ########
# não rodar caso usar dados observados
# permite colocar médias (mean) e desvios padrões (sd) diferentes para cada modalidade

# definir n de estudantes
n <- 1000

# definir total de vagas
tot <- 100

# necessita função gera_nvagas. input = ppi, pcd, tot. tem ppi e pcd default.
gera_nvagas (0.5366,0.0843,tot) # mg = (0.5366,0.0843,tot). output = nvagas

# necessita função gera_distr. input = pub, bxa, ppi, pcd, n. todos default.
gera_distr(n=n) #distr informa número de candidatos em cada modalidade

# criar vetor mod, com a ordem das modalidades
mod <- c("A0","L1","L2","L5","L6", "L9", "L10", "L13", "L14")

# definir modalidades de cotas e AC
{
  y <- nvagas>0 # definir como FALSE modalidades com 0 vagas 
  mod<-mod[y] # eliminar modalidades com 0 vagas
  nvagas<-nvagas[y] # eliminar modalidades com 0 vagas
  distr <- distr[y] # caso for usar dist. eliminar modalidades com 0 vagas
  mod_cotas <- mod[mod!= "A0"] # cria mod_cotas sem A0
  rm(y)
}

# define mean e sd de cada modalidade
# informa n de candidatos em cada modalidade a partir de distr

# grupos informa mean e sd de nota de cada modalidade

# # distribuição igual para cada modalidade

m<-500;s<-100 #m = mean, s = sd
grupos<-data.frame("A0" =c("A0" ,m,s,round(distr[1])),
                   "L1" =c("L1" ,m,s,round(distr[2])),
                   "L2" =c("L2" ,m,s,round(distr[3])),
                   "L5" =c("L5" ,m,s,round(distr[4])),
                   "L6" =c("L6" ,m,s,round(distr[5])),
                   "L9" =c("L9" ,m,s,round(distr[6])),
                   "L10"=c("L10",m,s,round(distr[7])),
                   "L13"=c("L13",m,s,round(distr[8])),
                   "L14"=c("L14",m,s,round(distr[9])) #dados arbitrarios
); rownames(grupos)<-c("mod","mean","sd","n"); rm(m,s)

# # distribuição diferente para cada modalidade
# grupos<-data.frame("A0" =c("A0" ,650,110,round(distr[1])),
#                    "L1" =c("L1" ,400,100,round(distr[2])),
#                    "L2" =c("L2" ,450,100,round(distr[3])),
#                    "L5" =c("L5" ,600,110,round(distr[4])),
#                    "L6" =c("L6" ,500,100,round(distr[5])),
#                    "L9" =c("L9" ,420,100,round(distr[6])),
#                    "L10"=c("L10",410,100,round(distr[7])),
#                    "L13"=c("L13",300,100,round(distr[8])),
#                    "L14"=c("L14",310,100,round(distr[9])) #dados arbitrarios
# ); rownames(grupos)<-c("mod","mean","sd","n")


# cria função gera_candidatos
gera_candidatos <- function(mod,mean,sd){
  
  n<-as.numeric(grupos["n",mod])
                # gerar id dos candidatos
                 id<-c(1:n)
                 
                # gerar n notas aleatorias com 2 casas decimais
                 # usar mean e sd baseado no data.frame grupos
                nota<-as.numeric(round(rnorm(n,
                                              mean=as.numeric(grupos["mean",mod]), 
                                              sd=as.numeric(grupos["sd",mod])), 
                                        digits = 2)) # distrib normal
                
                # gerar coluna de mod.ins (modalidade de inscrição) com base no input mod
                mod_ins<-mod
                
                # gerar coluna de mod.con (modalidade de convocação) vazio
                mod_con<-vector("numeric", n)
                
  # gerar data.frame candidatos_mod
  candidatos_mod<-data.frame(id,nota,mod_ins,mod_con)
  
  # adicionar candidatos_mod a data.frame candidatos
 rbind(candidatos,candidatos_mod) ->> candidatos
}

{ #criar data.frame candidatos do zero
# cria data.frame candidatos em branco
candidatos <- data.frame()

# roda função gera_candidatos com for loop

for (i in 1:length(mod)){
gera_candidatos(mod=mod[i])
}

# colocar candidatos em ordem decrescente de nota
candidatos<<-candidatos[order(candidatos[,2],decreasing=T),]
}

# max(candidatos$nota) # confere nota máxima
# min(candidatos$nota) # confere nota mínima
 

