# ==============================================================================
# Arquivo: data_02_cria_candidatos.R
#
# Cria candidatos fictícios
#
# Gera variável distr, da distribuição de candidatos em cada modalidade
# Utiliza porcentagens de modalidade na população em geral
#
# Cria data frame com candidatos com nota e modalidade de inscrição
# Permite usar média e desvio padrão diferente para cada modalidade
# 
# Modificado em 2023-02-11.
# Autor: Mateus Silva Figueiredo
# ==============================================================================
#
# Dicionário
# 
# Inputs:
# 
# pub (% de egressos de escola pública)
# bxa (% de baixa renda)
# ppi (% de pretos pardos indígenas)
# pcd (% de pessoas com deficiência)
#
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
# função gera_distr
# vetor distr
# se n=1, vetor distr é porcentagem de candidatos em cada modalidade
# se n=no total de candidatos, distr é número de candidatos em cada modalidade

# ==============================================================================
# Cria função gera_distr
# Cria distr

# dados da população em geral
# pub<-0.7    # proporção de egresso de escola pública no estado. 0.7 arbitrario.
# bxa<-0.6    # proporção de baixa renda no estado. 0.6 arbitrario.
# ppi<-0.5366 # proporção de ppi no estado. mg = 0.5366
# pcd<-0.0843 # proporção de pcd no estado. mg = 0.0843

gera_distr<-function(pub=0.7,bxa=0.6,ppi=0.5366,pcd=0.0843,n=1){
  distr<-c(
    (n*pub * bxa    *(1-ppi)*(1-pcd)), #L01
    (n*pub * bxa    *ppi    *(1-pcd)), #L02
    (n*pub * (1-bxa)*(1-ppi)*(1-pcd)), #L05
    (n*pub * (1-bxa)*    ppi*(1-pcd)), #L06
    (n*pub * bxa    *(1-ppi)*   pcd ), #L09
    (n*pub * bxa    *    ppi*   pcd ), #L10
    (n*pub * (1-bxa)*(1-ppi)*   pcd ), #L13
    (n*pub * (1-bxa)*    ppi*   pcd )) #L14
  distr<<-c((n-sum(distr)),distr) #define A0 como resto
}

gera_distr(pub=0.7,bxa=0.6,ppi=0.5366,pcd=0.0843,n=1)
distr
# rm(distr)
# valor default de n = 1
# n pode ser diferente de 1 para calcular número de candidatos por modalidade

# ==============================================================================

# Criando dados simulados de candidatos #

# não rodar caso usar dados observados
# permite colocar médias (mean) e desvios padrões (sd) diferentes para cada modalidade

# definir n de estudantes
n <- 1000

# definir total de vagas
tot <- 100

# opcional. define seed para resultado ser reproduzível
set.seed(2023)

# ------------------------------------------------------------------------------
# definir nvagas
# ordem: A0, L01, L02, L05, L06, L09, L10, L13, L14

# opção 1: usar função gera_nvagas. input = ppi, pcd, tot. tem ppi e pcd default.
# gera_nvagas (0.5366,0.0843,tot) # mg = (0.5366,0.0843,tot). output = nvagas

# opção 2: vagas arbitrárias. Preferencialmente copiando de termo de adesão
# nvagas <- c(25,5,6,4,6,1,1,1,1) # Medicina UFV
nvagas <- c(30,5,8,5,8,1,1,1,1) # Pedagogia UFV

nvagas %>% length == 9

# ------------------------------------------------------------------------------
# gera distr = numero de candidatos em cada modalidade

# necessita função gera_distr. input = pub, bxa, ppi, pcd, n. todos default.
gera_distr(n=n) #distr informa número de candidatos em cada modalidade

# criar vetor mod, com a ordem das modalidades
mod <- c("A0","L01","L02","L05","L06", "L09", "L10", "L13", "L14")

# definir modalidades de cotas e AC
{
  ha_vagas <- nvagas>0 # definir como FALSE modalidades com 0 vagas 
  mod<-mod[ha_vagas] # eliminar modalidades com 0 vagas
  nvagas<-nvagas[ha_vagas] # eliminar modalidades com 0 vagas
  distr <- distr[ha_vagas] # caso for usar dist. eliminar modalidades com 0 vagas
  mod_cotas <- mod[mod!= "A0"] # cria mod_cotas sem A0
  rm(ha_vagas)
}

# ------------------------------------------------------------------------------

# define mean e sd de cada modalidade
# informa n de candidatos em cada modalidade a partir de distr

# stats_por_mod informa mean e sd de nota de cada modalidade

# ------------------------------------------------------------------------------
# preencher nota de cada candidato

# ---

# opção 1: distribuição igual para cada modalidade

m<-500;s<-100 #m = mean, s = sd
stats_por_mod<-data.frame("A0" =c("A0" ,m,s,round(distr[1])),
                   "L01"=c("L01",m,s,round(distr[2])),
                   "L02"=c("L02",m,s,round(distr[3])),
                   "L05"=c("L05",m,s,round(distr[4])),
                   "L06"=c("L06",m,s,round(distr[5])),
                   "L09"=c("L09",m,s,round(distr[6])),
                   "L10"=c("L10",m,s,round(distr[7])),
                   "L13"=c("L13",m,s,round(distr[8])),
                   "L14"=c("L14",m,s,round(distr[9])) #dados arbitrarios
); rownames(stats_por_mod)<-c("mod","mean","sd","n"); rm(m,s)

# ---

# opção 2: distribuição diferente para cada modalidade
# stats_por_mod<-data.frame("A0" =c("A0" ,650,110,round(distr[1])),
#                    "L01" =c("L01" ,400,100,round(distr[2])),
#                    "L02" =c("L02" ,450,100,round(distr[3])),
#                    "L05" =c("L05" ,600,110,round(distr[4])),
#                    "L06" =c("L06" ,500,100,round(distr[5])),
#                    "L09" =c("L09" ,420,100,round(distr[6])),
#                    "L10"=c("L10",410,100,round(distr[7])),
#                    "L13"=c("L13",300,100,round(distr[8])),
#                    "L14"=c("L14",310,100,round(distr[9])) #dados arbitrarios
# ); rownames(stats_por_mod)<-c("mod","mean","sd","n")


# ------------------------------------------------------------------------------

# cria função gera_candidatos
gera_candidatos <- function(mod,mean,sd){
  
  n<-as.numeric(stats_por_mod["n",mod])
                # gerar id dos candidatos
                 id<-c(1:n)
                 
                # gerar n notas aleatorias com 2 casas decimais
                 # usar mean e sd baseado no data.frame stats_por_mod
                nota<-as.numeric(round(rnorm(n,
                                              mean=as.numeric(stats_por_mod["mean",mod]), 
                                              sd=as.numeric(stats_por_mod["sd",mod])), 
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
  
  
# ------------------------------------------------------------------------------
  

# roda função gera_candidatos com for loop

for (i in 1:length(mod)){
gera_candidatos(mod=mod[i])
}

# colocar candidatos em ordem decrescente de nota
candidatos<<-candidatos[order(candidatos[,2],decreasing=T),]
}

candidatos$id<-c(1:nrow(candidatos)) # gera id único para cada candidato

# max(candidatos$nota) # confere nota máxima
# min(candidatos$nota) # confere nota mínima

# ==============================================================================
# acrescentar colunas pub, bxa, ppi, pcd
# assume que nenhum candidato omitiu privilégio

# criar coluna pub para egressos de escola publica
candidatos$mod_ins %in% c("L01","L02","L06","L05","L09","L14","L13","L10") -> candidatos$pub

# criar coluna bxa para baixa renda
candidatos$mod_ins %in% c("L01","L02","L09","L10") -> candidatos$bxa

# criar coluna ppi para estudantes pretos pardos indigenas
candidatos$mod_ins %in% c("L02","L06","L10","L14") -> candidatos$ppi

# criar coluna pcd para estudantes pessoas com deficiência
candidatos$mod_ins %in% c("L09","L10","L13","L14") -> candidatos$pcd

# ==============================================================================
# acrescentar coluna Curso e Processo_Seletivo

candidatos$Curso <- "Curso_simulado"
candidatos$Processo_Seletivo <- "SISU9999"

# ==============================================================================
# Limpeza
rm(distr,stats_por_mod,n,tot,i)
