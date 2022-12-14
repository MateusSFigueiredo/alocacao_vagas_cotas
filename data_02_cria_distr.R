# ==============================================================================
# Arquivo: data_02_cria_distr.R
# Gera vari?vel distr, da distribui??o de candidatos em cada modalidade
# Utiliza porcentagens de modalidade na popula??o em geral
#
# Modificado em 2022-08-10-23-33.
# Autor: Mateus Silva Figueiredo
# ==============================================================================
# Dicion?rio
# 
# Inputs:
# pub (% de egressos de escola p?blica)
# bxa (% de baixa renda)
# ppi (% de pretos pardos ind?genas)
# pcd (% de pessoas com defici?ncia)
# 
# Outputs:
# fun??o gera_distr
# vetor distr
# se n=1, vetor distr ? porcentagem de candidatos em cada modalidade
# se n=no total de candidatos, distr ? n?mero de candidatos em cada modalidade
# ==============================================================================
#

# dados da popula??o em geral
# pub<-0.7    # propor??o de egresso de escola p?blica no estado. 0.7 arbitrario.
# bxa<-0.6    # propor??o de baixa renda no estado. 0.6 arbitrario.
# ppi<-0.5366 # propor??o de ppi no estado. mg = 0.5366
# pcd<-0.0843 # propor??o de pcd no estado. mg = 0.0843

gera_distr<-function(pub=0.7,bxa=0.6,ppi=0.5366,pcd=0.0843,n=1){
distr<-c(
    (n*pub * bxa    *(1-ppi)*(1-pcd)), #L1
    (n*pub * bxa    *ppi    *(1-pcd)), #L2
    (n*pub * (1-bxa)*(1-ppi)*(1-pcd)), #L5
    (n*pub * (1-bxa)*    ppi*(1-pcd)), #L6
    (n*pub * bxa    *(1-ppi)*   pcd ), #L9
    (n*pub * bxa    *    ppi*   pcd ), #L10
    (n*pub * (1-bxa)*(1-ppi)*   pcd ), #L13
    (n*pub * (1-bxa)*    ppi*   pcd )) #L14
distr<<-c((n-sum(distr)),distr) #define A0 como resto
}

# gera_distr()
# distr
# rm(distr)
# valor default de n = 1
# n pode ser diferente de 1 para calcular n?mero de candidatos por modalidade


