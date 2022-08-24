# ==============================================================================
# Arquivo: data_02_cria_distr.R
# Gera variável distr, da distribuição de candidatos em cada modalidade
# Utiliza porcentagens de modalidade na população em geral
#
# Modificado em 2022-08-10-23-33.
# Autor: Mateus Silva Figueiredo
# ==============================================================================
# Dicionário
# 
# Inputs:
# pub (% de egressos de escola pública)
# bxa (% de baixa renda)
# ppi (% de pretos pardos indígenas)
# pcd (% de pessoas com deficiência)
# 
# Outputs:
# função gera_distr
# vetor distr
# se n=1, vetor distr é porcentagem de candidatos em cada modalidade
# se n=no total de candidatos, distr é número de candidatos em cada modalidade
# ==============================================================================
#

# dados da população em geral
# pub<-0.7    # proporção de egresso de escola pública no estado. 0.7 arbitrario.
# bxa<-0.6    # proporção de baixa renda no estado. 0.6 arbitrario.
# ppi<-0.5366 # proporção de ppi no estado. mg = 0.5366
# pcd<-0.0843 # proporção de pcd no estado. mg = 0.0843

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
# n pode ser diferente de 1 para calcular número de candidatos por modalidade


