### Arquivo: data_02_cria_dist

O objetivo do arquivo data_02_cria_distr.R � criar o a fun��o gera_distr

Fun��o gera_distr cria vetor distr.

O vetor distr informa a propor��o de pleiteantes a cada modalidade na popula��o em geral.

A fun��o gera_distr usa como input:
# pub<-0.7    # propor��o de egresso de escola p�blica no estado. default = 0.7 arbitrario.
# bxa<-0.6    # propor��o de baixa renda no estado. default =  0.6 arbitrario.
# ppi<-0.5366 # propor��o de ppi no estado. default = mg = 0.5366
# pcd<-0.0843 # propor��o de pcd no estado. default = mg = 0.0843
n   #n�mero total de candidatos.

A fun��o gera_fistr usa como default:
0.7 para pub. arbitr�rio.
0.6 para bxa. arbitr�rio.
0.5366 para ppi. baseado nos dados de 2010 de MG.
0.0843 para pcd. baseado nos dados de 2010 de MG.

N�o sei se distr � importante. Afinal de contas, egressos de escola p�blica t�m notas bastante diferentes de egressos de escola particular. Acho que dist s� complica o modelo sem realmente aumentar sua qualidade.