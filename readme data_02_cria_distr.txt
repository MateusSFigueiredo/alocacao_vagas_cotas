### Arquivo: data_02_cria_dist

O objetivo do arquivo data_02_cria_distr.R é criar o a função gera_distr

Função gera_distr cria vetor distr.

O vetor distr informa a proporção de pleiteantes a cada modalidade na população em geral.

A função gera_distr usa como input:
# pub<-0.7    # proporção de egresso de escola pública no estado. default = 0.7 arbitrario.
# bxa<-0.6    # proporção de baixa renda no estado. default =  0.6 arbitrario.
# ppi<-0.5366 # proporção de ppi no estado. default = mg = 0.5366
# pcd<-0.0843 # proporção de pcd no estado. default = mg = 0.0843
n   #número total de candidatos.

A função gera_fistr usa como default:
0.7 para pub. arbitrário.
0.6 para bxa. arbitrário.
0.5366 para ppi. baseado nos dados de 2010 de MG.
0.0843 para pcd. baseado nos dados de 2010 de MG.

Não sei se distr é importante. Afinal de contas, egressos de escola pública têm notas bastante diferentes de egressos de escola particular. Acho que dist só complica o modelo sem realmente aumentar sua qualidade.