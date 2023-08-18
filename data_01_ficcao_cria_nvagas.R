# ==============================================================================
# Arquivo: data_01_cria_nvagas.R
# Informa número de vagas em um curso
# Cria nvagas com divisões e arredondamentos consecutivos
#
# Modificado em: 2023-02-11-11-00-00.
# Fazer round arredondar 0.5 para cima.
# Não consegui fazer o código concordar com o Termo de Adesão toda vez.
# Problema: Se tiver 0.5 vagas para PCD, arredonda para cima ou para baixo?
# Termo de Adesão de Medicina UFV arredonda pra cima.
# Termo de Adesão de Eng Hidrica da UFRGS arredonda pra baixo.

# Autor: Mateus Silva Figueiredo
# ==============================================================================
#
# Dicionário
#
# Inputs:
# tot (total de vagas)
# ppi (% de pretos pardos indígenas)
# pcd (% de pessoas com deficiência)
#
# Outputs:
# nvagas (vetor com número de vagas em cada modalidade.
#         modalidades em ordem crescente)
#
# ==============================================================================

# Fazer round_5_up arredondar 0.5 para cima
round_5_up = function(x) {
  posneg = sign(x);
  z = abs(x);
  z = z + 0.5 + sqrt(.Machine$double.eps);
  z = trunc(z);
  z*posneg}

### Função gera_nvagas

gera_nvagas<-function (ppi=0.5366, pcd=0.0843, tot){
  tot <- tot
  ppi100 <- ppi*100
  pcd100 <- pcd*100
  n.cotas <- ceiling(tot/2);
  n.A0   <- tot-n.cotas;
  ###divide 50% para baixa renda, o resto pra alta renda
  n.baixa <- ceiling(n.cotas/2); #baixa renda, L1+L9+L2+L10
  n.alta  <- n.cotas-n.baixa; #alta renda, L5+L13+L6+L14
  ###divide % de ppi por estado para ppi, o resto pra não-ppi (branco)
  n.baixa.ppi     <- ceiling(n.baixa*ppi); #baixa renda ppi, L2+L10
  n.baixa.branco  <- n.baixa-n.baixa.ppi; #baixa renda branco, L1+L9
  n.alta.ppi      <- ceiling(n.alta*ppi); #alta renda ppi, L6+L14
  n.alta.branco   <- n.alta-n.alta.ppi #alta renda branco, L5+13
  
  ###divide % de 1-pcd por estado para sd, o resto pra com deficiência

  n.baixa.ppi.sd     <- round(n.baixa.ppi*(1-pcd)) #L2, sem deficiência
  n.baixa.ppi.pcd    <- n.baixa.ppi-n.baixa.ppi.sd; #L10, pessoa com deficiência
  
  n.baixa.branco.sd  <- round(n.baixa.branco*(1-pcd)); #L1, sem deficiência
  n.baixa.branco.pcd <- n.baixa.branco-n.baixa.branco.sd; #L9, pessoa com deficiência

  n.alta.ppi.sd      <- round(n.alta.ppi*(1-pcd));    #L6, sem deficiência
  n.alta.ppi.pcd     <- n.alta.ppi-n.alta.ppi.sd;  #L14, pessoa com deficiência

  n.alta.branco.sd   <- round(n.alta.branco*(1-pcd));  #L5, sem deficiência
  n.alta.branco.pcd  <- n.alta.branco-n.alta.branco.sd #L13, pessoa com deficiência

    ###Cria vetor nvagas
  nvagas <<- c(n.A0,n.baixa.branco.sd,n.baixa.ppi.sd,
               n.alta.branco.sd,n.alta.ppi.sd,
               n.baixa.branco.pcd,n.baixa.ppi.pcd,
               n.alta.branco.pcd,n.alta.ppi.pcd)
}

# Roda função e cria valores

# gera_nvagas (ppi=0.5366,pcd=0.1,tot=50) # informa três parâmetros
# gera_nvagas (tot=50) # usa dois default, informa apenas tot
# output = vetor nvagas
# nvagas # ordem alfabética das modalidades
# mod <- c("A0","L1","L2","L5","L6", "L9", "L10", "L13", "L14")
