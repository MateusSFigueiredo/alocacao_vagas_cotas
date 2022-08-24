### Arquivo: data_01_cria_nvagas

O objetivo do arquivo data_01_cria_nvagas.R � criar a fun��o dividevagas e o vetor nvagas.

A fun��o dividevagas recebe como inputs os n�meros de ppi, pcd, e tot.
# ppi = % de pretos pardos indigenas
# pcd = % de pessoas com deficiencia
# tot = n�mero total de vagas

A fun��o dividevagas produz como output nvagas.

nvagas � um vetor com 9 elementos num�ricos. Cada elemento � o n�mero de vagas de determinada modalidade. As modalidades est�o em ordem crescente.
A ordem das modalidades � "A0","L1","L2","L5","L6", "L9", "L10", "L13", "L14".

Caso seja necess�rio usar os valores intermedi�rios (n.A0, n.alta, etc.) fora da fun��o, deve-se substituir as setas <- por setas <<-.
Com a seta <-, os valores intermedi�rios ficam apenas dentro da fun��o.
Com a seta <<-, eles s�o salvos para o Global Environment.

Usa como default os valores de ppi e pcd de MG segundo IBGE 2010.