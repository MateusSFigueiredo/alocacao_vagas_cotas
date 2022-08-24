### Arquivo: data_01_cria_nvagas

O objetivo do arquivo data_01_cria_nvagas.R é criar a função dividevagas e o vetor nvagas.

A função dividevagas recebe como inputs os números de ppi, pcd, e tot.
# ppi = % de pretos pardos indigenas
# pcd = % de pessoas com deficiencia
# tot = número total de vagas

A função dividevagas produz como output nvagas.

nvagas é um vetor com 9 elementos numéricos. Cada elemento é o número de vagas de determinada modalidade. As modalidades estão em ordem crescente.
A ordem das modalidades é "A0","L1","L2","L5","L6", "L9", "L10", "L13", "L14".

Caso seja necessário usar os valores intermediários (n.A0, n.alta, etc.) fora da função, deve-se substituir as setas <- por setas <<-.
Com a seta <-, os valores intermediários ficam apenas dentro da função.
Com a seta <<-, eles são salvos para o Global Environment.

Usa como default os valores de ppi e pcd de MG segundo IBGE 2010.