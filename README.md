# Trabalho Prático I - Hill Climbing & ILS
## CCF480 - META-HEURÍSTICAS

Implementação do trabalho prático 1 da disciplina de Meta-Heurísticas. Foram implementados os algoritmos Hill Climbing e Iterated Local Search. O algoritmo ILS precisa de um método de busca local. O próprio HC poderia ser usado, mas para variar o trabalho, foram implementados também outros dois algoritmos de busca local:

- PatternSearch: baseado no método de Hook-Jeeves descrito em doi:10.1145/321062.321069
- AmoebaSearch: baesado no método de Nelder-Mead descrito em doi:10.1093/comjnl/7.4.308

Para compilar, é necessário ter o `ghc` e o `make`, e executar

    $ make

O programa compilado aceita como parâmetro o caminho de saída para o arquivo CSV contendo os resultados obtidos, e o número de vezes que cada configuração deve ser executada. Por simplicidade, cada configuração de teste foi descrita no próprio arquivo `src/Main.hs`, então é necessário recompilar para mudar as configurações.
