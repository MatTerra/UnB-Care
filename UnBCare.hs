module UnBCare where

import ModeloDados

{-

██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

 
 
O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}


{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}
comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento med quant estoque 
  | medicamentoExiste med estoque = [ if fst estoque == med
                                        then (fst estoque, snd estoque+quant) 
                                        else (fst estoque, snd estoque) | estoque <- estoque]
  | otherwise = (med,quant):estoque

temMedicamento :: Medicamento -> EstoqueMedicamentos -> Bool
temMedicamento _ [] = False
temMedicamento x (y: ys) = if fst y == x && snd y > 0 
                             then True 
                             else temMedicamento x ys

medicamentoExiste :: Medicamento -> EstoqueMedicamentos -> Bool
medicamentoExiste _ [] = False
medicamentoExiste x (y:ys) = if x == fst y 
                               then True 
                               else medicamentoExiste x ys

{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}
tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento med estoque 
  | temMedicamento med estoque = Just [ if fst estoque == med 
                                          then (fst estoque, snd estoque-1) 
                                          else (fst estoque, snd estoque)| estoque <- estoque ]
  | otherwise = Nothing 

{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}
consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento med (item:estoque) 
  | temMedicamento med (item:estoque) = if fst item == med 
                                          then snd item 
                                          else consultarMedicamento med estoque
  | otherwise = 0

{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}
demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos receituario = foldr somarEstoque (map prescricaoParaEstoque receituario) []

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

somarEstoque :: EstoqueMedicamentos -> EstoqueMedicamentos -> EstoqueMedicamentos
somarEstoque estoqueBase [] = quicksort estoqueBase 
somarEstoque estoqueBase (estoqueNovoItem : estoqueNovo) 
  | medicamentoExiste (fst estoqueNovoItem) estoqueBase = somarEstoque 
    [if fst estoqueNovoItem == fst item 
       then (fst item, snd item + snd estoqueNovoItem)
       else item | item <- estoqueBase] 
    estoqueNovo
  | otherwise = somarEstoque (estoqueNovoItem:estoqueBase) estoqueNovo
  
prescricaoParaEstoque :: Prescricao -> (Medicamento, Quantidade)
prescricaoParaEstoque prescricao = (fst prescricao, length (snd prescricao))

{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}
receituarioValido :: Receituario -> Bool
receituarioValido [] = True
receituarioValido receituario = medicamentosOrdenadosNoReceituario receituario 
                                  && horariosOrdenadosNoReceituario receituario

medicamentosOrdenadosNoReceituario :: Receituario -> Bool
medicamentosOrdenadosNoReceituario receituario = medicamentosOrdenados (map fst receituario)

medicamentosOrdenados :: [Medicamento] -> Bool
medicamentosOrdenados [] = True
medicamentosOrdenados (medicamento : medicamentos) = (and . map (> medicamento)) medicamentos 

horariosOrdenadosNoReceituario :: Receituario -> Bool
horariosOrdenadosNoReceituario receituario = (and . map horariosOrdenadosNaPrescricao) receituario

horariosOrdenadosNaPrescricao :: Prescricao -> Bool
horariosOrdenadosNaPrescricao prescricao = (horariosOrdenados . snd) prescricao

horariosOrdenados :: [Horario] -> Bool
horariosOrdenados [] = True
horariosOrdenados (horario:horarios) = (and . map (> horario)) horarios && horariosOrdenados horarios

planoValido :: PlanoMedicamento -> Bool
planoValido [] = True
planoValido plano = horariosOrdenadosNoPlano plano && medicamentosOrdenadosNoPlano plano

horariosOrdenadosNoPlano :: PlanoMedicamento -> Bool
horariosOrdenadosNoPlano plano = (horariosOrdenados . map fst) plano

medicamentosOrdenadosNoPlano :: PlanoMedicamento -> Bool
medicamentosOrdenadosNoPlano plano = (and . map medicamentosOrdenados . map snd) plano

{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1, Medicar m1 x]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

 -}
plantaoValido :: Plantao -> Bool
plantaoValido plantao = (horariosOrdenados . map fst) plantao 
  && not ((or . map medicaECompraMesmoMedicamento . map snd) plantao) 
  && (and . map medicamentosOrdenados . map (map medCuidado)) ((map apenasMedicars . map snd) plantao) 

medCuidado :: Cuidado -> Medicamento
medCuidado (Medicar med) = med
medCuidado (Comprar med _) = med

eMedicar :: Cuidado -> Bool
eMedicar (Medicar _) = True
eMedicar (Comprar _ _) = False

apenasMedicars :: [Cuidado] -> [Cuidado]
apenasMedicars c = filter eMedicar c

medicaECompraMesmoMedicamento :: [Cuidado] -> Bool
medicaECompraMesmoMedicamento (Medicar med:[]) = False
medicaECompraMesmoMedicamento (Comprar med _:[]) = False
medicaECompraMesmoMedicamento (Medicar med:c) = (elem med . map medCuidado) c || medicaECompraMesmoMedicamento c
medicaECompraMesmoMedicamento (Comprar med _:c) = (elem med . map medCuidado) c || medicaECompraMesmoMedicamento c

{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário.

-}
geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario receituario = invertePlanoReceituario receituario

invertePlanoReceituario :: (Eq a, Ord a, Ord b) => [(a, [b])] -> [(b, [a])]
invertePlanoReceituario [] = []
invertePlanoReceituario entrada = foldr somarListaDeTuplasPorChave [] (map inverteItemPlanoReceituario entrada)

inverteItemPlanoReceituario :: (a, [b]) -> [(b, [a])]
inverteItemPlanoReceituario item = [(a, [fst item]) | a <- (snd item)]

somarListaDeTuplasPorChave :: (Eq a, Ord a, Ord b) => [(a, [b])] -> [(a, [b])] -> [(a, [b])]
somarListaDeTuplasPorChave listaInicial [] = quicksort listaInicial
somarListaDeTuplasPorChave listaInicial (tuplaNova : listaNova) 
  | possuiChave (fst tuplaNova) listaInicial = somarListaDeTuplasPorChave 
    (somarTuplaEmTuplaExistente tuplaNova listaInicial) 
    listaNova
  | otherwise = somarListaDeTuplasPorChave (tuplaNova:listaInicial) listaNova
  
somarTuplaEmTuplaExistente :: (Eq a) => (a, [b]) -> [(a, [b])] -> [(a, [b])]
somarTuplaEmTuplaExistente tuplaNova lista = [if fst tuplaNova == fst tupla 
                                                then (fst tupla, (snd tupla ++ snd tuplaNova)) 
                                                else tupla | tupla <- lista] 

possuiChave :: (Eq a) => a -> [(a, [b])] -> Bool 
possuiChave _ [] = False
possuiChave chave (tupla : lista) = if chave == fst tupla 
                                      then True 
                                      else possuiChave chave lista

{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario ?

-}
geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano plano = invertePlanoReceituario plano

{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado 
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}
executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao plantao estoque = (executaCuidados . concatMap snd) plantao (Just estoque)

executaCuidados :: [Cuidado] -> Maybe EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaCuidados _ Nothing = Nothing
executaCuidados [] estoque = estoque
executaCuidados (Medicar med:c) (Just estoque) = executaCuidados c (tomarMedicamento med estoque)
executaCuidados (Comprar med quant:c) (Just estoque) = executaCuidados c (Just (comprarMedicamento med quant estoque))


{-

QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano 
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão 
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}
satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos  -> Bool
satisfaz plantao plano estoque = not (executaPlantao plantao estoque == Nothing) && [(fst plantao, horarioPlantaoParaPlano (snd plantao)) | plantao <- plantao] == plano

horarioPlantaoParaPlano :: [Cuidado] -> [Medicamento]
horarioPlantaoParaPlano plantao = (map medCuidado . apenasMedicars) plantao

{-

QUESTÃO 11 (EXTRA) VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano de medicamentos e ao estoque.

-}
plantaoCorreto :: PlanoMedicamento ->  EstoqueMedicamentos  -> Plantao
plantaoCorreto [] estoque = []
plantaoCorreto plano estoque = somarTuplaEmTuplaExistente (fst (plano!!0), (gerarComprasDoPlano plano estoque)) [(fst plano, (map Medicar . snd) plano) | plano <- plano]

estoqueAComprar :: EstoqueMedicamentos -> EstoqueMedicamentos -> EstoqueMedicamentos
estoqueAComprar [] estoqueNecessario = estoqueNecessario
estoqueAComprar estoqueAtual [] = []
estoqueAComprar estoqueAtual estoqueNecessario = [estoque | estoque <- (somarEstoque estoqueNecessario ([(fst estoque, snd estoque * (-1)) | estoque <- estoqueAtual])), snd (estoque)>0]

planoParaEstoque :: PlanoMedicamento -> EstoqueMedicamentos
planoParaEstoque plano = (demandaMedicamentos . geraReceituarioPlano) plano

gerarComprasDoPlano :: PlanoMedicamento -> EstoqueMedicamentos -> [Cuidado]
gerarComprasDoPlano plano estoque = [Comprar med quant | (med, quant) <- (estoqueAComprar estoque . planoParaEstoque) plano]
