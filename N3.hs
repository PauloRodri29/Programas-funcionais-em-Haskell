data Produto = Produto { nome::String , preco::Double , quantidade::Int }deriving(Eq,Ord,Show)

data ArvoreBin = Nulo | No { prod::Produto, esq::ArvoreBin ,dir::ArvoreBin }deriving(Eq,Ord,Show)

implement :: ArvoreBin --Árvore de Teste
implement = No (Produto "Colheres" 3.50 40) (No(Produto "Panos" 4.30 20) (No (Produto "Pratos" 4.50 50) Nulo Nulo) Nulo) Nulo

busca::ArvoreBin->String->ArvoreBin --Questão Letra A) Busca
busca Nulo _ = Nulo
busca ab npro
    | npro == nome (prod ab) = No (prod ab) Nulo Nulo
    | npro <= nome (prod ab) = busca (dir ab) npro
    | otherwise = busca (esq ab) npro

atua_Add::ArvoreBin->Produto->ArvoreBin --Quesão Letra B) Adcionar\Atualizar
atua_Add Nulo item = Nulo
atua_Add ab item
    | nome item == nome (prod ab) = No item (esq ab) (dir ab)
    | nome item > nome (prod ab) = No (prod ab) (esq ab) (atua_Add (dir ab) item)
    | otherwise = No (prod ab) (atua_Add (esq ab) item) (dir ab)


soma_total:: ArvoreBin -> Double --Questão Letra C) Valor total dos produtos 
soma_total Nulo = 0
soma_total ab = fromIntegral (quantidade (prod ab)) * preco (prod ab) + soma_total (esq ab) + soma_total (dir ab)

vender::ArvoreBin -> String -> Int -> ArvoreBin --Questão Letra D) Vendas de Produtos
vender ab iten qtd
    | iten == (nome . prod) ab = corrigiAB (No (Produto ((nome . prod) ab) ((preco . prod) ab) ((quantidade . prod) ab - qtd)) (esq ab) (dir ab))
    | iten > (nome . prod) ab = No (prod ab) (esq ab) (vender (dir ab) iten qtd)
    | otherwise = No  (prod ab) (vender (esq ab) iten qtd) (dir ab)

corrigiAB::ArvoreBin -> ArvoreBin
corrigiAB ab
   | (quantidade . prod) ab <= 0 = remover ab ((nome . prod) ab)
   | otherwise = ab

getMaximo::ArvoreBin->ArvoreBin
getMaximo ab
    | dir ab /= Nulo = getMaximo (dir ab)
    | otherwise = ab
    
remover::ArvoreBin -> String -> ArvoreBin
remover Nulo _ = Nulo
remover ab item
    | item == (nome . prod) ab = remAux ab
    | item > (nome . prod) ab = No (prod ab) (esq ab) (remover (dir ab) item)
    | otherwise = No (prod ab) (remover (esq ab) item) (dir ab)

remAux::ArvoreBin -> ArvoreBin
remAux ab
    | esq ab == Nulo && dir ab == Nulo = Nulo
    | esq ab == Nulo = dir ab
    | dir ab == Nulo = esq ab
    | otherwise = No (prod (getMaximo ab)) (remover (esq ab) ((nome . prod . getMaximo) ab)) (dir ab)

--Main (Parte de interação legivel para o usuário)
main::IO()
main = do
    menu Nulo
    return () 

menu :: ArvoreBin -> IO()
menu dados = do
    print "=========== SISTEMA ============"
    print "1. Adcionar ou atualizar produto no estoque"
    print "2. Buscar produto no estoque"
    print "3. Valor total do estoque (R$)"
    print "4. Sair"
    print "Escolha uma opcao "
    op <-  getChar
    getChar
    case op of
        '1'-> do
            entrada <- adProd dados
            menu entrada
        '2'-> do
            achado <- bus dados
            if achado == Nulo
                then print "Produto Invalido"
                else print (prod achado ) 
            menu dados
        '3'-> do
            print (soma_total dados)
        '4' -> do
            putStrLn "Voce saiu \n"
        _   -> do
            print "Opcao invalida"
            menu dados


adProd:: ArvoreBin -> IO ArvoreBin
adProd dados = do
    print "Nome do produto: "
    itm <- getLine
    print "Quantidade: "
    quant <- getLine
    print "Valor: "
    valr <- getLine
    return ( atua_Add dados (Produto itm (read valr::Double) (read quant::Int)))
     
bus::ArvoreBin-> IO ArvoreBin
bus dados = do
    print "Insira o nome do produto"
    it <- getLine
    let itach = busca dados it 
    return itach

--Consegui até aqui!!;)