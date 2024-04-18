import Control.Exception
import Data.List
import System.IO
import System.IO.Error
import System.Process

type Jogadores = [Jogador]

type Nome = String

type Pontuacao = Int

type Vez = Int

type Tabela = [Char]

data Jogador = Jogador Nome Pontuacao
  deriving (Show, Read)

-- Função que recebe uma String e retorna uma ação IO que resulta em uma String.
-- Ela exibe a String passada como argumento, lê uma entrada do usuário e retorna essa entrada.
getString :: String -> IO String
getString str = do
  putStr str -- Exibe a String no console sem adicionar uma nova linha.
  res <- getLine -- Lê uma linha de entrada do usuário e armazena na variável res.
  return res -- Retorna a linha lida como resultado da ação IO.

main :: IO ()
main = do
  -- Executa a ação ler_arquivo e, se ocorrer uma exceção, trata o erro com a função tratar_erro.
  catch (ler_arquivo) tratar_erro
  where
    -- Tenta ler o conteúdo do arquivo "data_total.txt".
    ler_arquivo = do
      arq <- openFile "data_total.txt" ReadMode -- Abre o arquivo para leitura.
      data_total <- hGetLine arq -- Lê uma linha do arquivo.
      hClose arq -- Fecha o arquivo.
      exibe_menu (read data_total) -- Passa os data_total lidos para a função exibe_menu após converter para o tipo adequado.
      return ()
    -- Trata o erro se o arquivo "data_total.txt" não existir.
    tratar_erro erro =
      if isDoesNotExistError erro
        then do
          arq <- openFile "data_total.txt" WriteMode -- Abre o arquivo para escrita.
          hPutStrLn arq "[]" -- Escreve uma lista vazia no arquivo.
          hClose arq -- Fecha o arquivo.
          exibe_menu [] -- Passa uma lista vazia para a função exibe_menu.
          return ()
        else ioError erro -- Lança a exceção de entrada e saída recebida.

-- função que exibe o Menu
-- Função que exibe o Menu e solicita uma opção ao usuário.
-- Ela recebe uma lista de Jogadores e retorna uma ação IO que resulta em uma lista de Jogadores.
exibe_menu :: Jogadores -> IO Jogadores
exibe_menu data_total = do
  system "cls" -- Limpa a tela (apenas no Windows).
  putStrLn "-------------------------------- Jogo da Velha --------------------------------" -- Exibe um cabeçalho.
  putStrLn "\nDigite 1 para cadastrar jogador" -- Exibe opções para o usuário.
  putStrLn "Digite 2 para jogar"
  putStrLn "Digite 3 para visualizar o ranking"
  putStrLn "Digite 0 para sair"
  putStr "Opção: " -- Solicita uma opção ao usuário.
  op <- getChar -- Lê um caractere digitado pelo usuário.
  getChar -- Descarta o Enter pressionado após a leitura do caractere.
  execute_op data_total op -- Executa a função execute_op com os data_total dos jogadores e a opção escolhida.

-- Função para manipular a opção escolhida pelo usuário.
-- Ela recebe uma lista de Jogadores e uma opção (Char) e retorna uma ação IO que resulta em uma lista de Jogadores.
execute_op :: Jogadores -> Char -> IO Jogadores
execute_op data_total '1' = cadastro data_total -- Se a opção escolhida for '1', chama a função cadastro.
execute_op data_total '2' = inicio_jogo data_total -- Se a opção escolhida for '2', chama a função inicio_jogo.
execute_op data_total '3' = do
  -- Se a opção escolhida for '3', executa um bloco do-notation.
  putStrLn "\nRanking dos jogadores:\n" -- Exibe uma mensagem indicando o início do ranking.
  if (null data_total)
    then do
      -- Verifica se não há jogadores cadastrados.
      putStrLn ("Não há jogadores cadastrados!") -- Se não houver jogadores, exibe uma mensagem informando.
    else -- Se houver jogadores, exibe o ranking.
      ranking (data_total)
  putStr "\nPressione Enter para voltar ao exibe_menu"
  getChar -- Lê o Enter pressionado pelo usuário.
  exibe_menu data_total
execute_op data_total '0' = do
  -- Se a opção escolhida for '0', executa um bloco do-notation.
  putStrLn ("\nTchau!!\n") -- Exibe uma mensagem de despedida.
  return data_total -- Retorna os data_total dos jogadores.
execute_op data_total _ = do
  -- Se a opção escolhida for diferente de '1', '2', '3' ou '0', executa um bloco do-notation.
  putStrLn ("\nOpção inválida") -- Exibe uma mensagem informando que a opção é inválida.
  putStr "\nPressione Enter para voltar ao exibe_menu"
  getChar -- Lê o Enter pressionado pelo usuário.
  exibe_menu data_total

-- Função responsável pelo cadastro de jogadores.
-- Recebe uma lista de Jogadores e retorna uma ação IO que resulta em uma lista de Jogadores.
cadastro :: Jogadores -> IO Jogadores
cadastro data_total = do
  nome <- getString "\nDigite um nome de usuário: " -- Solicita ao usuário que digite um nome de usuário.
  if (jogador_existe data_total nome)
    then do
      -- Verifica se o nome já existe na lista de jogadores.
      putStrLn "\nEsse nome já existe, escolha outro."
      putStr "\nPressione Enter para continuar"
      getChar -- Lê o Enter pressionado pelo usuário.
      exibe_menu data_total
    else do
      arq <- openFile "data_total.txt" WriteMode -- Abre o arquivo
      hPutStrLn arq (show ((Jogador nome 0) : data_total)) -- Escreve no arquivo a lista de jogadores atualizada com o novo jogador.
      hClose arq -- Fecha o arquivo.
      putStrLn ("\nUsuário " ++ nome ++ " cadastrado com sucesso.")
      putStr "\nPressione Enter para continuar"
      getChar -- Lê o Enter pressionado pelo usuário
      exibe_menu ((Jogador nome 0) : data_total) -- Retorna a nova lista de jogadores para o exibe_menu.

-- Função que verifica se um jogador existe na lista de jogadores.
-- Recebe uma lista de Jogadores, um Nome e retorna um Bool indicando se o jogador existe.
jogador_existe :: Jogadores -> Nome -> Bool
jogador_existe [] _ = False -- Se a lista de jogadores estiver vazia, o jogador não existe.
jogador_existe ((Jogador n p) : xs) nome
  | (n == nome) = True -- Se o nome do jogador for igual ao nome fornecido, o jogador existe.
  | otherwise = jogador_existe xs nome -- Caso contrário, verifica o restante da lista.

-- Função que prepara o início do jogo.
-- Recebe uma lista de jogadores e retorna uma ação IO que resulta em uma lista de jogadores.
inicio_jogo :: Jogadores -> IO Jogadores
inicio_jogo data_total = do
  jogador1 <- getString "\nDigite o nome do primeiro jogador: "
  -- Testa se o jogador1 existe na lista de jogadores.
  if not (jogador_existe data_total jogador1)
    then do
      putStrLn "\nEsse jogador não existe!"
      putStr "\nPressione Enter para continuar"
      getChar -- Lê o Enter pressionado pelo usuário.
      exibe_menu data_total
    else do
      jogador2 <- getString "\nDigite o nome do segundo jogador: "
      if not (jogador_existe data_total jogador2)
        then do
          putStrLn "\nEsse jogador não existe!"
          putStr "\nPressione Enter para continuar"
          getChar -- Lê o Enter pressionado pelo usuário.
          exibe_menu data_total
        else do
          -- Se chegou aqui, é porque os dois jogadores existem na lista.
          jogo_novo data_total jogador1 jogador2 -- Inicia um novo jogo com os dois jogadores.

-- Função que inicia um novo jogo.
-- Recebe uma lista de jogadores, os nomes dos dois jogadores e retorna uma ação IO que resulta em uma lista de jogadores.
jogo_novo :: Jogadores -> Nome -> Nome -> IO Jogadores
jogo_novo data_total jogador1 jogador2 = do
  putStrLn
    ( "\nIniciando o jogo \""
        ++ jogador1
        ++ " vs "
        ++ jogador2
        ++ "\" "
    )
  putStrLn ("\n" ++ jogador1 ++ " será o \'X\' e " ++ jogador2 ++ " será o \'O\'. Vamos lá!!") -- Informa qual jogador será 'X' e qual será 'O'.
  run_jogo data_total ['1', '2', '3', '4', '5', '6', '7', '8', '9'] jogador1 jogador2 0

-- Função responsável por controlar o jogo da velha.
-- Recebe uma lista de jogadores, a tabuleiro do jogo, os nomes dos jogadores e um inteiro indicando de quem é a vez.
run_jogo :: Jogadores -> Tabela -> Nome -> Nome -> Vez -> IO Jogadores
run_jogo data_total tabuleiro jogador1 jogador2 vez = do
  -- Imprime o tabuleiro do jogo.
  putStrLn
    ( "\n"
        ++ "                              "
        ++ (show (tabuleiro !! 0))
        ++ " | "
        ++ (show (tabuleiro !! 1))
        ++ " | "
        ++ (show (tabuleiro !! 2))
        ++ "\n                              ---------------\n"
        ++ "                              "
        ++ (show (tabuleiro !! 3))
        ++ " | "
        ++ (show (tabuleiro !! 4))
        ++ " | "
        ++ (show (tabuleiro !! 5))
        ++ "\n                              ---------------\n"
        ++ "                              "
        ++ (show (tabuleiro !! 6))
        ++ " | "
        ++ (show (tabuleiro !! 7))
        ++ " | "
        ++ (show (tabuleiro !! 8))
        ++ "\n"
    )
  -- Verifica se o jogador1 venceu.
  if (ganhador_j1 tabuleiro)
    then do
      putStrLn ("Parábens " ++ jogador1 ++ "! Você ganhou o jogo!!")
      -- Atualiza a pontuação do jogador1 no arquivo.
      arq_escrita <- openFile "data_total.txt" WriteMode
      hPutStrLn arq_escrita (show (update_pontuacao data_total jogador1))
      hClose arq_escrita
      -- Lê os data_total atualizados do arquivo.
      arq_leitura <- openFile "data_total.txt" ReadMode
      dados_atualizados <- hGetLine arq_leitura
      hClose arq_leitura
      putStr "\nPressione Enter para voltar ao exibe_menu"
      getChar
      -- Retorna ao exibe_menu com os data_total atualizados.
      exibe_menu (read dados_atualizados)
    else do
      -- Verifica se o jogador2 venceu.
      if (ganhador_j2 tabuleiro)
        then do
          putStrLn ("Parábens " ++ jogador2 ++ "! Você ganhou o jogo!!")
          -- Atualiza a pontuação do jogador2 no arquivo.
          arq_escrita <- openFile "data_total.txt" WriteMode
          hPutStrLn arq_escrita (show (update_pontuacao data_total jogador2))
          hClose arq_escrita
          -- Lê os data_total atualizados do arquivo.
          arq_leitura <- openFile "data_total.txt" ReadMode
          dados_atualizados <- hGetLine arq_leitura
          hClose arq_leitura
          putStr "\nPressione Enter para voltar ao exibe_menu"
          getChar
          -- Retorna ao exibe_menu com os data_total atualizados.
          exibe_menu (read dados_atualizados)
        else do
          -- Verifica se houve empate.
          if ((length (intersect "123456789" tabuleiro)) == 0)
            then do
              putStrLn ("Empatou!")
              putStr "\nPressione Enter para voltar ao exibe_menu"
              getChar
              exibe_menu data_total
            else do
              -- Verifica de quem é a vez de jogar.
              if (vez == 0)
                then do
                  putStr (jogador1 ++ ", é sua vez de jogar, escolha a posiçao de deseja: ")
                  op <- getChar
                  getChar
                  -- Testa se a opção é válida.
                  if not (elem op "123456789")
                    then do
                      putStrLn "\nEssa opção NÃO é válida, tente novamente "
                      -- Como a opção é inválida, continua a vez do jogador1.
                      run_jogo data_total tabuleiro jogador1 jogador2 0
                    else -- Se a opção é válida, verifica se já foi marcada.

                      if not (elem op tabuleiro)
                        then do
                          putStrLn "\nEssa opção já foi marcada, tente novamente "
                          run_jogo data_total tabuleiro jogador1 jogador2 0
                        else -- Se a opção é válida e ainda não foi marcada, passa a vez para o jogador2 e atualiza o tabuleiro.
                          run_jogo data_total (novo_tabuleiro tabuleiro vez op) jogador1 jogador2 1
                else do
                  putStr (jogador2 ++ ", é sua vez de jogar, escolha a posiçao de deseja: ")
                  op <- getChar
                  getChar
                  if not (elem op "123456789")
                    then do
                      putStrLn "\nEssa opção NÃO é válida, tente novamente..."
                      run_jogo data_total tabuleiro jogador1 jogador2 1
                    else
                      if not (elem op tabuleiro)
                        then do
                          putStrLn "\nEssa opção já foi marcada, escolha outra opção..."
                          run_jogo data_total tabuleiro jogador1 jogador2 1
                        else run_jogo data_total (novo_tabuleiro tabuleiro vez op) jogador1 jogador2 0

-- Função responsável por atualizar o tabuleiro do jogo com a jogada do jogador.
-- Retorna uma nova configuração do tabuleiro.
novo_tabuleiro :: Tabela -> Vez -> Char -> Tabela
novo_tabuleiro (x : xs) vez e
  | ((x == e) && (vez == 0)) = (['X'] ++ xs) -- Se a vez for do jogador 1, substitui o elemento na posição pelo 'X'.
  | ((x == e) && (vez == 1)) = (['O'] ++ xs) -- Se a vez for do jogador 2, substitui o elemento na posição pelo 'O'.
  | otherwise = x : (novo_tabuleiro xs vez e)

-- Função que verifica se o jogador1 venceu.
-- Recebe a configuração atual do tabuleiro.
-- Retorna True se o jogador1 venceu e False caso contrário.
ganhador_j1 :: Tabela -> Bool
ganhador_j1 tabuleiro
  -- Verifica nas linhas.
  | (((tabuleiro !! 0) == 'X') && ((tabuleiro !! 1) == 'X') && ((tabuleiro !! 2) == 'X')) = True
  | (((tabuleiro !! 3) == 'X') && ((tabuleiro !! 4) == 'X') && ((tabuleiro !! 5) == 'X')) = True
  | (((tabuleiro !! 6) == 'X') && ((tabuleiro !! 7) == 'X') && ((tabuleiro !! 8) == 'X')) = True
  -- Verifica nas colunas.
  | (((tabuleiro !! 0) == 'X') && ((tabuleiro !! 3) == 'X') && ((tabuleiro !! 6) == 'X')) = True
  | (((tabuleiro !! 1) == 'X') && ((tabuleiro !! 4) == 'X') && ((tabuleiro !! 7) == 'X')) = True
  | (((tabuleiro !! 2) == 'X') && ((tabuleiro !! 5) == 'X') && ((tabuleiro !! 8) == 'X')) = True
  -- Verifica nas diagonais.
  | (((tabuleiro !! 0) == 'X') && ((tabuleiro !! 4) == 'X') && ((tabuleiro !! 8) == 'X')) = True
  | (((tabuleiro !! 2) == 'X') && ((tabuleiro !! 4) == 'X') && ((tabuleiro !! 6) == 'X')) = True
  | otherwise = False

ganhador_j2 :: Tabela -> Bool
ganhador_j2 tabuleiro
  | (((tabuleiro !! 0) == 'O') && ((tabuleiro !! 1) == 'O') && ((tabuleiro !! 2) == 'O')) = True
  | (((tabuleiro !! 3) == 'O') && ((tabuleiro !! 4) == 'O') && ((tabuleiro !! 5) == 'O')) = True
  | (((tabuleiro !! 6) == 'O') && ((tabuleiro !! 7) == 'O') && ((tabuleiro !! 8) == 'O')) = True
  | (((tabuleiro !! 0) == 'O') && ((tabuleiro !! 3) == 'O') && ((tabuleiro !! 6) == 'O')) = True
  | (((tabuleiro !! 1) == 'O') && ((tabuleiro !! 4) == 'O') && ((tabuleiro !! 7) == 'O')) = True
  | (((tabuleiro !! 2) == 'O') && ((tabuleiro !! 5) == 'O') && ((tabuleiro !! 8) == 'O')) = True
  | (((tabuleiro !! 0) == 'O') && ((tabuleiro !! 4) == 'O') && ((tabuleiro !! 8) == 'O')) = True
  | (((tabuleiro !! 2) == 'O') && ((tabuleiro !! 4) == 'O') && ((tabuleiro !! 6) == 'O')) = True
  | otherwise = False

-- Função que atualiza a pontuação do vencedor.
-- Recebe a lista de jogadores, o nome do vencedor e retorna uma nova lista atualizada.
update_pontuacao :: Jogadores -> String -> Jogadores
update_pontuacao ((Jogador nome pontuacao) : xs) vencedor
  | (nome == vencedor) = [(Jogador nome (pontuacao + 1))] ++ xs -- Se o nome corresponder ao vencedor, incrementa sua pontuação em 1.
  | otherwise = (Jogador nome pontuacao) : (update_pontuacao xs vencedor) -- Se não, mantém os data_total e continua a busca.

-- Função que exibe o ranking dos jogadores.
-- O critério é da maior para a menor pontuação.
ranking :: Jogadores -> IO ()
ranking [] = return () -- Se a lista de jogadores estiver vazia, não há nada a exibir.
ranking (x : xs) = do
  putStrLn ((check_nome x) ++ " possui " ++ (show (check_pontos x)) ++ " pontos")
  ranking xs -- Chama recursivamente a função para exibir o restante do ranking.

-- Função que recebe um jogador e retorna o nome.
check_nome :: Jogador -> Nome
check_nome (Jogador nome _) = nome

-- Função que recebe um jogador e retorna a pontuação.
check_pontos :: Jogador -> Pontuacao
check_pontos (Jogador _ pontuacao) = pontuacao
