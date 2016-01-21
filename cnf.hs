-- EGILEAK: ENEKO SAMPEDRO ETA GONTZAL PUJANA -- 2015-2016 -- LKSA --

--Hasierako metodoa izango da, programa exekutatuko duena
cnf:: IO ()
cnf = do 
  n <- lortu_n
  putStrLn "\nOrain CNF formula sartuko da."
  putStrLn "Adibidea: [[-1,1,0],[0,0,1],[-1,-1,-1]]"
  putStrLn "(1: balioa, -1: balio ukatua eta 0: baliorik ez)"
  putStrLn "\nTekleatu CNF formula:"
  formula <- lortu_cnf (n)

  putStr "Sartutako CNF formula: \n"
  putStr (inprimatu (formula) ([]))
  cnf_2 (formula) (n)

--Formula sartu eta geroko menuaz pantailaratzen arduratzen da. Behin aukera batekin amaitzerakoan, berriz exekutatuko dena
cnf_2:: [[Integer]] -> Integer -> IO ()
cnf_2 formula n = do
  putStrLn "\n\nHautatu aukera bat: \n"
  putStrLn "1.Egiazkotasun-proba"
  putStrLn "2.Betegarritasun-proba"
  putStrLn "3.Baliozkotasun-proba"
  putStrLn "4.Bukatu"
  
  aukera <- lortu_aukera
  menua (aukera) (n) (formula)
    
--Menuaren aukeretaz arduraztzen den programa da.     
menua:: Integer -> Integer -> [[Integer]] -> IO ()
menua x n formula
  | x == 1 =  do
    y <- lortu_balorazio (n)
    putStr "Emaitza: \n"
    print(egiazkoa (formula) (y))
    cnf_2 (formula) (n)
  | x == 2 = do
    betegarria (formula) (n)
    cnf_2 (formula) (n)
  | x == 3 = do
    baliozkoa (formula) (n)
    cnf_2 (formula) (n)
  | otherwise = putStrLn "Amaitu."

--EGIAZKOTASUNA-----------------------------------------
--Formula eta balorazio bat sartuta, egiazko edo faltsua den esango digu. Horretarako, trueOrFalse azpi programa 
--idatzi dugu, klausula bakoitzaren elementuak, boolear motan ordezkatzen dituena. Egiazko funtzioak, klausula bakoitzaren
--elementuak or bidez alderatuko ditu, eta ondoren, and baten bitartez, klausula guztien alderaketa, boolear bat lortuz.
egiazkoa:: [[Integer]] -> [Bool] -> Bool
egiazkoa x y = and[ t | t <- [or(trueOrFalse v y) | v <- x]] 

trueOrFalse:: [Integer] -> [Bool] -> [Bool]
trueOrFalse [] [] = []
trueOrFalse (x:y) u
  | x == -1 = not(head(u)):(trueOrFalse y (tail u) )
  | x == 1 = ( head(u)):(trueOrFalse y (tail u ) )
  | otherwise = ( trueOrFalse y (tail u) )
--EGIAZKOTASUNA-----------------------------------------

--BETEGARRITASUNA---------------------------------------
--Betegarritasuna burutzeko, egiazkoa metodoa erabili dugu, baina balorazioak automatikoki sartuko dira. (Balorazio posible guztiak)
--Balorazioak ateratzeko, azterketan inplementatutako aldatu funtzioa erabili dugu. Betegarria funtzioak, betegarria_lag funtzioari deia egingo dio.
--Azken funtzio honek, kontagailu bat izango du, 0-tik hasiko dena, 2^n-1 arte. Kontagailu hau, aldatu funtzioari transferituko zaio, honek
--balorazioak itzultzeko. Balorazio hauek egiazkoa funtzioan sartu, eta True itzultzen bada, programa bukatu egingo da, mezu bat pantailaratuz.
--False ematen badu, errekurtsibitatea erabiliz, betegarria_lag funtzioa berriz deituko da, kontagailua + 1 eginda. Kontagailua 2^n-1 baino handiagoa bada
--sartu dugun formula betegarria ez dela pantailaratuko digu.
betegarria:: [[Integer]] -> Integer -> IO ()
betegarria x n = do
  putStrLn "\nFormula betegarria den kalkulatuko da."
  betegarria_lag (x) (n) (0)
  
betegarria_lag:: [[Integer]] -> Integer -> Integer -> IO ()
betegarria_lag x n k
  | k >= 2^n = do
    putStrLn "\nSartutako formula ez da betegarria."
  | egiazkoa (x) (intToBool(aldatu (k) (2) (n))) = do
    putStrLn "\nSartutako formula betegarria da balorazio honekin: "
    print(intToBool(aldatu (k) (2) (n)))
  | otherwise = betegarria_lag x n (k+1)
--BETEGARRITASUNA---------------------------------------

--BALIOZKOTASUNA----------------------------------------
--Baliozkotasuna funtzioak, betegarritasunaren antzekoa da. Baina, egiazkoak False itzultzen badu, programa amaituko da mezu bat aurkeztuz. True izatekotan, 
--hurrengo balorazioekin probatuko du 2^n-1 baliora ailegatu arte, formula betegarria ez dela adieraziz.
baliozkoa:: [[Integer]] -> Integer -> IO () -- n*m*p
baliozkoa x n = do
  putStrLn "Formula baliozkoa den kalkulatuko da."
  baliozkoa_lag (x) (n) (0)
  
baliozkoa_lag:: [[Integer]] -> Integer -> Integer -> IO ()
baliozkoa_lag x n k
  | k >= 2^n = do
    putStrLn "\nSartutako formula baliozkoa da."
  | egiazkoa (x) (intToBool(aldatu (k) (2) (n))) = baliozkoa_lag x n (k+1)
  | otherwise = do
    putStrLn "\nSartutako formula ez da baliozkoa"
--BALIOZKOTASUNA----------------------------------------

--Balorazioak kalkulatzeko------------------------------
aldatu :: Integer -> Integer -> Integer -> [Integer]
aldatu x b n
  | (x < 0) = error "Zenbakia negatiboa da"
  | (b < 2) = error "Oinarria ez da zuzena"
  | (x > ((b^n)-1)) = error "Zenbakia handiegia da"
  | otherwise = [ (aldatu_lag x b n i) | i <- [1..n] ]

aldatu_lag :: Integer -> Integer -> Integer -> Integer -> Integer
aldatu_lag x b n i
  | (x < 0) = error "Zenbakia negatiboa da"
  | (b < 2) = error "Oinarria ez da zuzena"
  | (x > ((b^n)-1)) = error "Zenbakia handiegia da"
  | (i < 1) || (i > n) = error "Indizea ez da zuzena"
  | otherwise = (mod (div x (b^(n-i))) b)
--Balorazioak kalkulatzeko------------------------------

--Formula era egokian inprimatzeko----------------------
--Erabiltzaileak formula sartzen duenean, programak idatzitako formula pantailaratuko duen funtzioa da inprimatu.
--Formula 0,1 eta -1-ez osatuta dagoenez, era egokian inprimatuko dugu, inprimatu_lag erabilita. Hasteko, formularen 
--klausulak banan bana sartu egingo dira inprimatu_lag-en, kontagailu batekin eta zerrenda huts batekin.
--Azken funtzio honek, klausula bakoitza errekorrituko du, 1 edo -1 topatuz. Kasu hauetan, "x" edo "¬x" idatziko ditu [Char] zerrendan. 
--Lehen aipatutako kontagailuak, hauen atzean zenbaki hori inprimatuko du. 0 bat topatzean, ez du ezer egingo. Behin zerrenda osoa
--errekorritu dela, inprimatu funtzioak berriz deituko du inprimatu_lag, klausula guztiak begiratu arte.
--Azkenik, zuzendu funtzioak [Char] baten azken elementua ezabatuko du. Honen arrazoia, formula bakoitzean "¬" bat gelditzen delako da.
--Beraz, formula nolabait zuzentzeko,a zken karaktere hau ezabatuko dugu.
inprimatu:: [[Integer]] -> [Char] -> [Char]
inprimatu [] z = zuzendu(z)
inprimatu (x:y) z = inprimatu (y) (z++(inprimatu_lag (x) 0 []))

inprimatu_lag:: [Integer] -> Integer -> [Char] -> [Char]
inprimatu_lag [] k z = "("++zuzendu(z)++")^"
inprimatu_lag (x:y) k z
  | x == 1 = inprimatu_lag (y) (k+1) (z++("X"++show(k)++"v"))
  | x == -1 = inprimatu_lag (y) (k+1) (z++("¬X"++show(k)++"v"))
  | otherwise = inprimatu_lag (y) (k+1) (z)

zuzendu:: [Char] -> [Char]
zuzendu [] = []
zuzendu x = init(x)
--Formula era egokian inprimatzeko----------------------

--Beste funtzioak---------------------------------------

--Funtzio honek Integerrez osatutako zerrenda bat, boolearrez osatutako baten aldatuko du. Honen aplikazioa
--aldatu funtzioak integerren zerrenda bat ematen digunez, egiazkoa funtzioan boolear lista bat sartzeko izango da.
intToBool:: [Integer] -> [Bool] --n
intToBool [] = []
intToBool x
  | head(x) == 0 = False:(intToBool(tail x))
  | otherwise = True:(intToBool(tail x))

--Zenbaki bat teklatutik irakurriko du. Integer hau erabiltzaileak aldagai kopurua zehazteko izango da. Aurrebaldintza gisa, erabiltzaileak
--integer bat teklatu egin beharko du.
lortu_n:: IO Integer
lortu_n = do
  putStrLn "Sartu CNF formularen aldagai kopurua:"
  z <- getLine
  z <- (lortu_n_konprobatu (read z :: Integer))
  return (z)

--Erabiltzaileak 1 baino txikiago den Integer bat sartzen badu, lortu_n_konprobatu funtzioak, lortu_n deituko du
--beste zenbaki bat eskatzeko. Funtzioa bukatu egingo da, 1 edo handiago den Integer bat sartzean
lortu_n_konprobatu:: Integer -> IO Integer  
lortu_n_konprobatu x
  | x < 1 = do
    putStrLn "Zenbaki ez egokia. Zenbakia 1 edo handiago izan behar du."
    lortu_n
  | otherwise = return (x)

--Menuan 1etik 4ra bitarteko zenbakiak sartu behar direnez, lortu_n funtzioan egin dugun aplikazioa idatzi dugu, baina konprobatuko du zenbakia 
-- [1,4] tartean dagoela. Bestela, zenbakia berriz eskatuko.
lortu_aukera:: IO Integer
lortu_aukera = do
  z <- getLine
  z <- (lortu_aukera_konprobatu (read z :: Integer))
  return (z)

lortu_aukera_konprobatu:: Integer -> IO Integer  
lortu_aukera_konprobatu x
  | x < 1 || x > 4 = do
    putStrLn "Zenbaki ez egokia. Zenbakia 1 eta 4 artean egon behar da."
    lortu_aukera
  | otherwise = return (x)

--Erabiltzaileak balorazioa sartzen duen momentuan, konprobatuko dugu funtzio honen bidez, lehen sartu duen aldagai
--kopuruaren elementu kopuru berdina duela. Bestela, balorazioa berriz eskatuko dio. Aurrebaldintza bezala, boolearrez osatutako 
--zerrenda sartu behar du
lortu_balorazio:: Integer -> IO [Bool] 
lortu_balorazio x = do
  putStrLn "\nBalorazioa sartuko da (Lehen sartutako aldagaien kopurua eta balorazioaren tamainak berdinak izan behar dira)"
  putStrLn "Adibidez, 3 zenbakia sartu bada, balorazioa bat [True, False, False] izan daiteke."
  z <- getLine
  z <- (balorazioa_konprobatu (read z :: [Bool]) (x))
  return (z)

balorazioa_konprobatu:: [Bool] -> Integer ->  IO [Bool]
balorazioa_konprobatu x y
  | (toInteger(length (x)) == y) = return(x)
  | otherwise = do
    putStrLn "Sartutako balorazioa ez da egokia."
    lortu_balorazio (y)

--Erabiltzaileak sartu behar duen formulaz arduratuko den programak dira lortu_cnf, cnf_konprobatu eta cnf_konprobatu1. Lehengo honek, formula
--eskatuko dio erabiltzaileari, non aurrebaldintza bezala, Integerrez osatutako zerrenden zerrenda sartu beharko duen.
--cnf_konprobatu1 ikusiko du ea klausula batek 1, -1 edo 0 integerrez osatuta badagoen. cnf_konprobatu, cnf_konprobatu1 erabilita, formularen
--klausula guztiak konprobatuko ditu ea hauen luzera erabiltzaileak sartutako aldagaien kopuruaren berdin den eta 0,1 edo -1-ez osatuta dauden.
--Bi balditzetako bat ez bada betetzen, formula berri bat eskatuko dio erabiltzaileari.
lortu_cnf:: Integer -> IO [[Integer]]
lortu_cnf n = do
  z <- getLine
  z <- (cnf_konprobatu (n) (read z :: [[Integer]]))
  return (z)

cnf_konprobatu:: Integer -> [[Integer]] -> IO [[Integer]]
cnf_konprobatu n [] = return([])
cnf_konprobatu n x
  | (toInteger(length(head(x))) == n) && cnf_konprobatu1(head(x)) = do
    z <- cnf_konprobatu (n) (tail(x))
    return(head(x):z)
  | otherwise = do
    putStrLn "Sartutako formula ez da egokia. Klausularen bat tamaina ez egokia du edo sartutako zenbakiak ez daude [-1,1] tartean."
    lortu_cnf (n)

cnf_konprobatu1:: [Integer] -> Bool
cnf_konprobatu1 [] = True
cnf_konprobatu1 x
  | head(x) == 1 || head(x) == -1 || head(x) == 0 = cnf_konprobatu1(tail(x))
  | otherwise = False
--Beste funtzioak---------------------------------------
