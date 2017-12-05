--Para compilar ghc
--Para el interactivo ghci
--Para compilar con el interactivo: :l prueba.hs

--Diferentes modelos de test?? Como tratar eso.

--Modulo utilzado para transponer una lista de listas usando transpose
import Data.List

--Modulo utilizado para convertir [IO Int] en IO [Int] usando sequence
import Control.Monad

----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------Tipos de datos generales---------------------------------------------------

{- Cada pregunta individual constará de un identificador único, el número de 
   respuestas posibles, la respuesta correcta, y el valor que tendrá acertarla -}

type IdQuestion = String
type NumAnswers = Float
type RightAnswer = Int
type Value = Float

data Question = Question IdQuestion NumAnswers RightAnswer Value deriving Show

getIdQuestion :: Question -> String
getIdQuestion (Question s _ _ _) = s

getNumAnswers :: Question -> NumAnswers
getNumAnswers (Question _ num _ _) = num

getRightAnswers :: Question -> RightAnswer
getRightAnswers (Question _ _ ra _) = ra

getValue :: Question -> Value
getValue (Question _ _ _ v) = v

{- Cada test tendrá un identificador único y una lista de preguntas -}

type IdTest = String

data Test = Test IdTest [Question] deriving Show

getIdTest :: Test -> IdTest
getIdTest (Test id _ ) = id

getQuestions :: Test -> [Question]
getQuestions (Test _ qs ) = qs 

{- Cada modelo tendrá un identificador único, un identificador del test del que 
   es modelo y una lista que indica el reordenamiento de las preguntas.         -}

type IdModel = String

type OrderQuestion = [Int]

data Model = Model IdTest IdModel OrderQuestion deriving Show

getIdTest :: Model -> IdTest
getIdTest (Model idTest _ _) = idTest

getIdModel :: Model -> IdModel
getIdModel (Model _ id _) = id

{- Una respuesta a un test tendrá asociada un alumno, el identificador del modelo,
   y la lista de respuestas que ha dado al modelo correspondiente de un test   -}

type Answer = Int

data TestAnswer = TestAnswer Student IdModel [Answer] deriving Show

getStudent :: TestAnswer -> Student
getStudent (TestAnswer st _ _) = st

getIdTestAnswer :: TestAnswer -> IdTest
getIdTestAnswer (TestAnswer _ id _) = id

getAnswers :: TestAnswer -> [Answer]
getAnswers (TestAnswer _ _ ans) = ans

{- Un estudiante llevará un identificador único, un nombre y un apellido -}

data Student = Student { secondName :: String
                   , firstName :: String
                   , idStudent :: String
                   }deriving (Eq, Show, Read)

getNameSt :: Student -> String
getNameSt (Student secondName firstName idStudent) = firstName

----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------Correccion de un test------------------------------------------------------

--Represents a concret correction of a TestAnswered
type Punctuation = Float
type PunctuationOver10 = Float
type NumAnswered = Float
{- Una corrección de un examen lleva asociada el estudiante que ha respondido, la puntuación 
   total obtenida, la puntuación sobre diez y el número de preguntas que ha respondido -}
data Correction = Correction Student Punctuation PunctuationOver10 NumAnswered  deriving Show

getStudentCorrection :: Correction -> Student
getStudentCorrection (Correction st _ _ _ ) = st

getPunctuation :: Correction -> Punctuation
getPunctuation (Correction _ p _ _) = p
getP = getPunctuation

getPunctuation10 :: Correction -> PunctuationOver10
getPunctuation10 (Correction _ _ po _) = po
getP10 = getPunctuation10

getNumAnswered :: Correction -> NumAnswered
getNumAnswered ( Correction _ _ _ n ) = n

correctionToString :: Correction -> String
correctionToString correction = (" Alumno: " ++ (getNameSt (getStudentCorrection correction)) ++ "\n" ++ 
                                 "  Puntuacion total: " ++ (show (getPunctuation correction)) ++ "\n" ++ 
                                 "  Nota sobre 10: " ++ (show (getPunctuation10 correction)) ++ "\n")

--Corrects a test answer
corrige :: Test -> TestAnswer -> Correction

--Comprobamos  que son del mismo test y contamos la puntuacion.
corrige (Test id1 questions) (TestAnswer st id2 answers)
    | id1 == id2 = correct questions answers st (0, 0, 0)
    | otherwise = (Correction st 0 0 0)  
     
type MaxPunctuation = Float
--Corrects a list of questions giving a list of answers
correct :: [Question] -> [Answer] -> Student -> (Punctuation, MaxPunctuation, NumAnswered) -> Correction

correct [] _ st (punt, maxPunt, nAns) = (Correction st punt (punt/maxPunt*10) nAns)
correct _ [] st (punt, maxPunt, nAns) = (Correction st punt (punt/maxPunt*10) nAns)
correct ((Question _ numAns rans value):qs) (stAns:as) st (punt, max, nAns)
    | rans==stAns = correct qs as st ((punt+value), (max+value), (nAns+1))
    | stAns==emptyAnswer = correct qs as st (punt, (max+value), nAns)
    | otherwise = correct qs as st ((punt-(1/(numAns-1))), (max+value), (nAns+1))

--Corrects a list of test answers
correctListAnswers :: Test -> [TestAnswer] -> [Correction]
correctListAnswers _ [] = []
correctListAnswers test (a:as) = (corrige test a):(correctListAnswers test as)

----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------

--------------------------------------------Calculo general de estadisticas-------------------------------------------------

{- Unas estadísticas llevan asociadas la media, el número medio de preguntas respondidas, las 
   calificaciones obtenidas y las estadísticas sobre las preguntas del test asociado -}
data Statistics = Statistics Mean MeanAnswered Marks StatisticAnswer Tops deriving Show

getMean :: Statistics -> Mean
getMean ( Statistics m _ _ _ _ ) = m

getMeanAnswered :: Statistics -> MeanAnswered
getMeanAnswered ( Statistics _ mA _ _ _ ) = mA

getMarks :: Statistics -> Marks
getMarks ( Statistics _ _ mk _ _ ) = mk

getStatAns:: Statistics -> StatisticAnswer
getStatAns ( Statistics _ _ _ s _ ) = s

getTops:: Statistics -> Tops
getTops ( Statistics _ _ _ _ t ) = t

statisticsToString :: Statistics -> String
statisticsToString stats = (" Estadisticas: \n" ++ (meanToString (getMean stats)) ++ 
                           (mAnsToString (getMeanAnswered stats)) ++ (marksToString (getMarks stats)) ++ 
                           (sAnsToString (getStatAns stats)) ++ (topsToString (getTops stats)))

--Por ahora solo calcula la media y la media respondidas.
--Hay que utilizar funciones aux para ahorar duplicidades. 
--Computes statistics for a list of test answers
estadisticas :: Test -> [TestAnswer] -> Statistics
estadisticas test answers = Statistics (meanCorrections listCorrections) 
                                       (meanAnswered listCorrections) 
                                       (computeMarks listCorrections)
                                       statAnswers 
                                       (computeTops statAnswers)
                            where listCorrections = (correctListAnswers test answers) 
                                  statAnswers = (computeStatisticsAnswers test answers) 

----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------

--------------------------------------------Calculo de notas medias---------------------------------------------------------
type Mean = Float
meanToString :: Mean -> String
meanToString mean = ("  Media total: " ++ (show mean) ++ "\n")

--Takes a list of corrections test and returns the mean
meanCorrections :: [Correction] -> Mean
meanCorrections corrections = (meanCorrectionsAux corrections 0 0)

--Takes a list of corrections test, number of corrections and 
--the added puntuation and returns the mean of the results.
meanCorrectionsAux :: [Correction] -> Float -> Float -> Mean
meanCorrectionsAux [] added numC = added/numC
meanCorrectionsAux (c:cs) added numC = meanCorrectionsAux cs (added+(getPunctuation10 c)) (numC+1)

----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------Calculo de media de respondidas----------------------------------------
type MeanAnswered = Float

meanAnsweredToString :: MeanAnswered -> String
meanAnsweredToString mAns = ("  Numero medio de preguntas respondidas: " ++ (show mAns) ++ "\n")
mAnsToString = meanAnsweredToString

meanAnswered :: [Correction] -> MeanAnswered
meanAnswered corrections = (meanAnsweredAux corrections 0 0)

meanAnsweredAux :: [Correction] -> NumAnswered -> Float -> MeanAnswered
meanAnsweredAux []  added numCorrections = added/numCorrections
meanAnsweredAux (c:cs) added numCorrections = meanAnsweredAux cs (added + (getNumAnswered c)) (numCorrections+1)

----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------

-----------------------------------------------------Calculo de notas no numericas------------------------------------------

type Suspensos = Int
type Aprobados = Int
type Notables = Int
type Sobresalientes = Int

data Marks = Marks Suspensos Aprobados Notables Sobresalientes deriving Show

addMark :: Marks -> String -> Marks
addMark (Marks ss ap nt sb) mark
    | mark == "ss" = (Marks (ss+1) ap nt sb)
    | mark == "ap" = (Marks ss (ap+1) nt sb)
    | mark == "nt" = (Marks ss ap (nt+1) sb)
    | otherwise = (Marks ss ap nt (sb+1))

marksToString :: Marks -> String
marksToString (Marks ss ap nt sb) = ("  Notas totales" ++ ['\n'] ++ 
                                     "   Suspensos: " ++ (show ss) ++ 
                                     "   Aprobados: " ++ (show ap) ++ 
                                     "   Notables: " ++ (show nt) ++ 
                                     "   Sobresalientes: " ++ (show sb) ++ ['\n'])

--Takes a list of correction tests and returns the non numeric marks
computeMarks :: [Correction] -> Marks
computeMarks corrections = (computeMarksAux corrections (Marks 0 0 0 0))

computeMarksAux :: [Correction] -> Marks -> Marks
computeMarksAux []  marks = marks
computeMarksAux (c:cs) marks
    | notas < 5 = computeMarksAux cs (addMark marks "ss")
    | notas < 7 = computeMarksAux cs (addMark marks "ap")
    | notas < 9 = computeMarksAux cs (addMark marks "nt")
    | otherwise = computeMarksAux cs (addMark marks "sb")
    where notas = (getP10 c)

----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------

--------------------------------------------Calculo de estadisticas para preguntas------------------------------------------

data StatisticAnswer = StatisticAnswer [Frequency] deriving Show

getListFrequency :: StatisticAnswer -> [Frequency]
getListFrequency (StatisticAnswer list) = list

statisticAnswerToString :: StatisticAnswer -> String
statisticAnswerToString sAns = "  Frecuencia de resultados por pregunta: \n" ++ 
                               (statisticAnswerToStringAux (getListFrequency sAns) "")
sAnsToString = statisticAnswerToString

statisticAnswerToStringAux :: [Frequency] -> String -> String
statisticAnswerToStringAux [] str = str
statisticAnswerToStringAux (freq:fs) str = statisticAnswerToStringAux fs (str ++ (frequencyToString freq) ++ ['\n'])

--Takes a list of questions and a list of test answers and returns
--all the statistics about the questions.
computeStatisticsAnswers :: Test -> [TestAnswer] -> StatisticAnswer
computeStatisticsAnswers test list = statisticAnswer (getQuestions test) (unpackAnswerTest list)

--Returns statistics about all the questions in the list. 
statisticAnswer :: [Question] -> [[Answer]] -> StatisticAnswer
statisticAnswer questions answers = StatisticAnswer (statisticAnswerAux questions (transposeAnswers answers))

--Auxiliar function to statisticAnswer function. 
--Computes a list of frequency data from answers.
--The rows are the multiple answers for a unique question
statisticAnswerAux :: [Question] -> [[Answer]] -> [Frequency]
statisticAnswerAux [] _ =  []
statisticAnswerAux (q:qs) (law:laws) = (frequencyQuestion q law):(statisticAnswerAux qs laws)

--Returns a list of answer lists 
-- from a list of answer tests
unpackAnswerTest :: [TestAnswer] -> [[Answer]]
unpackAnswerTest [] = []
unpackAnswerTest (tan:tans) = (getAnswers tan):(unpackAnswerTest tans)

--Function to transpose a matrix of answer (using Data.List library)
transposeAnswers :: [[Answer]] -> [[Answer]]
transposeAnswers listOfLists = transpose listOfLists

----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------Subcalculo de frecuencias--------------------------------------------------

type Correct = Float
type Wrong = Float
type Blank = Float

data Relative = Relative Correct Wrong Blank deriving Show

relativeToString :: Relative -> String
relativeToString (Relative c w b) = (" F.Rel: C->" ++ (show c) ++ " I->" ++ (show w) ++ " B->" ++ (show b) ++ "\n")

data Absolute = Absolute Correct Wrong Blank deriving Show

absoluteToString :: Absolute -> String
absoluteToString (Absolute c w b) = ("                F.Abs: C->" ++ (show c) ++ " I->" ++ (show w) ++ " B->" ++ (show b))

data Frequency = Frequency IdQuestion Absolute Relative deriving Show

getIdFrequency :: Frequency -> String
getIdFrequency (Frequency id _ _ ) = id

getCorrectRelative :: Frequency -> Correct
getCorrectRelative (Frequency _ _ (Relative c _ _)) = c

getWrongRelative :: Frequency -> Wrong
getWrongRelative (Frequency _ _ (Relative _ w _)) = w

getBlankRelative :: Frequency -> Blank
getBlankRelative (Frequency _ _ (Relative _ _ b)) = b

emptyFrequency = (Frequency " " (Absolute 0 0 0) (Relative 0 0 0))
emptyAnswer = -1

frequencyToString :: Frequency -> String
frequencyToString (Frequency id abs rel) = ("   Pregunta " ++ id ++ ": " ++ (relativeToString rel) ++ (absoluteToString abs))

--Returns frequency and data about the answers of a question
frequencyQuestion :: Question -> [Answer] -> Frequency
frequencyQuestion question answers = frequencyQuestionAux question answers 0 0 0

--Auxiliar function to compute frequencys of a single question
frequencyQuestionAux :: Question -> [Answer] -> Correct -> Wrong -> Blank -> Frequency
frequencyQuestionAux q [] relc relw relb = Frequency (getIdQuestion q) (Absolute relc relw relb) 
                                                     (Relative (relc/(relc+relw+relb)) (relw/(relc+relw+relb)) 
                                                                                       (relb/(relc+relw+relb)))

frequencyQuestionAux (Question id max answer value) (a:as) relc relw relb
    | answer == a = frequencyQuestionAux (Question id max answer value) as (relc+1) relw relb
    | a < 0 = frequencyQuestionAux (Question id max answer value) as relc relw  (relb+1)
    | otherwise =  frequencyQuestionAux (Question id max answer value) as relc (relw+1)  relb

----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------Subcalculo de los tops-----------------------------------------------------
type Best = IdQuestion
type Worst = IdQuestion
type Blankest = IdQuestion
type LessBlank = IdQuestion

data Tops = Tops Best Worst Blankest LessBlank deriving Show

--Computes the best, the worst and the blankest question
computeTops :: StatisticAnswer -> Tops
computeTops sAns = (Tops (highestQuestion list) (lowestQuestion list) (fst blank ) (snd blank)) 
                   where blank = (blankestQuestion list) 
                         list = (getListFrequency sAns)

--Se puede calcular la mejor y la peor en la misma funcion

--Computes the best question
highestQuestion :: [Frequency] -> Best
highestQuestion [] = ""
highestQuestion list = highestQuestionAux list (head list)

--Auxiliar function of highestQuestion
highestQuestionAux :: [Frequency] -> Frequency -> Best
highestQuestionAux [] highest = getIdFrequency highest
highestQuestionAux (f:fs) highest
    | (getCorrectRelative f) > (getCorrectRelative highest) = highestQuestionAux fs f
    | otherwise = highestQuestionAux fs highest

--Computes the worst question
lowestQuestion :: [Frequency] -> Worst
lowestQuestion [] = ""
lowestQuestion list = lowestQuestionAux list (head list)

--Auxiliar function for lowestQuestion (Usar <= para que cambie resp. a highest)
lowestQuestionAux :: [Frequency] -> Frequency -> Worst
lowestQuestionAux [] lowest = getIdFrequency lowest
lowestQuestionAux (f:fs) lowest
    | (getWrongRelative f) > (getWrongRelative lowest) = lowestQuestionAux fs f
    | otherwise = lowestQuestionAux fs lowest

--Computes the top of blank questions
blankestQuestion :: [Frequency] -> (Blankest, LessBlank)
blankestQuestion [] = ("","")
blankestQuestion (h:list) = blankestQuestionAux list (h,h)

--Auxiliar function for blankestQuestion (Un <= para una de ellas seria interesante)
blankestQuestionAux :: [Frequency] -> (Frequency, Frequency) -> (Blankest, LessBlank)
blankestQuestionAux [] (highest, lowest) = (getIdFrequency highest, getIdFrequency lowest)
blankestQuestionAux (f:fs) (highest, lowest)
    | (getBlankRelative f) > (getBlankRelative highest) = blankestQuestionAux fs (f, lowest)
    | (getBlankRelative f) < (getBlankRelative lowest) = blankestQuestionAux fs (highest, f)
    | otherwise = blankestQuestionAux fs (highest, lowest)

topsToString :: Tops -> String
topsToString (Tops b w bh bl) = ("  Preguntas con resultados extremos: " ++ "\n" ++
                                 "   Pregunta con mejores resultados: " ++ b ++ ['\n'] ++ 
                                 "   Pregunta con peores resultados: " ++ w ++ ['\n'] ++ 
                                 "   Pregunta mas veces en blanco: " ++ bh ++ ['\n'] ++ 
                                 "   Pregunta menos veces en blanco: " ++ bl ++ ['\n'])

----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------

-------------------------------------------Interaccion con el usuario-------------------------------------------------------

main :: IO ()
main = do
    putStrLn "\n Gestor de tests:\n"
    putStrLn "  1.- Correccion de un test."
    putStrLn "  2.- Estadisticas de una lista de respuestas."
    putStrLn "  3.- Testing automatico.\n"
    putStr " KikeAplication> "
    election <- getLine
    case election of
        "1" -> testCorrection
        "2" -> statisticsComputation
        "3" -> testing
        _ -> putStrLn "Closing app \n"

--Prints a test correction, that recives from the user
testCorrection :: IO ()
testCorrection = do
    myTest <- testIntroduction
    myAnswer <- testAnswersIntroduction
    putStrLn (correctionToString (corrige myTest myAnswer))

--Recives a Test from the user
testIntroduction :: IO Test
testIntroduction = do
    putStr "  Introduzca id del test: "
    idT <- getLine
    putStr "  Indique cuantas preguntas va a tener: "
    numQ <- getLine
    preguntas <- (questionsIntroduction ((read numQ)::Int) [])
    return (Test idT preguntas)
    
--Computes a list of question to be introduced by the user
questionsIntroduction :: Int -> [IO Question] -> IO [Question]
questionsIntroduction numQ list = do
    if numQ > 0 then (questionsIntroduction (numQ-1) ((qIntr numQ):list))                     
                else (sequence list)

--Recives a question from the user
qIntr :: Int -> IO Question
qIntr number = do
    putStr ("  Id de la pregunta " ++ (show number) ++ ": ")
    idr <- getLine
    putStr "  Numero de respuestas:"
    nRes <- getLine
    putStr "  Respuesta correcta:"
    correcta <- getLine
    putStr "  Valor de pregunta:"
    valor <- getLine
    return (Question idr ((read nRes)::Float) ((read correcta)::Int) ((read valor)::Float) )

--Recives a TestAnswer from the user
testAnswersIntroduction :: IO TestAnswer
testAnswersIntroduction = do 
    st <- stIntr
    idT <- idIntr
    putStr "  Indique cuantas respuestas va a tener: "
    numA <- getLine
    resp <- (answersIntroduction ((read numA)::Int) [])
    return (TestAnswer st idT resp) 
                            
--Recives an answer from the user
answersIntroduction :: Int -> [IO Answer] -> IO [Answer]
answersIntroduction numA lis = do 
    if numA > 0 then answersIntroduction (numA-1) (aIntr:lis)
                else sequence lis

--Recives a student from the user
stIntr :: IO Student
stIntr = do
    putStr "  Nombre: "
    first <- getLine
    putStr "  Apellido: "
    second <- getLine
    putStr "  Identificador: "
    iden <- getLine
    return (Student second first iden)

--Recives a Test from the user
idIntr :: IO IdTest
idIntr = do
    putStr "  Identificador del test: "
    respuesta <- getLine
    return respuesta

--Recives a answer from the user
aIntr :: IO Answer
aIntr = do
    putStr "  Respuesta: "
    respuesta <- getLine
    return ((read respuesta)::Int)

--Prints statistics from an user introduction
statisticsComputation :: IO ()
statisticsComputation = do
    myTest <- testIntroduction
    putStr "  Indique cuantos test va a corregir: "
    numA <- getLine
    theListAnswer <- (listTestAnswersIntroduction ((read numA)::Int) [])
    putStrLn (statisticsToString (estadisticas myTest theListAnswer))

--Computes a list of TestAnswer to be introduced by the user
listTestAnswersIntroduction :: Int -> [IO TestAnswer] -> IO [TestAnswer]
listTestAnswersIntroduction numA list = do 
    if numA > 0 then (listTestAnswersIntroduction (numA-1) (testAnswersIntroduction:list))                     
                else (sequence list)

----------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------

------------------------------------------Database for automatic testing----------------------------------------------------

kike = (Student "Curry" "Kike" "001")
juan = (Student "Zidane" "Juan" "002")
messi = (Student "Messi" "Lionel" "003")
esteban = (Student "Granero" "Esteban" "004")
perelman = (Student "Aguirre" "Jesus" "005")
david = (Student "Gomez" "David" "006")
victor = (Student "Adolfo" "Victor" "007")

pregunta1 = Question "1" 5 2 1
pregunta2 = Question "2" 5 3 1
pregunta3 = Question "3" 5 1 1
pregunta4 = Question "4" 5 4 1
pregunta5 = Question "5" 5 3 1
pregunta6 = Question "6" 5 3 1
pregunta7 = Question "7" 5 2 1
pregunta8 = Question "8" 5 1 1
pregunta9 = Question "9" 5 2 1
pregunta10 = Question "10" 5 3 1

test1 = Test "0001" [pregunta1, pregunta2, pregunta3]
test2 = Test "0002" [pregunta1, pregunta2, pregunta3, pregunta4, pregunta5, pregunta6]
test3 = Test "0003" [pregunta1, pregunta2, pregunta3, pregunta4, 
                     pregunta5, pregunta6,pregunta7, pregunta8, pregunta9, pregunta10]

respuesta1 = TestAnswer kike "0001" [2,3,1]
respuesta2 = TestAnswer messi "0001" [1,2,1]
respuesta3 = TestAnswer esteban "0001" [2,-1,2]
respuesta4 = TestAnswer juan "0001" [2,3,2]

r1 = TestAnswer kike "0003"     [2,3,1,3,2,4,3,2,4,1 ]
r2 = TestAnswer messi "0003"    [1,2,1,-1,4,2,2,3,2,4]
r3 = TestAnswer esteban "0003"  [2,-1,1,4,3,4,1,3,2,3]
r4 = TestAnswer juan "0003"     [2,3,2,-1,3,4,4,3,4,1]
r5 = TestAnswer perelman "0003" [2,3,1,3,2,4,2,1,2,-1]
r6 = TestAnswer david "0003"    [1,2,1,-1,4,2,2,1,2,3]
r7 = TestAnswer victor "0003"   [2,-1,1,4,3,3,2,1,2,3]

m1 = Model test1 "Modelo 001" ["1","2","3"]
m2 = Model test1 "Modelo 002" ["3","2","1"]

l1 = [respuesta1, respuesta2, respuesta3, respuesta4]
l3 = [r1, r2, r3, r4, r5, r6, r7]

testing :: IO()
testing = putStr (testCorrige ++ "\n\n" ++ testEstadisticas)

testCorrige = (correctionToString (corrige test1 respuesta1))
testEstadisticas = (statisticsToString (estadisticas test1 l1))

