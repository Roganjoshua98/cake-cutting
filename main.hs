import Data.List ( genericLength, sortOn, (\\) )
import GHC.Float ( divideFloat )
import Data.Ord (Down(Down))
import Sound.Tidal.Context (EventF(whole))

instance Show (a -> b) where
         show a= "function"

-- QUICK ACCESS FUNCTIONS

-- Divide and Choose
dac :: [Agent]
dac = divideAndChoose agent1 agent2 wholeCake

-- Lone Divider
ldAgents :: [Agent]
ldAgents = [agent1, agent2, agent3]

ld :: [Agent]
ld = ldTurns ldAgents wholeCake

-- Selfridge's Algorithm

sa :: [Agent]
sa = selfridgesAlgorithm agent1 agent2 agent3 wholeCake


-- TYPES AND CONSTANTS AND OBJECT METHODS

type Section = (Float, Float)
type Sections = [Section]
type Name = String
type Agent = (Name, Sections, Section -> Section -> Float)

wholeCake :: Section
wholeCake = (0,1)

tolerance :: Float
tolerance = 0.00001

agent1 :: Agent
agent1 = ("Al", [], eval)
agent2 :: Agent
agent2 = ("Bea", [], eval2)
agent3 :: Agent
agent3 = ("Carl", [], eval3)

showAgent :: Agent -> String
showAgent a = get1st a ++ " has " ++ show (length $ get2nd a) ++ " slice(s) of cake of total size " ++ show (getTotalAssignedLength a) ++ " which they value at " ++ show (get3rd a (head $ get2nd a) wholeCake)

getTotalAssignedLength :: Agent -> Float
getTotalAssignedLength a = sum $ map cakeLength (get2nd a)

showAgents :: [Agent] -> [String]
showAgents = map showAgent

get1st :: (a,b,c) -> a
get1st (x,_,_) = x
get2nd :: (a,b,c) -> b
get2nd (_,x,_) = x
get3rd :: (a,b,c) -> c
get3rd (_,_,x) = x

removeElement :: Eq a => a -> [a] -> [a]
removeElement _ []                 = []
removeElement x (y:ys) | x == y    = removeElement x ys
                    | otherwise = y : removeElement x ys


-- EVERYTHING TO DO WITH CUTTING

-- Cut(i , x , α); returns y : 
-- Agent i is asked to either indicate or cut the cake at place y such that the value agent i assigns to interval [x , y ] is equal to α. 
-- In other words: Vi([x,y])=α. -}
cut :: Agent -> Float -> Float -> Float -> Section
cut a x y v = if not $ get3rd a (cakeSlice x y) wholeCake `approxEqual` v
               then cut a x (y-tolerance) v
               else cakeSlice x y

cakeSlice ::  Float -> Float -> Section
cakeSlice x y
    | x < 0 || x > 1    = error "X is out of bounds"
    | y < 0 || y > 1    = error "Y is out of bounds"
    | otherwise         = (x,y)

--Tolerence level to allow for approx equivalence
approxEqual :: Float -> Float -> Bool
approxEqual a b = abs (a - b) <= tolerance

-- EVERYTHING TO DO WITH EVALUATION

-- Eval(i , x , y ); returns α: 
-- Agent i is asked to evaluate interval [x , y ]. The output of eval(i , x , y ) is therefore identical to Vi ([x , y ]).

cakeLength :: Section -> Float
cakeLength c = snd c - fst c

eval :: Section -> Section -> Float -- Flat and even eval
eval c wc = cakeLength c `divideFloat` cakeLength wc

eval2 :: Section -> Section -> Float -- Prefers the second half of the cake to the first
eval2 c wc
    | fst c < splitPoint && snd c <= splitPoint     = eval c wc * x
    | fst c >= splitPoint && snd c > splitPoint     = eval c wc * y
    | fst c < splitPoint && snd c > splitPoint      = (eval (fst c, splitPoint) wc * x) + (eval (splitPoint, snd c) wc * y)
    | otherwise                                     = error "Eval 2 broke"
    where splitPoint = 0.5
          x = 0.5
          y = 1.5

eval3 :: Section -> Section -> Float -- Has preferences for three different sections of the cake
eval3 c wc
    -- Entirely in section x
    | fst c < firstDivider && snd c <= firstDivider                             = eval c wc * x
    -- Entirely in section y
    | fst c >= firstDivider && snd c <= secondDivider                           = eval c wc * y
    -- Entirely in section z
    | fst c >= secondDivider && snd c > secondDivider                           = eval c wc * z
    -- In section x and y
    | fst c <= firstDivider && snd c <= secondDivider                           = (eval (fst c, firstDivider) wc * x) + (eval (firstDivider, snd c) wc * y)
    -- In section y and z
    | fst c >= firstDivider && fst c < secondDivider && snd c >= secondDivider  = (eval (fst c, secondDivider) wc * y) + (eval (secondDivider, snd c) wc * z)
    -- In all three sections
    | fst c < firstDivider && snd c > secondDivider                             = (eval (fst c, firstDivider) wc * x) + (eval (firstDivider, secondDivider) wc * y) + (eval (secondDivider, snd c) wc * z)
    -- Should never reach here!
    | otherwise                                                                 = error "Eval 3 broke"
    where firstDivider  = 0.2
          secondDivider = 0.7
          x = 0.75
          y = 1.7
          z = 0
{-
testEnvyFree :: [Agent] -> Bool
testEnvyFree as = let a1 = head as
                      a2 = as !! 1
                      a3 = as !! 2
                      t e cs = sum $ map e cs
                      tst ae ac = t (get3rd ae) (get2nd ac)
                      tsta1 = let x = tst a1 a1 in (x >= tst a1 a2) && (x >= tst a1 a3)
                      tsta2 = let x = tst a2 a2 in x >= tst a2 a1 && x >= tst a2 a3
                      tsta3 = let x = tst a3 a3 in x >= tst a3 a1 && x >= tst a3 a2
                  in tsta1 && tsta2 && tsta3
                  -}


-- EVERYTHING TO DO WITH ASSIGNMENT

-- Assign(i , x , y ); no return: 
-- Agent i is assigned the piece of cake denoted as interval [x , y ], where x < y .

assign :: Agent -> Section -> Agent
assign a s = (get1st a, [s], get3rd a)

assignAppend :: Agent -> Section -> Agent
assignAppend a s = (get1st a, get2nd a ++ [s], get3rd a)

filterAssigned :: [Agent] -> [Agent]
filterAssigned = filter (\x -> get2nd x /= [])  -- Filter list so it's only assigned agents
filterNotAssigned :: [Agent] -> [Agent]
filterNotAssigned = filter (null . get2nd) -- Filter list so it's only unassigned agents
filterAgent :: Agent -> [Agent] -> [Agent]
filterAgent a = filter (\x -> get1st x == get1st a) -- Select agent from list
filterNotAgent :: Agent -> [Agent] -> [Agent]
filterNotAgent a = filter (\x -> get1st x /= get1st a) -- Select all but agent from list

updateAgent :: Agent -> [Agent] -> [Agent]
updateAgent a as = filterNotAgent a as ++ [a]

-- DIVIDE AND CHOOSE for 2 agents

dacCut :: Agent -> Section -> Section
dacCut a c = uncurry (cut a) c 0.5  -- Cut to the agent's approximation of half

dacSlices :: Agent -> Section -> (Section, Section)
dacSlices a c = let x = dacCut a c
                in (x, (snd x, snd c)) -- Put the two slices from the one cut into a tuple

dacEvalSlices :: Agent -> Agent -> Section -> (Section, Section)
dacEvalSlices a1 a2 c = let s = dacSlices a1 c
                            get f = get3rd a2 (f s) wholeCake
                        in if get fst >= get snd then (snd s, fst s) else s -- Agent 2 evals both slices and chooses the preferred

divideAndChoose :: Agent -> Agent -> Section -> [Agent]
divideAndChoose a1 a2 c = let e = dacEvalSlices a1 a2 c
                           in [assign a1 (fst e), assign a2 (snd e)] -- List of assigned agents

-- LAST DIMINISHER (aka Trimming Algorithm) for n agents

ldV :: Float
ldV = 1 `divideFloat` genericLength ldAgents

ldCut :: Agent -> Section -> Section -- Cut approximately to 1/n 
ldCut a c = uncurry (cut a) c ldV

ldInspect :: Agent -> Section -> Agent -- Agent evals slice and cuts slice if they think it's too big
ldInspect a s = if ldV < get3rd a s wholeCake || ldV `approxEqual` get3rd a s wholeCake
                then assign a (ldCut a s)
                else a

ldAllInspect :: [Agent] -> Section -> [Agent] -- Agents inspect and are returned back to list.
ldAllInspect as s = [ldInspect a s | a <- as]

ldTurn :: [Agent] -> Section -> Agent -- Last agent to cut is assigned. All before are ignored
ldTurn as c = if length as > 1
              then last $ filterAssigned $ ldAllInspect as (ldCut (head as) c)
              else assign (head as) c

ldTurns :: [Agent] -> Section -> [Agent] -- Append assigned agent and go on to next turn
ldTurns _ (1,1) = []
ldTurns as c = let a = ldTurn (filterNotAssigned as) c in a : ldTurns (updateAgent a as) (snd (head $ get2nd a), snd c)

lastDiminisher :: [Agent] -> Section -> [Agent]
lastDiminisher = ldTurns

-- SELFRIDGE'S ALGORITHM for 3 agents

{-
Envy-Free Algorithm for Three Players
1) Have A1 cut three equal pieces which A2 ranks X1, X2, Xs from largest to smallest.
2) Have A2 trim off E from X1 (if necessary) so that X'1 = X1 - E and X2 have equal size.
3) From among X'1, X2 and X3 choose in the order A3, A2, and A1. If available, A2 must choose X'1.
4) Either A2 or A3 received X'1, call him person P1 and the other person P2. 
5) Have P2 cut E into three equal pieces which are chosen in the order P1, A1, P2.
-}

-- 1) Have A1 cut three equal pieces which A2 ranks X1, X2, Xs from largest to smallest.

saCut :: Agent -> Section -> Sections -- Have A1 cut three equal pieces
saCut a c = let cut1 = uncurry (cut a) c (1/3)
                c2 = (snd cut1, snd c)
                cut2 = uncurry (cut a) c2 (1/3)
                c3 = (snd cut2, snd c)
            in [cut1, cut2, c3]

saSort :: Agent -> Sections -> Section -> Sections -- A2 ranks the pieces from largest to smallest
saSort a cs wc = let es = sortOn Down $ [e wc | e <- map (get3rd a) cs]
                     getC e = head $ filter (\x -> get3rd a x wc == e) cs
                 in [getC e | e <- es]

saStep1 :: Agent -> Agent -> Section -> Sections -- Entirety of step 1
saStep1 a1 a2 c = saSort a2 (saCut a1 c) wholeCake

-- 2) Have A2 trim off E from X1 (if necessary) so that X'1 = X1 - E and X2 have equal size.

saTrim :: Agent -> Section -> Section -> Sections -- Have A1 trim off E from X1 so that X'1 = X1 - E and X'1 = X2
saTrim a c1 c2 = let e = uncurry (cut a) c1 (get3rd a c2 wholeCake)
                 in [e, (snd e, snd c1)]

saStep2 :: Agent -> Sections -> Sections -- Entirety of step 2
saStep2 a2 c =  let c1 = head c
                    c2 = c !! 1
                    x'1 = head $ saTrim a2 c1 c2
                    e = last $ saTrim a2 c1 c2
                in if get3rd a2 c1 wholeCake /= get3rd a2 c2 wholeCake
                   then x'1 : c2 : c!!2 : [e]
                   else c

-- 3) From among X'1, X2 and X3 choose in the order A3, A2, and A1. If available, A2 must choose X'1.

saChoose :: Agent -> Sections -> Section -> Agent
saChoose a cs wc = assignAppend a (head $ saSort a cs wc)

saChooseA2 :: Agent -> Sections -> Agent
saChooseA2 a cs = if get3rd a (head cs) wholeCake `approxEqual` get3rd a (cs !! 1) wholeCake
                  then assignAppend a (head cs)
                  else saChoose a cs wholeCake

saStep3 :: Agent -> Agent -> Agent -> Sections -> [Agent]
saStep3 a1 a2 a3 cs = let ch1 = saChoose a3 cs wholeCake
                          cs2 = removeElement (head $ get2nd ch1) cs
                          ch2 = saChooseA2 a2 cs2
                          cs3 = removeElement (head $ get2nd ch2) cs2
                          ch3 = saChoose a1 cs3 wholeCake
                      in [ch1, ch2, ch3]

-- 4) Either A2 or A3 received X'1, call him person P1 and the other person P2.

saStep4 :: [Agent] -> Sections -> [Agent]
saStep4 as cs = let a1 = last as
                    p1 = head $ filter (\x -> head (get2nd x) == head cs) as
                    p2 = if get1st (head as) == get1st p1 then as !! 1 else head as
                in [a1, p1, p2]

-- 5) Have P2 cut E into three equal pieces which are chosen in the order P1, A1, P2.

saCutE :: Section -> Float -> Sections
saCutE c v = let cl = (snd c - fst c) / 3
                 cutL = fst c + cl
                 cutR = snd c - cl
             in [(fst c, cutL), (cutL, cutR), (cutR, snd c)]

saChooseE :: Agent -> Agent -> Agent -> Sections -> Section -> [Agent]
saChooseE a1 p1 p2 cs wc = let ch1 = saChoose p1 cs wc
                               cs2 = removeElement (last $ get2nd ch1) cs
                               ch2 = saChoose a1 cs2 wc
                               cs3 = removeElement (last $ get2nd ch2) cs2
                               ch3 = saChoose p2 cs3 wc
                      in [ch1, ch2, ch3]

saStep5 :: Agent -> Agent -> Agent -> Section -> [Agent]
saStep5 a1 p1 p2 c = saChooseE a1 p1 p2 (saCutE c (1/3)) c

selfridgesAlgorithm :: Agent -> Agent -> Agent -> Section -> [Agent]
selfridgesAlgorithm a1 a2 a3 c = let s1 = saStep1 a1 a2 c
                                     s2 = saStep2 a2 s1
                                     s3 = saStep3 a1 a2 a3 s2
                                     s4 = saStep4 s3 s2
                                 in if length s2 == 4
                                    then saStep5 (head s4) (s4!!1) (last s4) (last s2)
                                    else s3


a2 = head sa
a1 = sa!!1
a3 = last sa
a1e :: Section -> Float
a1e c = get3rd a1 c wholeCake
a2e :: Section -> Float
a2e c = get3rd a2 c wholeCake
a3e :: Section -> Float
a3e c = get3rd a3 c wholeCake
a1c1 = head $ get2nd a1
a1c2 = last $ get2nd a1
a2c1 = head $ get2nd a2
a2c2 = last $ get2nd a2
a3c1 = head $ get2nd a3
a3c2 = last $ get2nd a3