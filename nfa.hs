module NFA where

import Prelude

type State = String
type Transition = (State, String, State)
type Automat = (State, [State], [Transition])


--transNFA :: [Transition]
--transNFA = [(0, "a", 0), (0, "a", 1), (0, "b", 1), (1, "b", 0), (1, "a", 1)]

--a2 :: Automat
--a2 = (1, transNFA)

makeSet :: Eq a => [a] -> [a]
makeSet [] = []
makeSet (x:xs) | elem x xs = makeSet xs
               | otherwise = x : (makeSet xs)

findEpsilons :: State -> [Transition] -> [State]
findEpsilons x ts = x : findEpsilonsInner x ts
  where
    findEpsilonsInner :: State -> [Transition] -> [State]
    findEpsilonsInner x [] = []
    findEpsilonsInner x ((from, ch, to):ts) =
      if ch == "" && x == from then to: findEpsilonsInner x ts
      else findEpsilonsInner x ts        

followEpsilons :: [State] -> [Transition] -> [State]
followEpsilons states ts = findNext states []
  where
    findNext :: [State] -> [State] -> [State]
    findNext [] res = res
    findNext (x:xs) res = findNext xs (makeSet (res ++ followers))
      where
        followers = findEpsilons x ts
        
followEpsilonsRec :: [State] -> [Transition] -> [State]
followEpsilonsRec states ts = followEpsilonsInner states []
  where
    followEpsilonsInner states lastStates =
      if (length lastStates) < (length nextStates) then followEpsilonsInner nextStates states
      else nextStates
      where
        nextStates = followEpsilons states ts

testNFA :: Automat -> String -> Bool
testNFA (s,f,ts) input = moveNFA (s,f,ts) input (followEpsilonsRec [s] ts)

moveNFA :: Automat -> String -> [State] -> Bool
moveNFA a "" states = isFinalNFA a states
moveNFA (s,f,ts) (x:input) states = moveNFA (s,f,ts) input (followEpsilonsRec (stepNFA states x ts) ts)

isFinalNFA :: Automat -> [State] -> Bool
isFinalNFA (s,f,ts) states = elem (head f) states

stepNFA :: [State] -> Char -> [Transition] -> [State]
stepNFA states input trans = makeSet [z | (x,y,z) <- trans, elem x states && isValidTransition y input]
  where
    isValidTransition :: String -> Char -> Bool
    isValidTransition s c = if length s == 0 then False
                            else (head s) == c

printNFA :: Automat -> IO ()
printNFA (s,f,ts) = putStr (concat $ map (++ "\n") ls)
  where
    ls = [s, (head f)] ++ (map (\(f,c,t) -> f ++ " " ++ c ++ " " ++ t ) ts)

-- merging
lengthNFA :: Automat -> Int
lengthNFA (s,f,ts) = length (makeSet $ countStates ts)
  where
    countStates [] = []
    countStates ((f,c,t):ts) = f:t:countStates ts

prefixNFA :: String -> Automat -> Automat
prefixNFA str (s,f,ts) = (str ++ s, (map (\x -> str ++ x) f), (map (\(f,c,t) -> (str ++ f, c, str ++ t)) ts))

stringNFA :: String -> Automat
stringNFA "" = ("", [], [])
stringNFA (x:xs) = stringNFAInner xs ("", [], [("", [x], [x])]) [x]
  where
    stringNFAInner :: String -> Automat -> State -> Automat
    stringNFAInner "" (s,f,ts) state = (s,[state],ts)
    stringNFAInner (x:xs) (s,f,ts) state = stringNFAInner xs (s,f, ts ++ [moveChar]) (state ++ [x])
      where
        moveChar = (state, [x], (state ++ [x]))

getPrefixedNFAs :: Automat -> Automat -> (Automat, Automat)
getPrefixedNFAs a1 a2 = (prefixNFA (prefix ++ "0") a1, prefixNFA (prefix ++ "1") a2)
  where
    prefix = show (lengthNFA a1 + lengthNFA a2)

alternativeNFA :: Automat -> Automat -> Automat
alternativeNFA a1 a2 = ("", [finalState], ts1Prefixed ++ ts2Prefixed ++ startArcs ++ endArcs)
  where
    ((s1Prefixed, f1Prefixed, ts1Prefixed), (s2Prefixed, f2Prefixed, ts2Prefixed)) = getPrefixedNFAs a1 a2
    startArcs = [("", "", s1Prefixed), ("", "", s2Prefixed)]
    endArcs = [((head f1Prefixed), "", finalState), ((head f2Prefixed), "", finalState)]
    finalState = "alternativeFinal"

concatNFA :: Automat -> Automat -> Automat
concatNFA a1 a2 = (s1Prefixed, f2Prefixed, ts1Prefixed ++ ts2Prefixed ++ [((head f1Prefixed), "", s2Prefixed)])
  where
    ((s1Prefixed, f1Prefixed, ts1Prefixed), (s2Prefixed, f2Prefixed, ts2Prefixed)) = getPrefixedNFAs a1 a2
    
iterateNFA :: Automat -> Automat
iterateNFA a = ("", [finalState], tsPrefixed ++ startArcs ++ endArcs)
  where
    (sPrefixed, fPrefixed, tsPrefixed) = prefixNFA prefix a
    prefix = (show $ lengthNFA a) ++ "i"
    startArcs = [("", "", sPrefixed), ("", "", finalState)]
    endArcs = [((head fPrefixed), "", finalState), ((head fPrefixed), "", sPrefixed)]
    finalState = "iterationFinal"
    