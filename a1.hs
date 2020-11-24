import RainbowAssign
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8            -- length of each password
nLetters = 5            -- number of letters to use in passwords: 5 -> a-e
width = 40              -- length of each chain in the table
height = 1000           -- number of "rows" in the table

--pwLength = 5
--nLetters = 18
--width = 60
--height = 800

filename = "table.txt"  -- filename to store the table

testhash :: Hash
testhash = 1171785135

generateTable :: IO ()
generateTable = do
  table <- buildTable rainbowTable nLetters pwLength width height
  writeTable table filename

test1 :: IO [Passwd]
test1 = do
    table <- readTable filename
    return (findHitsinRainTable table width testhash)

test2 :: Int -> IO ([Passwd], Int)
test2 n = do
      table <- readTable filename
      pws <- randomPasswords nLetters pwLength n
      let hs = map pwHash pws
      let result = Maybe.mapMaybe (findPassword table width) hs
      return (result, length result)

test3 :: IO (Maybe Passwd)
test3= do
    table <- readTable filename
    return (findPassword table width testhash)

main :: IO ()
main = do
      generateTable
      res <- test2 10000
      print res
----------------------------

-- | Finds lowest-order digit by hash value mod nLetters, 
-- | Then recurse using div. Is in lowest order -> highest order arrangement
-- | Uses Haskell's Lazy Evaluation to not store inf list
convertBase :: Int -> [Int]    
convertBase x = [remainder] ++ convertBase nextDig
              where 
                remainder = mod x nLetters; 
                nextDig = div x nLetters

-- | Main pwReduce function. 
-- | lstDigits gets a list of digits post-conversion in least significant to most significant 
-- | The list is then shortened to needed values, and reversed. Then each digit is mapped to a letter
-- | Used https://stackoverflow.com/questions/4880376/int32-to-int-in-haskell/4880399
pwReduce :: Hash -> Passwd
pwReduce hash = map toLetter value
              where 
                value = reverse(take pwLength lstDigits)
                lstDigits = convertBase (fromIntegral hash) 

----------------------------

-- | Recursive function acting as loop so the hashing function is run width+1 times
-- | Add orgPswds to store first iteration of passwords, and lstPswds to hold current set post-pwReduce
-- | Used https://wiki.haskell.org/How_to_work_on_lists
rainbowTableLoop  :: Int -> [Passwd] -> [Passwd] ->  Map.Map Hash Passwd                                            
rainbowTableLoop 0 orgPswds lstPswds =  Map.fromList (zip hashedPswds orgPswds)
                                            where 
                                              hashedPswds = map pwHash lstPswds

rainbowTableLoop wdth orgPswds lstPswds  = rainbowTableLoop (wdth-1) orgPswds newPswds
                                            where 
                                              newPswds = map pwReduce (map pwHash lstPswds) 

-- | Main rainbowTable function. 
-- | If wdth is negative or no passwords given, return an empty map
-- | else enter rainTableLoop to do the work
rainbowTable :: Int -> [Passwd] ->  Map.Map Hash Passwd
rainbowTable wdth lstPswds
  | wdth < 0 || lstPswds == []           = Map.empty
  | otherwise                            = rainbowTableLoop wdth lstPswds lstPswds

----------------------------

-- | Looks if hash value is one of the ending hash values by using lookupHash to search the entire table
-- | If not, recurse by rehashing and lowering width by 1
-- | Returns list of all valid hits/candidate rows, [] otherwise
-- | Fellow students on Discord ponted me to Map.lookup for searching function in maps: https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html
findHitsinRainTable :: Map.Map Hash Passwd -> Int -> Hash -> [Passwd]
findHitsinRainTable _ (-1) _ = []
findHitsinRainTable rainTable wdth hash 
    | Maybe.isJust(lookupHash)            = [Maybe.fromJust lookupHash] ++ findHitsinRainTable rainTable (wdth-1) newHash
    | otherwise                           = findHitsinRainTable rainTable (wdth-1) newHash
      where
        newHash = pwHash(pwReduce hash)
        lookupHash = Map.lookup hash rainTable

-- | Checks every candidate row found by hits to see if it returns a valid password (work done in findMatchingPswd)
-- | Returns the first valid password found, or Nothing is none of the candidate rows contain matches
findPasswordLoop :: Int -> Hash -> [Passwd] -> Maybe Passwd
findPasswordLoop _ _ [] = Nothing
findPasswordLoop wdth hash lstHits
  | Maybe.isJust(match)                   = match
  | otherwise                             = findPasswordLoop wdth hash (tail lstHits)
    where 
      match = findMatchingPswd (head lstHits) wdth hash

-- | Runs through rainbow table row based on password, checking if a hashed hit matches original hash value
-- | If it does, return it. If it doesn't, loop with the next Pswd in the row until wdth is run through 
findMatchingPswd :: Passwd -> Int -> Hash -> Maybe Passwd
findMatchingPswd  _ (-1) _ = Nothing 
findMatchingPswd pswd wdth hash 
  | pwHash pswd == hash                   = Just pswd
  | otherwise                             = findMatchingPswd nextPswd (wdth-1) hash
    where
      nextPswd = pwReduce (pwHash pswd)

-- | Main findPassword function. 
-- | If wdth is negative or the table is null there are no items so return Nothing
-- | else call and return looping/recursive FindPasswordLoop function with a list of candidate row passwords
-- | Used algorithm described in Piazza Post #41: https://piazza.com/class/keem0n9xrvq1ew?cid=41
findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword rainTable wdth hash
  | wdth < 0 || Map.null rainTable        = Nothing
  | otherwise                             = findPasswordLoop wdth hash lstHits    
  where
    lstHits = findHitsinRainTable rainTable wdth hash
