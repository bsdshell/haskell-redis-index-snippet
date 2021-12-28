-- {{{ begin_fold
-- script
-- #!/usr/bin/env runhaskell -i/Users/cat/myfile/bitbucket/haskelllib
-- {-# LANGUAGE OverloadedStrings #-}
-- import Turtle
-- echo "turtle"
{-# LANGUAGE DuplicateRecordFields #-} 
-- {-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- import Data.Set   -- collide with Data.List 
import Control.Monad
import Data.List.Split
import Data.Time
import Data.Time.Clock.POSIX
import Data.Char
import Data.Maybe(fromJust, fromMaybe)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Posix.Files
import System.Posix.Unistd
import System.Process
--import Text.Read
import Text.Regex
import Text.Regex.Base
import Text.Regex.Base.RegexLike
import Text.Regex.Posix
import Data.IORef 
import Data.Typeable (typeOf)
import Control.Monad (unless, when, liftM, liftM2, liftM3)
import Control.Monad.IO.Class
import Control.Concurrent 
import Database.Redis
import GHC.Generics

import qualified Text.Regex.TDFA as TD
import qualified Data.Array as DR 
import qualified Data.List as L
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as SB -- strict ByteString 
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DL 
import qualified Data.Map.Strict as M
import qualified Data.Aeson as DA
import qualified Data.Aeson.Text as AAT  -- encodeToLazyText

import AronModule 

{-| 
    1 Read Java and Haskell library
    2 Parse all functions/methods from Aron.java and AronModule.hs
    3 Build prefix string from name of functions/methods
    4 insert data to Redis
    5 insert $h/AronModule.hs, $b/javalib/AronLib.java and $b/snippets/snippet.hs to Redis

    KEY: Insert AronModule.hs, javalib/AronLib.java and snippets/snippet.hs to Redis, insert snippet, redis snippet
-} 
type MMap a b = M.Map a [b]

printBlock::[[DL.Text]] -> IO ()
printBlock cx = (mapM_ . mapM_) (putStrLn . lazyTextToStr) cx 

testFile = "myfile/bitbucket/testfile/test.txt"
jAron = "myfile/bitbucket/javalib/Aron.java"
jPrint = "myfile/bitbucket/javalib/Print.java"
-- jPrint =  "/Users/cat/myfile/bitbucket/testfile/Print_test.java"
hname = "myfile/bitbucket/haskelllib/AronModule.hs"
-- hname = "/Users/cat/myfile/bitbucket/testfile/AronModule_test.hs"
-- cppfile = "/tmp/cpp.h"
cppfile = "myfile/bitbucket/cpplib/AronLib.h"

-- geneMap2 bs ws ["line1", "line2"] -> [([k0, k1], 1, ["line1"])]
geneMap2::String->String-> [String] -> [([String], Integer, [String])]
geneMap2 _ _ [] = [] 
geneMap2 bs ws cx = zblock 
    where
     block = filter(\x -> len x > 0) $ splitBlock2 cx bs 
     sblock = map(\k -> (unique $ join $ map(\x -> filter(\e -> len e > 0 && isWord e) $ splitStrChar ws x) k, k)) block
     zblock = zipWith(\x y -> (fst y, x, snd y)) [0..] sblock


splitBlock2::[String] -> String -> [[String]]
splitBlock2 [] _ = []
splitBlock2 cx pat = splitWhen (\x -> matchTest (mkRegex pat) x) cx

{-| 
    [([k0, k1], a0)] ,  Map => k0 -> a0
                               k1 -> a0
-} 
addMore::(Ord e)=>[([e], a)] -> M.Map e [a]-> M.Map e [a] 
addMore [] m = m
addMore (s:cs) m = addMore cs $ add s m 

{-| 
    ===
    ([k0, k1], a0) , Map => k0 -> a0
                            k1 -> a0
-} 
add::(Ord e)=>([e], a) -> M.Map e [a] -> M.Map e [a] 
add ([], _)   m = m
add (s:cs, n) m = case ls of 
                       Just x -> add (cs, n) $ M.insert s (n:x) m  -- contains s
                       _      -> add (cs, n) $ M.insert s [n] m    -- not contains s
    where
        ls = M.lookup s m -- Maybe a

data Block = Block{bblock::[DL.Text]} deriving (Generic, Show)
data MBlock = MBlock{mblock::[Integer]} deriving (Generic, Show)
-- data Block = Block{bblock::[[ByteString]]} deriving (Generic, Show)

instance DA.FromJSON Block 
instance DA.ToJSON Block where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = DA.genericToEncoding DA.defaultOptions

instance DA.FromJSON MBlock 
instance DA.ToJSON MBlock where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = DA.genericToEncoding DA.defaultOptions


-- strToBS::String -> SB.ByteString
-- strToBS s = BS.pack s

fun::SB.ByteString -> Redis SB.ByteString
fun key = do
    result <- get key
    case result of
        Left _ -> return $ BSU.fromString "Some error occurred"
        Right v -> return $ fromMaybe (BSU.fromString "Could not find key") v

{-| 
    === Say sth
-} 
enMapBlock1::M.Map String [Integer] -> [(SB.ByteString, SB.ByteString)]
enMapBlock1 gmap = L.map(\s -> 
                            (toSBS $ fst s, 
                             toStrictBS $ DA.encode $ MBlock{mblock = snd s}
                            )
                        ) $ M.toList gmap -- > [(ByteString, [[ByteString]])]

enMapBlock2::M.Map Integer [String] -> [(SB.ByteString, SB.ByteString)]
enMapBlock2 gmap = L.map(\s -> 
                            (toSBS $ intToString $ fst s, 
                             toStrictBS $ DA.encode $ Block{bblock = (L.map) (\c -> DL.pack c) $ snd s}
                            )
                        ) $ M.toList gmap -- > [(ByteString, [[ByteString]])]

--mapPair::String -> String -> [String] ->(M.Map String [Integer], M.Map Integer [String])
--mapPair _ _ [] = (M.empty, M.empty)
--mapPair ws bs cx = (map1, map2)
--    where
--       bb = geneMap2 bs ws cx 
--       ls = map(\x -> (t1 x, t2 x)) bb 
--       lt = map(\x -> (t2 x, t3 x)) bb 
--       map1 = addMore ls M.empty
--       map2 = M.fromList lt 

helpme::IO()
helpme = do
         fl
         putStrLn "Need one argument" 
         putStrLn "redis_query Aron.list"
         fl

-- snippetF = "/Users/cat/myfile/bitbucket/snippets/snippet_tmp.hs"
snippetF = "myfile/bitbucket/snippets/snippet.hs"


showInfo::[[a]] -> IO ()
showInfo cxx =  do
                fw "-"
                let ls = [testFile, jAron, jPrint, hname, cppfile]
                let ss = zip ls cxx
                mapM_ (\x -> print $ (fst x) ++ " => " ++ (show $ len $ snd x)) ss
                lfile <- getEnv "b" >>= \x -> return $ x </> "publicfile/log" </> "global.log"
                
                mapM_ (\x -> logFile2 lfile [fst x ++ " => " ++ (show $ len $ snd x) ++ "\n"]) ss
                putStrLn $ "log file => [" ++ lfile ++ "]"
                return ()


{-|
    === KEY: Generate [([String], Integer, [String])] from 'captureCppFun'

    @ 
    [(
        [ "AronLib.e"
        , "AronLib.ew"
        , "AronLib.ewl"
        , "AronLib.ewli"
        , "AronLib.ewlin"
        , "AronLib.ewline"
        , "AronLib.i"
        , "AronLib.in"
        , "AronLib.ine"
        , "AronLib.l"
        , "AronLib.li"
        , "AronLib.lin"            <-  [String]
        , "AronLib.line"
        , "AronLib.n"
        , "AronLib.ne"
        , "AronLib.new"
        , "AronLib.newl"
        , "AronLib.newli"
        , "AronLib.newlin"
        , "AronLib.newline"
        , "AronLib.w"
        , "AronLib.wl"
        , "AronLib.wli"
        , "AronLib.wlin"
        , "AronLib.wline"
        ]
    , 40007                        <- Integer
    , [ "void newline(){" ]        <- [String]
    )
    ]
    @ 

    @
    [([String], Integer, [String])]
    @
 -}
redisExtractCppAronLib::String -> [String] -> [([String], Integer, [String])]
redisExtractCppAronLib package cx = retMap 
    where
       lt = captureCppFun cx
       rMap = zipWith(\n x -> (substr $ snd x, n, [fst x])) [40000..] lt 
       retMap = map (\ls -> (map(\x -> package ++ x) $ t1 ls, t2 ls, t3 ls)) rMap
       substr s = unique $ join $ allSubstr s

{-|
     === KEY: capture cpp function

     @
        aronPath <- getEnv "cpplib" >>= \pa -> return $ pa </> "AronLib.h"
        namels <- captureCppFun aronPath
        pre namels
        pp $ "len namels=" ++ (show $ len namels)
     @

     TODO: Does work in the following cases
     @
        std::string fun(){
        }

        vector<std::string> splitStrRegex(const string& s, string rgxStr = "\\s+") {
     @
 -}
captureCppFun::[String] -> [(String, String)]
captureCppFun ls = filter (\(a, _) -> (len . trim) a > 0) $ map captureFun funls 
    where
       rexStr = "^[[:space:]]*[*<>a-zA-Z0-9_-]+[[:space:]]+([a-zA-Z0-9_-]+)[[:space:]]*[(][^()]*[)][[:space:]]*[{]{0,1}[[:space:]]*$"
       may = map (\s -> let may = matchAnyRegex (mkRegex rexStr) s
                            str = case may of 
                                   Just tup -> takeIndexBetweenInc tup s  
                                   Nothing  -> ""
                        in str) ls
       funls = filter (\x -> (len . trim) x > 0) may 

captureFun::String -> (String, String)
captureFun s =  if matchTest rex s then read tups :: (String, String) else ("", "")
    where
      rexStr = "^[[:space:]]*([*<>a-zA-Z0-9_-]|(::))+[*<>a-zA-Z0-9_-]+[[:space:]]+([a-zA-Z0-9_-]+)[[:space:]]*[(][^()]*[)][[:space:]]*[{]{0,1}[[:space:]]*$"
      rex = mkRegex rexStr
      tups = subRegex rex s "(\"\\0\",\"\\3\")"

{-|
    === KEY: Insert key value to Redis datastore
 -}
setRedisKeyValue::RedisCtx m f => [([String], Integer, [String])] -> m (f Status)
setRedisKeyValue cx = do 
   let b2 = cx 
   let hm = map(\x -> (t1 x, t2 x)) b2
   let hn = map(\x -> (t2 x, t3 x)) b2
   let hmap1 = addMore hm M.empty
   let hmap2 = M.fromList hn 
   let m1 = enMapBlock1 hmap1 -- hmap1 -> [(ByteString, [ByteString])]
   let m2 = enMapBlock2 hmap2 -- hmap2 -> [(ByteString, [[ByteString]])]
   mset m1 
   mset m2

main = do 
       -- argList <- getArgs 
       -- if len argList == 0 then helpme else do
       -- let input = head argList
       home <- getEnv "HOME"
       fblock <- readFileList $ home </> testFile 
       jAronBlock <- readFileList $ home </> jAron 
       jPrintBlock <- readFileList $ home </> jPrint 
       haskellBlock <- readFileList $ home </> hname
       snippetBlock <- readSnippet $ home </> snippetF

       cppBlock <- readFileList $ home </> cppfile 

       let bs = "^[[:space:]]*(---){1,}[[:space:]]*" -- block delimiter
       let ws = "[,.<>;()/\\ ]"                      -- ws: word delimiter 
       conn <- connect defaultConnectInfo
       runRedis conn $ do

                       -- redisExtractAronModule::[String] -> [([String], Integer, [String])]
--                       let b2 = redisExtractAronModule "AronModule." haskellBlock 
--                       let hm = map(\x -> (t1 x, t2 x)) b2
--                       let hn = map(\x -> (t2 x, t3 x)) b2

--                       let b3 = redisExtractJavaMethod "Aron." jAronBlock 
--                       let lm = map(\x -> (t1 x, t2 x)) b3
--                       let ln = map(\x -> (t2 x, t3 x)) b3

--                       let b4 = redisExtractJavaMethodWithPackage "Print." jPrintBlock
--                       let pm = map(\x -> (t1 x, t2 x)) b4
--                       let pn = map(\x -> (t2 x, t3 x)) b4

--                       let b5 = redisExtractSnippet snippetBlock
--                       liftIO $ pre b5
--                       let qm = map(\x -> (t1 x, t2 x)) b5
--                       let qn = map(\x -> (t2 x, t3 x)) b5

--                       let b6 = redisExtractCppAronLib "AronLib." cppBlock 
--                       liftIO $ pre b6
--                       let cm = map(\x -> (t1 x, t2 x)) b6
--                       let cn = map(\x -> (t2 x, t3 x)) b6

                       -- let lmm = hm ++ lm ++ pm
                       -- let lnn = hn ++ ln ++ pn
                       -- 
                       -- let map1 = addMore lmm M.empty
                       -- let map2 = M.fromList lnn 
                       -- 
                       -- -- use two maps 
                       -- -- Str -> [Integer]
                       -- -- Integer -> [String]
                       -- let m1 = enMapBlock1 map1 -- map1 -> [(ByteString, [ByteString])]
                       -- let m2 = enMapBlock2 map2 -- map2 -> [(ByteString, [[ByteString]])]
                       -- mset m1 
                       -- mset m2

                       -- Haskell
--                       let hmap1 = addMore hm M.empty
--                       let hmap2 = M.fromList hn 
--                       
--                       -- use two maps 
--                       -- Str -> [Integer]
--                       -- Integer -> [String]
--                       let m1 = enMapBlock1 hmap1 -- hmap1 -> [(ByteString, [ByteString])]
--                       let m2 = enMapBlock2 hmap2 -- hmap2 -> [(ByteString, [[ByteString]])]
--                       mset m1 
--                       mset m2

                       -- Java AronLib.*
--                       let lmap1 = addMore lm M.empty
--                       let lmap2 = M.fromList ln 
--                       
--                       -- use two maps 
--                       -- Str -> [Integer]
--                       -- Integer -> [String]
--                       let m3 = enMapBlock1 lmap1 -- map1 -> [(ByteString, [ByteString])]
--                       let m4 = enMapBlock2 lmap2 -- map2 -> [(ByteString, [[ByteString]])]
--                       mset m3 
--                       mset m4

                       -- Java Print.*
--                       let pmap1 = addMore pm M.empty
--                       let pmap2 = M.fromList pn 
                       
                       -- use two maps 
                       -- Str -> [Integer]
                       -- Integer -> [String]
--                       let m5 = enMapBlock1 pmap1 -- map1 -> [(ByteString, [ByteString])]
--                       let m6 = enMapBlock2 pmap2 -- map2 -> [(ByteString, [[ByteString]])]
--                       mset m5
--                       mset m6

                       -- not working yet for snippet.hs
                       -- /Users/cat/myfile/bitbucket/snippets/snippet_tmp.hs
--                       let qmap1 = addMore qm M.empty
--                       let qmap2 = M.fromList qn 
                       
                       -- use two maps 
                       -- Str -> [Integer]
                       -- Integer -> [String]
--                       let m7 = enMapBlock1 qmap1 -- map1 -> [(ByteString, [ByteString])]
--                       let m8 = enMapBlock2 qmap2 -- map2 -> [(ByteString, [[ByteString]])]
--                       mset m7
--                       mset m8

                       -- cpp file: $cpplib/AronLib.h
--                       let cppmap1 = addMore cm M.empty
--                       let cppmap2 = M.fromList cn 


--                       let m9 = enMapBlock1 cppmap1 -- map1 -> [(ByteString, [ByteString])]
--                       let m10 = enMapBlock2 cppmap2 -- map2 -> [(ByteString, [[ByteString]])]
--                       mset m9
--                       mset m10
--
 
                       let haskellls = redisExtractAronModule "AronModule." haskellBlock 
                       setRedisKeyValue haskellls 

                       let javals = redisExtractJavaMethod "Aron." jAronBlock 
                       setRedisKeyValue javals 

                       let printls = redisExtractJavaMethodWithPackage "Print." jPrintBlock
                       setRedisKeyValue printls 

                       let snippetls = redisExtractSnippet snippetBlock
                       setRedisKeyValue snippetls 

                       let cppls = redisExtractCppAronLib "AronLib." cppBlock 
                       setRedisKeyValue cppls 

                       -- set (strToStrictByteString "key1")  (strToStrictByteString "val1") 
                       return ()
        
       showInfo [fblock, jAronBlock, jPrintBlock, haskellBlock, cppBlock]




