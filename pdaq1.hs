

type PDA = (Int, [Int], [Transition])
type Transition = ((Int, String, String), (Int, String))
data Result = Accept | Reject deriving (Show)
type Configuration = (Int, String , String)
 
 
 
run :: PDA -> String -> Result
run (begin, ending, transition) "" = Accept
run(begin, ending, transition) str = runs (begin, ending, transition) [(begin, str, "")]
 
runs :: PDA -> [Configuration] -> Result
runs (begin, ending, transition) [] = Reject
runs (begin, ending, transition) (head:tail)
    | complete head ending = Accept
    | otherwise = runs (begin, ending, transition) (tail ++ (steps head transition []))
 
 
step :: Configuration -> Transition -> [Configuration]
step (a,b,"") ((d,"",""),(g,""))
   | a == d = [(g,b,"")]
   | otherwise = []
 
step (a, b, "") ((d, "", ""),(g, [h]))
   | a == d = [(g, b, [h])]
   | otherwise = []
 
step (a, (b:bs), "") ((d, [e], ""), (g, ""))
   | a == d && b == e = [(g, bs, "")]
   | otherwise = []
 
step (a,(b:bs),"") ((d,[e],""),(g,[h]))
   | a == d && b == e = [(g, bs, [h])]
   | otherwise = []
 
step (a,b,c) ((d,"",""),(g,""))
   | a == d = [(g, b, c)]
   | otherwise = []

 
step (a,(b:bs),(c:cs)) ((d,[e], [f]), (g, [h]))
   | a == d && b == e && c == f = [(g, bs, (h:cs))]
   | otherwise = []
 
step (a,b,c) ((d,"",""),(g,[h]))
   | a == d = [(g, b, (h:c))]
   | otherwise = []
 
step (a,b,(c:cs)) ((d,"",[f]),(g,""))
   | a == d && c == f = [(g, b, cs)]
   | otherwise = []
 
step (a,b,(c:cs)) ((d,"",[f]),(g,[h]))
   | a == d && c == f = [(g, b, (h:cs))]
   | otherwise = []
 
step (a,(b:bs),c) ((d,[e],""),(g,""))
   | a == d && b == e = [(g, bs, c)]
   | otherwise = []
 
step (a,(b:bs),c) ((d,[e],""),(g,[h]))
   | a == d && b == e = [(g, bs, h:c)]
   | otherwise = []
 
step (a,(b:bs),(c:cs)) ((d,[e],[f]),(g,""))
   | a == d && b == e && c == f = [(g, bs, cs)]
   | otherwise = []
 

step (a,b,"") ((d,[e],[f]),(g,"")) = []
step (a,b,"") ((d,"",[f]),(g,"")) = []
step (a,b,"") ((d,[e],[f]),(g,[h])) = []
step (a,b,"") ((d,"",[f]),(g,[h])) = []

steps :: Configuration -> [Transition] -> [Configuration] -> [Configuration]
steps config [] configs = configs
steps (a, "", c) trans configs = configs
steps (a, b, c) (t:tr) configs = steps (a, b, c) tr (configs ++ (step(a,b,c) t))

complete :: Configuration -> [Int] -> Bool
complete (a,b,c) [] = False
complete (a,b,c) (x:xs)
   | a == x && b == "" && c == "" = True
   | otherwise = complete (a,b,c) xs