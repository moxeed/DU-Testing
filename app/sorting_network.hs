applyGate _ _ _ [] = []
applyGate _ _ [] _ = []
applyGate input gate (i:ix) (g:gx)
    | isWire    = i:rest
    | otherwise = gateOut:rest
    where 
        isWire  = g == "-"
        isEnd   = all (/=g) gx
        [start, end] = [fst x | x <- zip input gate, snd x == g]
        gateOut = if isEnd then max start end else min start end
        rest    = applyGate input gate ix gx

applyGates []     input = input
applyGates (g:gx) input = applyGates gx $ applyGate input g input g

main = do 
    line <- getLine
    let input = [read x :: Integer | x <- words line]
    contents <- getContents
    let gates = [words x | x <- lines contents]

    print $ applyGates gates input