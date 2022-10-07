apply [] [] _ value = (value, [])
apply (i:ix) (g:gx) "-" value 
    | g /= "-" = (value, min i nextValue:result)
    where 
        (_, result)       = apply rest gx "-" value
        (nextValue, rest) = apply ix   gx g i
        
apply (i:ix) (g:gx) key value
    | g == "-" = (nextValue, i:rest)
    | g /= key = (nextValue, i:rest)
    | g == key = (i, max value i:ix)
    where (nextValue, rest) = apply ix gx key value

applyGates []     input = input
applyGates (g:gx) input = applyGates gx result
    where (_, result) = apply input g "-" 0

main = do 
    line <- getLine
    let input = [read x :: Integer | x <- words line]
    contents <- getContents
    let gates = [words x | x <- lines contents]

    print $ applyGates gates input