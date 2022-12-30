module Main where
import Language.Haskell.Exts
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc
import Data.Typeable
import Data.Traversable

parsedData :: IO (ParseResult (Module SrcSpanInfo))
parsedData = parseFile "./app/sorting_network.hs"

main :: IO ()
main = do
    parseResult <- parsedData
    let moduleTree = fromParseResult parseResult
    let b = traverse (const Nothing) [1,2,3,4]
    print b
