import Prelude hiding (lookup, (>>=))
import Data.list hiding (lookup)

--true unbiased fair beautiful pretty beguiling artful artificial sham false

--type String = [Char]

thesaurus :: [(String, [String])]
thesaurus = [("true",["accurate", "unbiased", "authentic", "factual", "genuine"]),
             ("unbiased",["candid","evenhanded", "fair", "impartial", "just"]),
             ("fair", ["beautiful", "comely", "pretty"])
             ("beautiful", ["gorgeous","pretty","lovely", "ravishing"])
             ("pretty", ["beguiling","beautiful", "comely", "fair", "lovely"]),
             ("beguiling", ["artful","cute","devious","deceitful"]),
             ("artful",["artificial","crafty","cunning","guileful","subtle"]),
             ("artificial", ["cunning","guileful","sham","synthetic"]),
             ("sham", ["bogus","ersatz","false","synthetic"])]


lookup :: Eq a => [(a,[b])] -> a ->[b]
lookup [] word = []
lookup ((word' , synonyms): rest) word = if word == word' then synonyms else lookup rest word