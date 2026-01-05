{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Evaluate" #-}

module HNI.Features where

import Brick.Span hiding (getSpan)
import Data.Array as A
import Data.List
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import HNI.Decoded
import HNI.Post
import Text.Regex.TDFA

data Country
  = London
  | UK
  | Europe
  | US
  | Canada
  | India
  | LatinAmerica
  | Netherlands
  | Amsterdam
  | Germany
  | Berlin
  | Austria
  | Vienna
  | Bristol
  | NYC
  | SF
  | BayArea
  | Global
  | Lisbon
  | Portugal
  | SanFrancisco
  | LosAngeles
  | Stockholm
  | Sweden
  | Ghent
  | Belgium
  | Paris
  | France
  | SouthKorea
  | Seoul
  | Japan
  | Tokyo
  | Toronto
  | Spain
  | Barcelona
  | Bilbao
  | OtherCountry Text
  deriving (Generic, Eq, Ord, Show)

data RemoteStatus
  = FullyRemote
  | RemoteWithRestriction Text
  | Hybrid
  | OnSite
  | InPerson
  deriving (Generic, Eq, Ord, Show)

data RoleType
  = SeniorSoftwareEngineer
  | StaffSoftwareEngineer
  | PrincipalEngineer
  | LeadEngineer
  | JuniorEngineer
  | FoundingEngineer
  | SoftwareEngineer
  | FrontendEngineer
  | BackendEngineer
  | FullstackEngineer
  | PlatformEngineer
  | DataEngineer
  | MLEngineer
  | AIEngineer
  | DevOpsEngineer
  | SREEngineer
  | InfrastructureEngineer
  | SystemsEngineer
  | EngineeringManager
  | ProductManager
  | TechnicalManager
  | DataScientist
  | MLScientist
  | AIScientist
  | CTO
  | VPEngineering
  | HeadOfEngineering
  | TechLead
  | TeamLead
  | SWE
  | OtherRole Text
  deriving (Generic, Eq, Ord, Show)

data PurposeType
  = Blockchain
  | Web3
  | NFT
  | Therapy
  | SportsBetting
  | Finance
  | Energy
  | Gifting
  | PropTech
  | OtherPurpose Text
  deriving (Generic, Eq, Ord, Show)

data TechStack
  = Python
  | React
  | JavaScript
  | PHP
  | TypeScript
  | Rust
  | Haskell
  | CommonLisp
  | Prolog
  | DistributedSystems
  | CPlusPlus
  | Laravel
  | NodeJS
  | Ruby
  | Rails
  | WebAssembly
  | WebGL
  | ApacheArrow
  | Parquet
  | DuckDB
  | Kubernetes
  | GraphQL
  | Postgres
  | OtherTech Text
  deriving (Generic, Eq, Ord, Show)

data SalaryInfo = SalaryInfo
  { salaryText :: Text,
    salaryAmount :: Maybe Double -- in GBP equivalent
  }
  deriving (Generic, Eq, Ord, Show)

data Feature
  = Location Country Span
  | Salary SalaryInfo Span
  | Remoteness RemoteStatus Span
  | Tech TechStack Span
  | Role RoleType Span
  | URL Text Span
  | Email Text Span
  | Purpose PurposeType Span
  deriving (Generic, Eq, Ord, Show)

parseCountry :: Text -> Country
parseCountry t = case T.toLower t of
  "london" -> London
  "uk" -> UK
  "europe" -> Europe
  "us" -> US
  "usa" -> US
  "united states" -> US
  "canada" -> Canada
  "india" -> India
  "latin america" -> LatinAmerica
  "latam" -> LatinAmerica
  "netherlands" -> Netherlands
  "amsterdam" -> Amsterdam
  "germany" -> Germany
  "berlin" -> Berlin
  "austria" -> Austria
  "vienna" -> Vienna
  "bristol" -> Bristol
  "nyc" -> NYC
  "sf" -> SF
  "bay area" -> BayArea
  "bayarea" -> BayArea
  "global" -> Global
  "worldwide" -> Global
  "lisbon" -> Lisbon
  "portugal" -> Portugal
  "san francisco" -> SanFrancisco
  "los angeles" -> LosAngeles
  "stockholm" -> Stockholm
  "sweden" -> Sweden
  "ghent" -> Ghent
  "belgium" -> Belgium
  "paris" -> Paris
  "france" -> France
  "south korea" -> SouthKorea
  "korea" -> SouthKorea
  "seoul" -> Seoul
  "japan" -> Japan
  "tokyo" -> Tokyo
  "toronto" -> Toronto
  "spain" -> Spain
  "barcelona" -> Barcelona
  "bilbao" -> Bilbao
  other -> OtherCountry other

parseRemoteStatus :: Text -> RemoteStatus
parseRemoteStatus t
  | T.toLower t == "remote" = FullyRemote
  | "remote" `T.isInfixOf` T.toLower t = FullyRemote -- RemoteWithRestriction t
  | T.toLower t == "hybrid" = Hybrid
  | T.toLower t `elem` ["onsite", "on-site"] = OnSite
  | T.toLower t `elem` ["in-person", "inperson"] = InPerson
  | otherwise = RemoteWithRestriction t

parseRole :: Text -> RoleType
parseRole t = case normalized of
  "senior software engineer" -> SeniorSoftwareEngineer
  "staff software engineer" -> StaffSoftwareEngineer
  "principal engineer" -> PrincipalEngineer
  "principal software engineer" -> PrincipalEngineer
  "lead engineer" -> LeadEngineer
  "lead software engineer" -> LeadEngineer
  "junior engineer" -> JuniorEngineer
  "junior software engineer" -> JuniorEngineer
  "founding engineer" -> FoundingEngineer
  "software engineer" -> SoftwareEngineer
  "frontend engineer" -> FrontendEngineer
  "frontend developer" -> FrontendEngineer
  "backend engineer" -> BackendEngineer
  "backend developer" -> BackendEngineer
  "fullstack engineer" -> FullstackEngineer
  "full-stack engineer" -> FullstackEngineer
  "fullstack developer" -> FullstackEngineer
  "full-stack developer" -> FullstackEngineer
  "platform engineer" -> PlatformEngineer
  "data engineer" -> DataEngineer
  "ml engineer" -> MLEngineer
  "ai engineer" -> AIEngineer
  "devops engineer" -> DevOpsEngineer
  "sre engineer" -> SREEngineer
  "sre" -> SREEngineer
  "infrastructure engineer" -> InfrastructureEngineer
  "systems engineer" -> SystemsEngineer
  "engineering manager" -> EngineeringManager
  "product manager" -> ProductManager
  "technical manager" -> TechnicalManager
  "tech manager" -> TechnicalManager
  "data scientist" -> DataScientist
  "ml scientist" -> MLScientist
  "machine learning scientist" -> MLScientist
  "ai scientist" -> AIScientist
  "cto" -> CTO
  "vp of engineering" -> VPEngineering
  "head of engineering" -> HeadOfEngineering
  "tech lead" -> TechLead
  "team lead" -> TeamLead
  "swe" -> SWE
  _ -> OtherRole t
  where
    normalized = T.toLower $ T.replace "-" " " t

parsePurpose :: Text -> PurposeType
parsePurpose t = case T.toLower t of
  "blockchain" -> Blockchain
  "web3" -> Web3
  "web3.0" -> Web3
  "nft" -> NFT
  "therapy" -> Therapy
  "sports betting" -> SportsBetting
  "finance" -> Finance
  "fintech" -> Finance
  "energy" -> Energy
  "gifting" -> Gifting
  "proptech" -> PropTech
  "openrent" -> PropTech
  "rental" -> PropTech
  "rentals" -> PropTech
  "renting" -> PropTech
  "rented" -> PropTech
  other -> OtherPurpose other

parseTech :: Text -> TechStack
parseTech t = case T.toLower t of
  "python" -> Python
  "react" -> React
  "javascript" -> JavaScript
  "js" -> JavaScript
  "php" -> PHP
  "typescript" -> TypeScript
  "ts" -> TypeScript
  "rust" -> Rust
  "haskell" -> Haskell
  "common lisp" -> CommonLisp
  "commonlisp" -> CommonLisp
  "lisp" -> CommonLisp
  "prolog" -> Prolog
  "distributed systems" -> DistributedSystems
  "c++" -> CPlusPlus
  "cpp" -> CPlusPlus
  "laravel" -> Laravel
  "node.js" -> NodeJS
  "nodejs" -> NodeJS
  "node" -> NodeJS
  "ruby" -> Ruby
  "rails" -> Rails
  "ruby on rails" -> Rails
  "webassembly" -> WebAssembly
  "wasm" -> WebAssembly
  "webgl" -> WebGL
  "apache arrow" -> ApacheArrow
  "arrow" -> ApacheArrow
  "parquet" -> Parquet
  "duckdb" -> DuckDB
  "kubernetes" -> Kubernetes
  "k8s" -> Kubernetes
  "graphql" -> GraphQL
  "postgres" -> Postgres
  "postgresql" -> Postgres
  other -> OtherTech other

parseSalary :: Text -> SalaryInfo
parseSalary txt = SalaryInfo txt (extractMaxSalary txt)
  where
    extractMaxSalary :: Text -> Maybe Double
    extractMaxSalary t
      | T.toLower t == "equity" = Nothing
      | otherwise = do
          let nums = extractNumbers t
              currency = detectCurrency t
              toGBP = currencyToGBP currency
          case nums of
            [] -> Nothing
            _ -> Just $ maximum nums * toGBP

    extractNumbers :: Text -> [Double]
    extractNumbers t =
      let cleaned = T.replace "," "" t
          words' = T.words cleaned
          parseNum w = case T.unpack w of
            s
              | all (\c -> c `elem` ("0123456789k+$£€¥" :: String)) s ->
                  let stripped = dropWhile (\c -> c `elem` ("$£€¥" :: String)) s
                      numPart = takeWhile (\c -> c `elem` ("0123456789" :: String)) stripped
                      hasK = 'k' `elem` s
                   in case reads numPart of
                        [(n, "")] -> Just (if hasK then n * 1000 else n)
                        _ -> Nothing
            _ -> Nothing
       in mapMaybe parseNum words'

    detectCurrency :: Text -> Text
    detectCurrency t
      | "$" `T.isInfixOf` t || "USD" `T.isInfixOf` T.toUpper t = "USD"
      | "€" `T.isInfixOf` t || "EUR" `T.isInfixOf` T.toUpper t = "EUR"
      | "£" `T.isInfixOf` t || "GBP" `T.isInfixOf` T.toUpper t = "GBP"
      | "SEK" `T.isInfixOf` T.toUpper t = "SEK"
      | "¥" `T.isInfixOf` t || "JPY" `T.isInfixOf` T.toUpper t = "JPY"
      | otherwise = "GBP" -- default assumption
    currencyToGBP :: Text -> Double
    currencyToGBP "USD" = 1 / 1.27
    currencyToGBP "EUR" = 1 / 1.17
    currencyToGBP "GBP" = 1.0
    currencyToGBP "SEK" = 1 / 13.5
    currencyToGBP "JPY" = 1 / 193
    currencyToGBP _ = 1.0

features :: Post Decoded -> [Feature]
features p =
  removeSubsumed
    . concatMap (\(mkTag, regexp) -> map (uncurry mkTag . toSpan) $ matchAllText regexp (payload (text p)))
    . concat
    $ [ location,
        remoteness,
        salary,
        role,
        tech,
        url,
        email,
        purpose
      ]
  where
    toSpan :: MatchText Text -> (Text, Span)
    toSpan m = (match, Span offset length)
      where
        (match, (offset, length)) = m A.! 0

    i :: String -> Regex
    i = makeRegexOpts (defaultCompOpt {caseSensitive = False}) defaultExecOpt

    r :: String -> Regex
    r = makeRegexOpts (defaultCompOpt {caseSensitive = True}) defaultExecOpt

    curr = "[£$€]|USD|EUR|GBP"

    -- Range: $150k - $200k, 100-150k, $200,000 - $245,000, 150-230k+
    -- Open-ended: 200k+, $200k+
    -- Single: £80k 80k€ (require k suffix to avoid matching $50mil funding)
    salary =
      [ (Salary . parseSalary, i $ "(" <> curr <> ")[[:digit:],]+k[ ]*[-–—][ ]*(" <> curr <> ")?[[:digit:],]+k?\\+?"),
        (Salary . parseSalary, i "[[:digit:]]{2,3}k?[ ]*[-–—][ ]*[[:digit:]]{2,3}k\\+?"),
        (Salary . parseSalary, i $ "(" <> curr <> ")?[[:digit:]]{2,3}k\\+"),
        (Salary . parseSalary, i $ "(" <> curr <> ")[[:digit:],]+k"),
        (Salary . parseSalary, i "[[:digit:]]{2,}[k€£$]+"),
        (Salary . parseSalary, i "equity")
      ]

    url = [(URL, i "https?://[[:alnum:]]*\\.[[:alnum:].-]*[[:alnum:]]+(/[^ ,]*)?")]
    email = [(Email, i "[^@ ]+@[^@ .]+\\.[^@ ]+[^ @.,]")]

    location =
      [ (Location . parseCountry, knownPlacesI),
        (Location . parseCountry, knownPlacesR),
        (Location . currencyToCountry, i "\\$[[:digit:]]"),
        (Location . currencyToCountry, i "USD"),
        (Location . currencyToCountry, i "€[[:digit:]]"),
        (Location . currencyToCountry, i "EUR"),
        (Location . currencyToCountry, i "£[[:digit:]]"),
        (Location . currencyToCountry, i "GBP"),
        (Location . currencyToCountry, i "\\bSEK\\b"),
        (Location . currencyToCountry, i "¥[[:digit:]]"),
        (Location . currencyToCountry, i "JPY")
      ]

    currencyToCountry :: Text -> Country
    currencyToCountry t = case T.toLower $ T.take 3 t of
      txt
        | "$" `T.isPrefixOf` txt || "usd" `T.isPrefixOf` txt -> US
        | "€" `T.isPrefixOf` txt || "eur" `T.isPrefixOf` txt -> Europe
        | "£" `T.isPrefixOf` txt || "gbp" `T.isPrefixOf` txt -> UK
        | "sek" `T.isPrefixOf` txt -> Sweden
        | "¥" `T.isPrefixOf` txt || "jpy" `T.isPrefixOf` txt -> Japan
        | otherwise -> OtherCountry t

    remoteness =
      [ (Remoteness . parseRemoteStatus, i "remote"),
        (Remoteness . parseRemoteStatus, i "hybrid|on-?site|in-?person")
      ]

    knownPlacesI :: Regex
    knownPlacesI =
      i
        $ intercalate "|"
          . map (\s -> "\\b" <> s <> "\\b")
        $ ["amsterdam", "netherlands", "berlin", "germany", "vienna", "austria", "bristol", "london", "uk", "canada", "toronto", "india", "nyc", "sf", "bay.?area", "global", "worldwide", "united states", "usa", "canada", "latin america", "latam", "europe", "lisbon", "portugal", "san francisco", "los angeles", "stockholm", "sweden", "ghent", "belgium", "paris", "france", "spain", "barcelona", "bilbao", "south korea", "korea", "seoul", "japan", "tokyo"]

    knownPlacesR :: Regex
    knownPlacesR =
      r
        $ intercalate "|"
          . map (\s -> "\\b" <> s <> "\\b")
        $ "US" : ["AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"]

    role =
      [ (Role . parseRole, i "(senior|staff|principal|lead|junior|founding)[ -]*(software|frontend|backend|fullstack|full-stack|platform|data|ml|ai|devops|sre|infrastructure|systems?)[ -]*(engineer|developer)?"),
        (Role . parseRole, i "(software|frontend|backend|fullstack|full-stack|platform|data|ml|ai|devops|sre|infrastructure|systems?)[ -]*(engineer|developer)"),
        (Role . parseRole, i "(engineering|product|technical|tech)[ -]*manager"),
        (Role . parseRole, i "(data|ml|machine learning|ai)[ -]*(scientist|researcher)"),
        (Role . parseRole, i "\\b(cto|vp of engineering|head of engineering|tech lead|team lead)\\b"),
        (Role . parseRole, i "\\bswe\\b")
      ]

    tech =
      [ (Tech . parseTech, i "\\bpython\\b"),
        (Tech . parseTech, i "\\breact\\b"),
        (Tech . parseTech, i "\\bjavascript\\b"),
        (Tech . parseTech, i "\\bnode\\.?js\\b"),
        (Tech . parseTech, i "\\bnode\\b"),
        (Tech . parseTech, i "\\bjs\\b"),
        (Tech . parseTech, i "\\bphp\\b"),
        (Tech . parseTech, i "\\btypescript\\b"),
        (Tech . parseTech, i "\\brust\\b"),
        (Tech . parseTech, i "\\bhaskell\\b"),
        (Tech . parseTech, i "\\b(common *)?lisp\\b"),
        (Tech . parseTech, i "\\bprolog\\b"),
        (Tech . parseTech, i "\\bdistributed systems\\b"),
        (Tech . parseTech, i "\\bc\\+\\+"),
        (Tech . parseTech, i "\\bcpp\\b"),
        (Tech . parseTech, i "\\blaravel\\b"),
        (Tech . parseTech, i "\\bruby( on rails)?\\b"),
        (Tech . parseTech, i "\\brails\\b"),
        (Tech . parseTech, i "\\bwebassembly\\b"),
        (Tech . parseTech, i "\\bwasm\\b"),
        (Tech . parseTech, i "\\bwebgl\\b"),
        (Tech . parseTech, i "\\bapache arrow\\b"),
        (Tech . parseTech, i "\\barrow\\b"),
        (Tech . parseTech, i "\\bparquet\\b"),
        (Tech . parseTech, i "\\bduckdb\\b"),
        (Tech . parseTech, i "\\bkubernetes\\b"),
        (Tech . parseTech, i "\\bk8s\\b"),
        (Tech . parseTech, i "\\bgraphql\\b"),
        (Tech . parseTech, i "\\bpostgres(ql)?\\b")
      ]

    purpose = [(Purpose . parsePurpose, i "blockchain"), (Purpose . parsePurpose, i "web3(.0)?"), (Purpose . parsePurpose, i "nft"), (Purpose . parsePurpose, i "therapy"), (Purpose . parsePurpose, i "sports betting"), (Purpose . parsePurpose, i "\\bfinance\\b"), (Purpose . parsePurpose, i "\\bfintech\\b"), (Purpose . parsePurpose, i "\\benergy\\b"), (Purpose . parsePurpose, i "\\bgifting\\b"), (Purpose . parsePurpose, i "\\bproptech\\b"), (Purpose . parsePurpose, i "\\bopenrent\\b"), (Purpose . parsePurpose, i "\\brental?s?\\b"), (Purpose . parsePurpose, i "\\brenting\\b"), (Purpose . parsePurpose, i "\\brented\\b")]

removeSubsumed :: [Feature] -> [Feature]
removeSubsumed feats = filter (not . subsumedByAny) feats
  where
    spans = map getSpan feats
    subsumedByAny feat = any (subsumes (getSpan feat)) spans
    subsumes (Span o1 l1) (Span o2 l2) =
      o2 <= o1 && o1 + l1 <= o2 + l2 && (o1, l1) /= (o2, l2)

getSpan :: Feature -> Span
getSpan = \case
  Location _ s -> s
  Salary _ s -> s
  Remoteness _ s -> s
  Tech _ s -> s
  Role _ s -> s
  URL _ s -> s
  Email _ s -> s
  Purpose _ s -> s
