{-# LANGUAGE LambdaCase #-}

module HNI.PrettyPrint where

import Data.Text (Text)
import qualified Data.Text as T
import HNI.Features

ppFeature :: Feature -> Text
ppFeature =
  pad
    . fmap T.strip
    . \case
      Location country _ -> ("Location", ppCountry country)
      Salary salaryInfo _ -> ("Salary", ppSalary salaryInfo)
      Remoteness status _ -> ("Remoteness", ppRemoteStatus status)
      Tech techStack _ -> ("Tech", ppTech techStack)
      Role roleType _ -> ("Role", ppRole roleType)
      URL x _ -> ("URL", x)
      Email x _ -> ("Email", x)
      Purpose purposeType _ -> ("Purpose", ppPurpose purposeType)
  where
    ctors = ["Location", "Salary", "Remoteness", "Tech", "Role", "URL", "Email", "Purpose"]
    len = maximum $ map T.length ctors
    pad (ctor, val) = T.justifyLeft len ' ' ctor <> " " <> val

ppSalary :: SalaryInfo -> Text
ppSalary (SalaryInfo txt (Just amt)) =
  txt <> " (~£" <> T.pack (show (round (amt / 1000) :: Int)) <> "k)"
ppSalary (SalaryInfo txt Nothing) = txt

ppCountry :: Country -> Text
ppCountry = \case
  London -> "London"
  UK -> "UK"
  Europe -> "Europe"
  US -> "US"
  Canada -> "Canada"
  India -> "India"
  LatinAmerica -> "Latin America"
  Netherlands -> "Netherlands"
  Amsterdam -> "Amsterdam"
  Germany -> "Germany"
  Berlin -> "Berlin"
  Austria -> "Austria"
  Vienna -> "Vienna"
  Bristol -> "Bristol"
  NYC -> "NYC"
  SF -> "SF"
  BayArea -> "Bay Area"
  Global -> "Global"
  Lisbon -> "Lisbon"
  Portugal -> "Portugal"
  SanFrancisco -> "San Francisco"
  LosAngeles -> "Los Angeles"
  Stockholm -> "Stockholm"
  Sweden -> "Sweden"
  Ghent -> "Ghent"
  Belgium -> "Belgium"
  Paris -> "Paris"
  France -> "France"
  SouthKorea -> "South Korea"
  Seoul -> "Seoul"
  Japan -> "Japan"
  Tokyo -> "Tokyo"
  Toronto -> "Toronto"
  Spain -> "Spain"
  Barcelona -> "Barcelona"
  Bilbao -> "Bilbao"
  OtherCountry t -> t

ppRemoteStatus :: RemoteStatus -> Text
ppRemoteStatus = \case
  FullyRemote -> "Remote"
  RemoteWithRestriction t -> T.toTitle t
  Hybrid -> "Hybrid"
  OnSite -> "Onsite"
  InPerson -> "In-Person"

ppRole :: RoleType -> Text
ppRole = \case
  SeniorSoftwareEngineer -> "Senior Software Engineer"
  StaffSoftwareEngineer -> "Staff Software Engineer"
  PrincipalEngineer -> "Principal Engineer"
  LeadEngineer -> "Lead Engineer"
  JuniorEngineer -> "Junior Engineer"
  FoundingEngineer -> "Founding Engineer"
  SoftwareEngineer -> "Software Engineer"
  FrontendEngineer -> "Frontend Engineer"
  BackendEngineer -> "Backend Engineer"
  FullstackEngineer -> "Fullstack Engineer"
  PlatformEngineer -> "Platform Engineer"
  DataEngineer -> "Data Engineer"
  MLEngineer -> "ML Engineer"
  AIEngineer -> "AI Engineer"
  DevOpsEngineer -> "DevOps Engineer"
  SREEngineer -> "SRE Engineer"
  InfrastructureEngineer -> "Infrastructure Engineer"
  SystemsEngineer -> "Systems Engineer"
  EngineeringManager -> "Engineering Manager"
  ProductManager -> "Product Manager"
  TechnicalManager -> "Technical Manager"
  DataScientist -> "Data Scientist"
  MLScientist -> "ML Scientist"
  AIScientist -> "AI Scientist"
  CTO -> "CTO"
  VPEngineering -> "VP of Engineering"
  HeadOfEngineering -> "Head of Engineering"
  TechLead -> "Tech Lead"
  TeamLead -> "Team Lead"
  SWE -> "SWE"
  OtherRole t -> T.toTitle t

ppTech :: TechStack -> Text
ppTech = \case
  Python -> "Python"
  React -> "React"
  JavaScript -> "JavaScript"
  PHP -> "PHP"
  TypeScript -> "TypeScript"
  Rust -> "Rust"
  Haskell -> "Haskell"
  CommonLisp -> "Common Lisp"
  Prolog -> "Prolog"
  DistributedSystems -> "Distributed Systems"
  CPlusPlus -> "C++"
  Laravel -> "Laravel"
  NodeJS -> "Node.js"
  Ruby -> "Ruby"
  Rails -> "Rails"
  WebAssembly -> "WebAssembly"
  WebGL -> "WebGL"
  ApacheArrow -> "Apache Arrow"
  Parquet -> "Parquet"
  DuckDB -> "DuckDB"
  Kubernetes -> "Kubernetes"
  GraphQL -> "GraphQL"
  Postgres -> "Postgres"
  OtherTech t -> t

ppPurpose :: PurposeType -> Text
ppPurpose = \case
  Blockchain -> "Blockchain"
  Web3 -> "Web3"
  NFT -> "NFT"
  Therapy -> "Therapy"
  SportsBetting -> "Sports Betting"
  Finance -> "Finance"
  Energy -> "Energy"
  Gifting -> "Gifting"
  PropTech -> "PropTech"
  OtherPurpose t -> t
