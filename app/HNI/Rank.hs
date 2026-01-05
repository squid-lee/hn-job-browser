module HNI.Rank where

import qualified Data.Map.Strict as Map
import Data.Semigroup (Max (..))
import HNI.Features

-- Take max score per feature category, then sum categories
rank :: [Feature] -> Double
rank = sum . map getMax . Map.elems . foldMap toMaxMap
  where
    toMaxMap feat = Map.singleton (featureCategory feat) (Max $ rankOne feat)

data FeatureCategory
  = LocationCat
  | SalaryCat
  | RemotenessCat
  | TechCat
  | RoleCat
  | URLCat
  | EmailCat
  | PurposeCat
  deriving (Eq, Ord)

featureCategory :: Feature -> FeatureCategory
featureCategory (Location _ _) = LocationCat
featureCategory (Salary _ _) = SalaryCat
featureCategory (Remoteness _ _) = RemotenessCat
featureCategory (Tech _ _) = TechCat
featureCategory (Role _ _) = RoleCat
featureCategory (URL _ _) = URLCat
featureCategory (Email _ _) = EmailCat
featureCategory (Purpose _ _) = PurposeCat

-- Linear ranking
rankOne :: Feature -> Double
rankOne (Location country _) = rankLocation country
rankOne (Salary salaryInfo _) = rankSalary salaryInfo
rankOne (Remoteness status _) = rankRemoteness status
rankOne (Tech techStack _) = rankTech techStack
rankOne (Role roleType _) = rankRole roleType
rankOne (URL _ _) = 0
rankOne (Email _ _) = 10
rankOne (Purpose purposeType _) = rankPurpose purposeType

rankSalary :: SalaryInfo -> Double
rankSalary (SalaryInfo _ Nothing) = -50  -- Equity only is not good
rankSalary (SalaryInfo _ (Just gbpAmount)) =
  let target = 110000
  in (gbpAmount - target) / 1000

rankLocation :: Country -> Double
rankLocation London = 30
rankLocation UK = 50
rankLocation Bristol = 30
rankLocation Europe = 20
rankLocation Netherlands = 20
rankLocation Amsterdam = 20
rankLocation Germany = 20
rankLocation Berlin = 20
rankLocation Austria = 20
rankLocation Vienna = 20
rankLocation Lisbon = 10
rankLocation Portugal = 10
rankLocation Stockholm = 10
rankLocation Sweden = 10
rankLocation Ghent = 10
rankLocation Belgium = 10
rankLocation Paris = 10
rankLocation France = 10
rankLocation Spain = 10
rankLocation Barcelona = 10
rankLocation Bilbao = 10
rankLocation US = -500
rankLocation Canada = -500
rankLocation India = -500
rankLocation LatinAmerica = -500
rankLocation NYC = -500
rankLocation SF = -500
rankLocation BayArea = -500
rankLocation SanFrancisco = -500
rankLocation LosAngeles = -500
rankLocation SouthKorea = -500
rankLocation Seoul = -500
rankLocation Japan = -500
rankLocation Tokyo = -500
rankLocation Toronto = -500
rankLocation Global = 50
rankLocation (OtherCountry _) = -500

rankRemoteness :: RemoteStatus -> Double
rankRemoteness FullyRemote = 100
rankRemoteness (RemoteWithRestriction _) = -200
rankRemoteness Hybrid = -20
rankRemoteness OnSite = -100
rankRemoteness InPerson = -100

rankRole :: RoleType -> Double
rankRole StaffSoftwareEngineer = 50
rankRole SeniorSoftwareEngineer = 50
rankRole PrincipalEngineer = 60
rankRole LeadEngineer = 40
rankRole (OtherRole _) = 0
rankRole _ = 10

rankTech :: TechStack -> Double
rankTech Python = -10
rankTech React = -10
rankTech JavaScript = -20
rankTech PHP = -50
rankTech TypeScript = 0
rankTech Rust = 10
rankTech Haskell = 20
rankTech CommonLisp = 20
rankTech Prolog = 20
rankTech DistributedSystems = 10
rankTech CPlusPlus = -10
rankTech Laravel = -100
rankTech NodeJS = -15
rankTech Ruby = -10
rankTech Rails = -10
rankTech WebAssembly = 10
rankTech WebGL = 10
rankTech ApacheArrow = 0
rankTech Parquet = 0
rankTech DuckDB = 10
rankTech Kubernetes = 0
rankTech GraphQL = -10
rankTech Postgres = 10
rankTech (OtherTech _) = 0

rankPurpose :: PurposeType -> Double
rankPurpose Blockchain = -1000
rankPurpose Web3 = -1000
rankPurpose NFT = -1000
rankPurpose Therapy = -100
rankPurpose SportsBetting = -100
rankPurpose Finance = 10
rankPurpose Energy = 30
rankPurpose Gifting = -10
rankPurpose PropTech = -20
rankPurpose (OtherPurpose _) = 0
