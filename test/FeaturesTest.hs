module FeaturesTest (tests) where

import qualified Data.Text as T
import HNI.Decoded (Decoded (..))
import HNI.Features
import HNI.Post (Post (..))
import Test.HUnit

-- Helper to create a test post
mkPost :: [T.Text] -> Post Decoded
mkPost textLines =
  Post
    { postId = 1,
      author = "test",
      createdAt = "2024-01-01",
      createdAtI = 1704067200,
      text = Decoded (T.unlines textLines),
      children = [],
      parentId = Just 1,
      options = [],
      points = Nothing,
      storyId = 1,
      title = Nothing,
      typ = "comment",
      url = Nothing
    }

-- Helper to check if a feature of a certain type exists
hasFeature :: (Feature -> Bool) -> [Feature] -> Bool
hasFeature predicate = any predicate

isRemoteness :: Feature -> Bool
isRemoteness (Remoteness _ _) = True
isRemoteness _ = False

-- Get all features of a type
getRoles :: [Feature] -> [RoleType]
getRoles fs = [r | Role r _ <- fs]

getSalaries :: [Feature] -> [SalaryInfo]
getSalaries fs = [s | Salary s _ <- fs]

getLocations :: [Feature] -> [Country]
getLocations fs = [c | Location c _ <- fs]

getTechs :: [Feature] -> [TechStack]
getTechs fs = [t | Tech t _ <- fs]

-- Test cases
testJuniorSoftwareEngineer :: Test
testJuniorSoftwareEngineer =
  TestCase $ do
    let post = mkPost ["Junior Software Engineer - Remote (Global), Full-Time"]
        feats = features post
        roles = getRoles feats
    assertBool "Should extract Junior Engineer role" (JuniorEngineer `elem` roles)

testSeniorSoftwareEngineer :: Test
testSeniorSoftwareEngineer =
  TestCase $ do
    let post = mkPost ["Senior Software Engineer needed for our team"]
        feats = features post
        roles = getRoles feats
    assertBool "Should extract Senior Software Engineer role" (SeniorSoftwareEngineer `elem` roles)

testStaffEngineer :: Test
testStaffEngineer =
  TestCase $ do
    let post = mkPost ["We're hiring a Staff Software Engineer"]
        feats = features post
        roles = getRoles feats
    assertBool "Should extract Staff Software Engineer role" (StaffSoftwareEngineer `elem` roles)

testSoftwareEngineer :: Test
testSoftwareEngineer =
  TestCase $ do
    let post = mkPost ["Software Engineer position available"]
        feats = features post
        roles = getRoles feats
    assertBool "Should extract Software Engineer role" (SoftwareEngineer `elem` roles)

testBackendEngineer :: Test
testBackendEngineer =
  TestCase $ do
    let post = mkPost ["Backend Engineer role in Python"]
        feats = features post
        roles = getRoles feats
    assertBool "Should extract Backend Engineer role" (BackendEngineer `elem` roles)

testSalaryRange :: Test
testSalaryRange =
  TestCase $ do
    let post = mkPost ["Salary: $150k - $200k"]
        feats = features post
        salaries = getSalaries feats
    assertBool ("Should extract salary, got: " ++ show salaries) (not $ null salaries)
    case salaries of
      (SalaryInfo _ (Just amt) : _) ->
        assertBool ("Should convert to GBP (around £157k), got: " ++ show amt) (amt > 150000 && amt < 165000)
      (SalaryInfo txt Nothing : _) ->
        assertFailure $ "Extracted salary text '" ++ show txt ++ "' but no amount"
      _ -> assertFailure "Should have extracted salary amount"

testSalarySingle :: Test
testSalarySingle =
  TestCase $ do
    let post = mkPost ["Compensation: £120k"]
        feats = features post
        salaries = getSalaries feats
    assertBool ("Should extract salary, got: " ++ show salaries) (not $ null salaries)
    case salaries of
      (SalaryInfo _ (Just amt) : _) ->
        assertBool ("Should be around £120k, got: " ++ show amt) (amt > 115000 && amt < 125000)
      (SalaryInfo txt Nothing : _) ->
        assertFailure $ "Extracted salary text '" ++ show txt ++ "' but no amount"
      _ -> assertFailure "Should have extracted salary amount"

testRemoteStatus :: Test
testRemoteStatus =
  TestCase $ do
    let post = mkPost ["100% REMOTE (Global)"]
        feats = features post
    assertBool "Should extract remote status" (hasFeature isRemoteness feats)

testRemoteUSA :: Test
testRemoteUSA =
  TestCase $ do
    let post = mkPost ["Remote (USA)"]
        feats = features post
        locs = getLocations feats
    assertBool "Should extract remote status" (hasFeature isRemoteness feats)
    assertBool ("Should extract USA location, got: " ++ show locs) (US `elem` locs)

testLocationUK :: Test
testLocationUK =
  TestCase $ do
    let post = mkPost ["Location: London, UK"]
        feats = features post
        locs = getLocations feats
    assertBool "Should extract London" (London `elem` locs)
    assertBool "Should extract UK" (UK `elem` locs)

testTechStack :: Test
testTechStack =
  TestCase $ do
    let post = mkPost ["Stack: Python, Rust, TypeScript"]
        feats = features post
        techs = getTechs feats
    assertBool "Should extract Python" (Python `elem` techs)
    assertBool "Should extract Rust" (Rust `elem` techs)
    assertBool "Should extract TypeScript" (TypeScript `elem` techs)

testMixRankPost :: Test
testMixRankPost =
  TestCase $ do
    let post =
          mkPost
            [ "MixRank | Software Engineers | 100% REMOTE (Global) | Full-Time",
              "Junior Software Engineer - Remote (Global), Full-Time",
              "We're looking for remote junior engineers",
              "Software Engineer - Remote (Global), Full-Time",
              "Python, Rust, SQL, Javascript/TypeScript"
            ]
        feats = features post
        roles = getRoles feats
    assertBool "Should extract at least one role" (not $ null roles)
    assertBool "Should extract Junior Engineer" (JuniorEngineer `elem` roles)
    assertBool "Should extract Software Engineer" (SoftwareEngineer `elem` roles)

-- Unit tests for parseSalary
testParseSalary150k :: Test
testParseSalary150k =
  TestCase $ do
    let SalaryInfo txt amt = parseSalary "$150k"
    assertEqual "Should extract text" "$150k" txt
    case amt of
      Just a -> assertBool ("Should be around £118k, got: " ++ show a) (a > 115000 && a < 122000)
      Nothing -> assertFailure "Should have extracted amount from $150k"

testParseSalaryRange :: Test
testParseSalaryRange =
  TestCase $ do
    let SalaryInfo txt amt = parseSalary "$150k - $200k"
    assertEqual "Should extract text" "$150k - $200k" txt
    case amt of
      Just a -> assertBool ("Should be max ~£157k, got: " ++ show a) (a > 150000 && a < 165000)
      Nothing -> assertFailure "Should have extracted amount from $150k - $200k"

testParseSalaryPound :: Test
testParseSalaryPound =
  TestCase $ do
    let SalaryInfo txt amt = parseSalary "£120k"
    assertEqual "Should extract text" "£120k" txt
    case amt of
      Just a -> assertBool ("Should be £120k, got: " ++ show a) (a > 115000 && a < 125000)
      Nothing -> assertFailure "Should have extracted amount from £120k"

tests :: Test
tests =
  TestList
    [ TestLabel "Junior Software Engineer" testJuniorSoftwareEngineer,
      TestLabel "Senior Software Engineer" testSeniorSoftwareEngineer,
      TestLabel "Staff Engineer" testStaffEngineer,
      TestLabel "Software Engineer" testSoftwareEngineer,
      TestLabel "Backend Engineer" testBackendEngineer,
      TestLabel "Parse Salary $150k" testParseSalary150k,
      TestLabel "Parse Salary Range" testParseSalaryRange,
      TestLabel "Parse Salary £120k" testParseSalaryPound,
      TestLabel "Salary Range" testSalaryRange,
      TestLabel "Salary Single" testSalarySingle,
      TestLabel "Remote Status" testRemoteStatus,
      TestLabel "Remote USA" testRemoteUSA,
      TestLabel "Location UK" testLocationUK,
      TestLabel "Tech Stack" testTechStack,
      TestLabel "MixRank Post" testMixRankPost
    ]
