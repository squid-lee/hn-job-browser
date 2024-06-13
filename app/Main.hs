module Main where

import Brick
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Text (Text)
import qualified Data.Text.IO as T
import HNI.App
import HNI.Decoded
import HNI.Post
import HNI.PrettyPrint
import System.Environment

main :: IO ()
main = main1

main1 :: IO ()
main1 = do
  -- as <- getArgs
  -- (json, filter) <- case as of
  --   [f] -> (,id) <$> BS.readFile f
  --   [f, strs] -> (,mapMaybe (regexen (split ',' strs))) <$> BS.readFile f
  --   _ -> error "OH NO"

  json <- BS.readFile "/tmp/hn-jun24.json"

  posts <- either error (pure . children . fmap HNI.Decoded.decode) (eitherDecode json)

  _ <- defaultMain app $ newState posts
  return ()

main2 :: IO ()
main2 = T.putStrLn $ ppPost p
  where
    p :: Post Decoded
    p = HNI.Decoded.decode <$> Post {author = "bdod6", children = [], createdAt = "2024-06-07T08:48:34.000Z", createdAtI = 1717750114, postId = 40606674, options = [], parentId = Just 40563283, points = Nothing, storyId = 40563283, text = "Doowii | Full Stack Engineer | Remote (US) | Full-Time<p>Doowii is on a mission to empower educators with AI-driven data analytics solutions that enhance educational outcomes. We\8217re a rapidly growing EdTech startup that recently closed a $4.1M fundraising round backed by GSV Ventures, Better VC, Avesta Fund, and more (press release: <a href=\"http:&#x2F;&#x2F;bit.ly&#x2F;3Rfodmt\" rel=\"nofollow\">http:&#x2F;&#x2F;bit.ly&#x2F;3Rfodmt</a>)<p>What We Do:<p>-Provide AI-driven data analytics solutions for schools&#x2F;universities, essentially serving as an AI data scientist for educators.\n-Seamlessly integrate advanced data analytics into major EdTech platforms through partnerships.<p>About the Role:\nWe\8217re looking for a Full Stack Engineer who is passionate about making a difference in education. You&#x27;ll work on developing and maintaining scalable web applications using a modern techstack. Your role will involve collaborating with other engineers to design and implement new features, optimizing our applications for genAI accuracy, speed, and scalability, and contributing to a high standard of code quality. Needs to have the startup hustle mindset.<p>How to Apply:<p>Go to this link: <a href=\"https:&#x2F;&#x2F;jobs.ashbyhq.com&#x2F;Doowii&#x2F;23a76737-5510-4f44-a6c7-db0e8ddfaffc\">https:&#x2F;&#x2F;jobs.ashbyhq.com&#x2F;Doowii&#x2F;23a76737-5510-4f44-a6c7-db0e...</a><p>Looking forward to hearing from you!", title = Nothing, typ = "comment", url = Nothing}
