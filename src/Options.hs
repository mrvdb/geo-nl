{-# LANGUAGE TemplateHaskell #-}
module Options
    (
     programInfo,
     options, clat, clon,
     versionOption,
     helpOption )where
-- Commandline options handling for geo-nl

import Options.Applicative
import Development.GitRev (gitHash)
import Data.Version (showVersion)
import Data.Monoid ((<>))
import Paths_geo_nl (version)       -- get the version from the cabal file I guess
    
-- Program info
programInfo :: InfoMod a
programInfo = fullDesc
              <> progDesc "Converts LAT, LON coordinates to an RD coordinate."
              <> header "geo-nl - convert WGS84 to RD coordinates"

-- Use builtin helper option
helpOption :: Parser (a -> a)
helpOption = helper
             
-- Support a version option, incl. githash
versionOption :: Parser (a -> a)
versionOption = infoOption
                (concat [showVersion version, " ", $(gitHash)])
                (long "version"
                <> short 'v' -- Char!!
                <> help "Show version")
-- latitude and longitude are required arguments
latArgument :: Parser Double
latArgument = argument auto
              (metavar "LAT"
              <> help "Latitude in decimal notation")
lonArgument :: Parser Double
lonArgument = argument auto
              (metavar "LON"
              <> help "Longitude in decimal notation")

-- Define type to store arguments and make sure we can show it
data Options = Options
    { clat     :: Double
    , clon     :: Double
    , cquiet   :: Bool} deriving Show

options :: Parser Options
options = Options 
          <$> latArgument
          <*> lonArgument
          <*> switch
              ( long "quiet"
              <> short 'q'
              <> help "Only show result, no text dressing" )


               
