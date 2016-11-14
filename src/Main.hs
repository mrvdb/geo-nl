module Main where

-- A demo program to convers ETRS89 coordinates to RD coordinates
-- Suggested name: geo-nl (because it is roughly equivalent to geo-uk)
-- 
-- Given two numbers representing a latitude and a longitude, convert
-- these numbers to RD coordinates with at least 1m precision
-- Example:
-- Input : N 51.6439  E 4.4376
-- Output: X 89273.89 Y 404546.25
--

import Text.Printf (printf)
import Control.Lens ((#), (^.))
import Options.Applicative

-- The coordinate package has lots of useful stuff for us
import Data.Geodetic.LL
import Data.Geodetic.XY 
import Data.Radian (toRadians)

-- Process commandline options for geo-nl
import Options
    
-- Convert ll coordinates to pseudo-rd coordinates
-- pseudo, because we don't apply the correction grid
ll2rd :: HasLL s => s -> XY
ll2rd ll = XY rdx rdy where
    -- Constants needed for calculation
    e, m, n, k, r :: Double
    e = 0.081696831222       -- excentricity of bessel ellips
    m = 0.003773953832       -- correction factors for gauss projection
    n = 1.00047585668        -- 
    k = 0.9999079            -- scaling factor for projection
    r = 6382644.571          -- radius of sphere

    -- Definition of the datumpoint for the RD system
    -- The datumpoint of the RD coordinate system lies in Amersfoort and
    -- is assigned a 'smart' offset of X=155000 and Y=463000 with the unit
    -- 'meters'. The choice of these values have the following reasons,
    -- which prove to be useful in practice:
    -- - X and Y will always be positive in the 'valid RD area'
    -- - min (Y) > max(X) :: it will always be clear which coordinate is which
    rd0 :: XY; x0, y0, b0 :: Double
    rd0 = XY 155000.0 463000.0    -- RD coordinates of the datumpoint
    x0 = rd0 ^. x; y0 = rd0 ^. y  -- 
    b0 = toRadians # 52.121097249 -- width and length on the sphere

    -- RD datumpoint in latlon coord
    ll0 :: LL
    φ, λ, λ0 :: Double
    ll0 = LL 52.156160556 5.387638889
    λ0 = toRadians # (ll0 ^. lon)

    -- Transform input so we can use it
    φ = toRadians # (ll ^. lat) -- latitude in radians 
    λ = toRadians # (ll ^. lon) -- longitude in radians

    -- Derived values
    q, w, b, dl, d :: Double
    q = atanh(sin φ) -
        (e * atanh(e * sin φ))    -- isometric distance on ellipsoid
    w = n*q + m                   -- isometric width on sphere
    
    b = 2*atan(exp w) - pi/2      -- width and length on sphere
    dl = n * (λ -λ0)             -- 
      
    -- Finally, calculate the RD x and y components
    d = 1 + sin b * sin b0 + cos b * cos b0 * cos dl
    rdx = 2 * k * r * ((sin dl * cos b) / d) + x0
    rdy = 2 * k * r * ((sin b * cos b0 - cos b * sin b0 * cos dl) / d) + y0
    
-- Entry point
main :: IO ()
main = do
  s <- execParser opts
  let rd = ll2rd ( LL (clat s) (clon s))
  printf "RD-coordinaten: %.2f / %.2f \n" (rd ^. x) (rd ^. y)
  where
    opts = info (helpOption
                 <*> versionOption
                 <*> options)
           programInfo
