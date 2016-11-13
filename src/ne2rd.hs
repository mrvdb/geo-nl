-- A demo program to convers ETRS89 coordinates to RD coordinates
-- Suggested name: geo-nl (because it is roughly equivalent to geo-uk)
-- 
-- Given two numbers representing a lattitude and a longitude, convert
-- these numbers to RD coordinates with at least 1m precision
-- Example:
-- Input : N 51.6439  E 4.4376
-- Output: X 89273.89 Y 404546.25
--
--
-- Potential interesting libraries:
-- -- geo-uk :: this seems like the same system as RD
-- -- Data.Geodetic :: definition of geodetic ellipsi which may be helpful for clarity
--

import Text.Printf (printf)

-- Inputs (to be read at runtime later)
type LatLoncoord = (Double, Double)
input :: LatLoncoord
input = (51.6439, 4.4376)
      
-- Static data definitions

-- Eulers constant, surprisingly(?) not predefined in Haskell
e :: Double
e = exp 1

-- The datumpoint of the RD coordinate system lies in Amersfoort and
-- is assigned 'smart' offset of X=155000 and Y=463000 with the unit
-- 'meters'. The choice of these values have the following reasons,
-- which prove to be useful in practice:
-- - X and Y will always be positive in the 'valid RD area'
-- - min (Y) > max(X) :: it will always be clear which coordinate is which
type RDcoord = (Integer, Integer)
rd0 :: RDcoord
rd0 = (155000 , 463000)
      
-- RD datumpoint in latlon coord
ll0 :: LatLoncoord
ll0 = (52.0 + 9.0/60.0 + 22.178/3600,
        5.0 + 23.0/60.0 + 15.5/3600)


-- ==================================================================

-- 
n, m, a_bessel, inv_f_bessel, a_etrs, inv_f_etrs :: Double
n            = 1.00047585668
m            = 0.003773953832
-- short axis of bessel ellipsoid
a_bessel     = 6377397.155
inv_f_bessel = 299.1528128
-- short axis of etrs ellipsoid
a_etrs       = 6378137
inv_f_etrs   = 298.257222101

-- deltaL = n*(lat - lat0)

-- tau0 = log(tan((pi+2*lat0)/4)*((1-e*sin(lat))/(1+e*sin(lat)))^(e/2))

-- t = n*tau + m
-- b = 2 * atan(e^t) - pi/2

k = 0.9999079
r = 6382644.571
-- rdX = x0+ (2*k.r.(sin(deltaL)*cos(b))/(1+sin(b)sin(b0)+cos(b)cos(b0)*deltaL))


-- helper functions for taking gonio from degree based values
deg2rad :: Double -> Double
deg2rad x = 1.0/180.0*pi*x

degSin :: Double -> Double
degSin  x = sin(deg2rad x)

degCos  :: Double -> Double
degCos  x = cos(deg2rad x)

degTan  :: Double -> Double
degTan  x = tan(deg2rad x)

degAtan  :: Double -> Double
degAtan x = atan(deg2rad x)

-- Coordinates
type Geocoord  = (Double, Double, Double)
type Cartesian = (Double, Double, Double)
type DegMinSec = (Integer, Integer, Double)

deg_min_sec2decimal:: DegMinSec -> Double
deg_min_sec2decimal (degrees, minutes, seconds) =
    fromIntegral degrees + fromIntegral minutes /60.0 + seconds/3600.0

decimal2deg_min_sec :: Double -> DegMinSec
decimal2deg_min_sec dec_deg = (degrees,minutes,seconds)
  where
    degrees = truncate dec_deg
    minutes = truncate ((dec_deg - fromIntegral degrees) * 60.0)
    seconds = ((dec_deg- fromIntegral degrees)*60.0 - fromIntegral minutes)*60.0


geographic2cartesian :: Geocoord -> Double -> Double -> Cartesian
geographic2cartesian (lattitude, longitude, height) a inv_f = (x, y, z)
  where
    f  = 1.0/inv_f
    ee = f*(2.0-f)
    n2  = a/sqrt(1.0-ee*(degSin lattitude **2))
    x = (n2+height)* degCos lattitude * degCos longitude
    y = (n2+height)* degCos lattitude * degSin longitude
    z = (n2*(1.0-ee)+height) * degSin lattitude

   

--     lon = deg_atan(y/x)
--     h = rho*deg_cos(lat)+z*deg_sin(lat)-n*(1.0-ee*(deg_sin(lat)^2))

-- Steps in conversion of phi,lambda, h to x,y,z
-- 1. convert geo to cart of 0 point
-- 2. apply translation (no rotation needed here)
-- [ HAVE: correct x,y,z for 0 point now]
-- 3. geo to cart of phi, lambda, h
-- 4. apply transform/rotate with x0,y0,z0 as reference
-- 5. convert cart to geo for point
-- 6. apply RD projection
-- [HAVE x_pseudo, y_pseudo, z_pseudo here]
-- 7. apply correction data, if memory allow


main :: IO ()
main = do
  -- printf "𝞓L=%f\n" deltaL
  printf "RD-coordinaten: ??\n"
