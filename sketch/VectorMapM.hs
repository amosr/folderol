-- Vector.mapM goes via lists
-- It is actually unstreamM, which converts from a stream to a vector.
-- Because it is polymorphic in the monad, it cannot mix the IO/ST to create the vector with the arbitrary effects.
-- (It could, but it would be very hard to get the interleaving right - requiring multiple unsafePerformIOs)
import qualified Data.Vector as Vector
import qualified System.Environment as Env
import qualified Data.Vector.Fusion.Stream.Monadic as MStream

main = do
  [numStr] <- Env.getArgs
  let num = read numStr
  let iot = Vector.generate num id
  let err = Vector.mapM checkNum iot
  print (Vector.length <$> err)

checkNum :: Int -> Either String Int
checkNum i
 | i > 10^15 == 0
 = Left "Number too big"
 | otherwise
 = Right i

