import qualified Ch15.FirstSpec
import qualified Ch15.MadnessSpec
import qualified Ch15.MonoidSpec
import qualified Ch15.OptionalSpec
import qualified Ch15.SemigroupSpec

main :: IO ()
main = do
  Ch15.FirstSpec.main
  Ch15.MadnessSpec.main
  Ch15.MonoidSpec.main
  Ch15.OptionalSpec.main
  Ch15.SemigroupSpec.main
