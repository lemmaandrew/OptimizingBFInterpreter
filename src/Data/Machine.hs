{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module which defines the @Machine@ data type and some functions in it
module Data.Machine
    ( Machine(pointer, memory)
    , initialize
    , size
    , pval
    , adjustPVal
    , setPVal
    , movePointer
    ) where

import           Control.Monad.Primitive     ( PrimMonad (PrimState) )
import qualified Data.Vector.Unboxed.Mutable as MVU
import           GHC.TypeLits                ( KnownNat, natVal, type (<=) )
import           Util                        ( Word8 )

-- | The machine which handles memory manipulation
data Machine s =
    Machine
        { pointer :: Int
        , memory  :: MVU.MVector s Word8
        }

-- | Initialize a @Machine@ with @pointer == 0@ and a 0-filled @MVU.MVector@ of length @n@
initialize ::
       (KnownNat n, 1 <= n, PrimMonad m) => proxy n -> m (Machine (PrimState m))
initialize p =
    case fromIntegral $ natVal p of
        n -> Machine 0 <$> MVU.replicate n 0

-- | Gets the length of a @Machine@'s memory
size :: Machine s -> Int
size = MVU.length . memory

-- | Gets the pointer value of a @Machine@
pval :: PrimMonad m => Machine (PrimState m) -> m Word8
pval (Machine p mem) = MVU.read mem p

-- | Adjusts the pointer value of a @Machine@ with a function @Word8 -> Word8@
adjustPVal ::
       PrimMonad m
    => (Word8 -> Word8)
    -> Machine (PrimState m)
    -> m (Machine (PrimState m))
adjustPVal f (Machine p mem) = MVU.modify mem f p >> return (Machine p mem)

-- | Sets the pointer value of a @Machine@ to a @Word8@
setPVal ::
       PrimMonad m
    => Word8
    -> Machine (PrimState m)
    -> m (Machine (PrimState m))
setPVal val (Machine p mem) = MVU.write mem p val >> return (Machine p mem)

-- | Moves the pointer @p@ @mv@ positions right or left.
-- If @p + mv < 0@, wraps around to @size machine - abs (p + mv)@.
-- If @p + mv >= size machine@, grows the vector to accomodate such that @p@ becomes equal to @size machine - 1@
movePointer ::
       PrimMonad m => Int -> Machine (PrimState m) -> m (Machine (PrimState m))
movePointer mv machine@(Machine p mem) =
    let p' = p + mv
        sm = size machine
    in if | p' >= sm  -> Machine p' <$> MVU.grow mem (p' - sm + 1)
           | p' < 0    -> return $ Machine (sm + p') mem
           | otherwise -> return $ Machine p' mem
