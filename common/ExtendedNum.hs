module ExtendedNum 
(
  ExtendedNum(..),
  extendedNum2Num
) where

data ExtendedNum a = Only a | NegInf | PosInf deriving (Eq, Show)

instance (Num a, Eq a) => Num (ExtendedNum a) where
        Only a + Only b = Only (a + b)
        Only _ + NegInf = NegInf
        Only _ + PosInf = PosInf
        NegInf + NegInf = NegInf
        PosInf + PosInf = PosInf
        NegInf + PosInf = 0
        x + y = y + x

        x - y = x + Only (-1) * y

        Only a * Only b = Only (a * b)
        Only a * PosInf
                | a == 0 = Only 0
                | signum a == 1 = PosInf
                | signum a == -1 = NegInf
        Only a * NegInf
                | a == 0 = Only 0
                | signum a == 1 = NegInf
                | signum a == -1 = PosInf
        PosInf * PosInf = PosInf
        NegInf * NegInf = PosInf
        PosInf * NegInf = NegInf
        x * y = y * x

        abs (Only a) = Only (abs a)
        abs NegInf = PosInf
        abs PosInf = PosInf
        
        signum (Only a) = Only (signum a)
        signum PosInf = Only 1
        signum NegInf = Only (-1)

        fromInteger a = Only $ fromInteger a

instance (Ord a) => Ord (ExtendedNum a) where
        Only a <= Only b = a <= b
        NegInf <= Only _ = True
        Only _ <= NegInf = False
        PosInf <= Only _ = False
        Only _ <= PosInf = True
        NegInf <= PosInf = True
        PosInf <= NegInf = False
        PosInf <= PosInf = True
        NegInf <= NegInf = True

instance Functor ExtendedNum where
        fmap f (Only a) = Only (f a)
        fmap _ NegInf = NegInf
        fmap _ PosInf = PosInf

extendedNum2Num :: (Num a) => ExtendedNum a -> a
extendedNum2Num NegInf = error "Things went wrong!"
extendedNum2Num PosInf = error "Things went wrong!"
extendedNum2Num (Only x) = x