-- Porting this file to ajhc has made it disgusting. Before, ...
--    {-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
-- ... had made the instance declarations invisible, but these extensions
-- appear not to be available in ajhc.
module DataHand.Layout (Layout(..), Layer(..), RawLayer(..), toRaw) where
import Data.Monoid
import Control.Applicative
import Data.Traversable
import Data.Foldable
import DataHand.Keys

data Layout = Layout { normal :: Layer Key
                     , normalS :: Layer Key -- nulls are just normal+shiftKey, otherwise send the keycode in this layer
                     , nas :: Layer Key
                     , nasS :: Layer Key
                     , function :: Layer Key
                     , functionS :: Layer Key
                     , tenk :: Layer Key
                     , tenkS :: Layer Key
    } deriving (Show, Eq)

data Layer t = EmptyLayer
             -- Each layer has 26x2 values, usually of type Key or Int
             | Layer t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t
             deriving (Show, Eq)--, Functor, Foldable, Traversable)

instance Functor Layer where
    fmap f EmptyLayer = EmptyLayer
    fmap f (Layer
        a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1
        n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1
        a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2
        n2 o2 p2 q2 r2 s2 t2 u2 v2 w2 x2 y2 z2) = Layer
        (f a1) (f b1) (f c1) (f d1) (f e1) (f f1) (f g1) (f h1) (f i1) (f j1) (f k1) (f l1) (f m1)
        (f n1) (f o1) (f p1) (f q1) (f r1) (f s1) (f t1) (f u1) (f v1) (f w1) (f x1) (f y1) (f z1)
        (f a2) (f b2) (f c2) (f d2) (f e2) (f f2) (f g2) (f h2) (f i2) (f j2) (f k2) (f l2) (f m2)
        (f n2) (f o2) (f p2) (f q2) (f r2) (f s2) (f t2) (f u2) (f v2) (f w2) (f x2) (f y2) (f z2)

instance Foldable Layer where
    foldMap f EmptyLayer = mempty
    foldMap f (Layer
        a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1
        n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1
        a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2
        n2 o2 p2 q2 r2 s2 t2 u2 v2 w2 x2 y2 z2) = f a1 `mappend` f b1 `mappend`
        f c1 `mappend` f d1 `mappend` f e1 `mappend` f f1 `mappend` f g1 `mappend`
        f h1 `mappend` f i1 `mappend` f j1 `mappend` f k1 `mappend` f l1 `mappend`
        f m1 `mappend` f n1 `mappend` f o1 `mappend` f p1 `mappend` f q1 `mappend`
        f r1 `mappend` f s1 `mappend` f t1 `mappend` f u1 `mappend` f v1 `mappend`
        f w1 `mappend` f x1 `mappend` f y1 `mappend` f z1 `mappend` f a2 `mappend`
        f b2 `mappend` f c2 `mappend` f d2 `mappend` f e2 `mappend` f f2 `mappend`
        f g2 `mappend` f h2 `mappend` f i2 `mappend` f j2 `mappend` f k2 `mappend`
        f l2 `mappend` f m2 `mappend` f n2 `mappend` f o2 `mappend` f p2 `mappend`
        f q2 `mappend` f r2 `mappend` f s2 `mappend` f t2 `mappend` f u2 `mappend`
        f v2 `mappend` f w2 `mappend` f x2 `mappend` f y2 `mappend` f z2

instance Traversable Layer where
    traverse f EmptyLayer = pure EmptyLayer
    traverse f (Layer
        a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1
        n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1
        a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2
        n2 o2 p2 q2 r2 s2 t2 u2 v2 w2 x2 y2 z2) = Layer
        <$> f a1 <*> f b1 <*> f c1 <*> f d1 <*> f e1 <*> f f1 <*> f g1 <*> f h1 <*> f i1 <*> f j1 <*> f k1 <*> f l1 <*> f m1
        <*> f n1 <*> f o1 <*> f p1 <*> f q1 <*> f r1 <*> f s1 <*> f t1 <*> f u1 <*> f v1 <*> f w1 <*> f x1 <*> f y1 <*> f z1
        <*> f a2 <*> f b2 <*> f c2 <*> f d2 <*> f e2 <*> f f2 <*> f g2 <*> f h2 <*> f i2 <*> f j2 <*> f k2 <*> f l2 <*> f m2
        <*> f n2 <*> f o2 <*> f p2 <*> f q2 <*> f r2 <*> f s2 <*> f t2 <*> f u2 <*> f v2 <*> f w2 <*> f x2 <*> f y2 <*> f z2

-- This type is just for remapping the indexes of the keys to the order they appear in the firmware.
data RawLayer t = EmptyRawLayer -- TODO should be all nulls version of the next?
                | RawLayer t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t
                deriving (Show, Eq)--, Functor, Foldable, Traversable)

instance Functor RawLayer where
    fmap f EmptyRawLayer = EmptyRawLayer
    fmap f (RawLayer
        a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1
        n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1
        a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2
        n2 o2 p2 q2 r2 s2 t2 u2 v2 w2 x2 y2 z2) = RawLayer
        (f a1) (f b1) (f c1) (f d1) (f e1) (f f1) (f g1) (f h1) (f i1) (f j1) (f k1) (f l1) (f m1)
        (f n1) (f o1) (f p1) (f q1) (f r1) (f s1) (f t1) (f u1) (f v1) (f w1) (f x1) (f y1) (f z1)
        (f a2) (f b2) (f c2) (f d2) (f e2) (f f2) (f g2) (f h2) (f i2) (f j2) (f k2) (f l2) (f m2)
        (f n2) (f o2) (f p2) (f q2) (f r2) (f s2) (f t2) (f u2) (f v2) (f w2) (f x2) (f y2) (f z2)

instance Foldable RawLayer where
    foldMap f EmptyRawLayer = mempty
    foldMap f (RawLayer
        a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1
        n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1
        a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2
        n2 o2 p2 q2 r2 s2 t2 u2 v2 w2 x2 y2 z2) = f a1 `mappend` f b1 `mappend`
        f c1 `mappend` f d1 `mappend` f e1 `mappend` f f1 `mappend` f g1 `mappend`
        f h1 `mappend` f i1 `mappend` f j1 `mappend` f k1 `mappend` f l1 `mappend`
        f m1 `mappend` f n1 `mappend` f o1 `mappend` f p1 `mappend` f q1 `mappend`
        f r1 `mappend` f s1 `mappend` f t1 `mappend` f u1 `mappend` f v1 `mappend`
        f w1 `mappend` f x1 `mappend` f y1 `mappend` f z1 `mappend` f a2 `mappend`
        f b2 `mappend` f c2 `mappend` f d2 `mappend` f e2 `mappend` f f2 `mappend`
        f g2 `mappend` f h2 `mappend` f i2 `mappend` f j2 `mappend` f k2 `mappend`
        f l2 `mappend` f m2 `mappend` f n2 `mappend` f o2 `mappend` f p2 `mappend`
        f q2 `mappend` f r2 `mappend` f s2 `mappend` f t2 `mappend` f u2 `mappend`
        f v2 `mappend` f w2 `mappend` f x2 `mappend` f y2 `mappend` f z2

instance Traversable RawLayer where
    traverse f EmptyRawLayer = pure EmptyRawLayer
    traverse f (RawLayer
        a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1
        n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1
        a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 m2
        n2 o2 p2 q2 r2 s2 t2 u2 v2 w2 x2 y2 z2) = RawLayer
        <$> f a1 <*> f b1 <*> f c1 <*> f d1 <*> f e1 <*> f f1 <*> f g1 <*> f h1 <*> f i1 <*> f j1 <*> f k1 <*> f l1 <*> f m1
        <*> f n1 <*> f o1 <*> f p1 <*> f q1 <*> f r1 <*> f s1 <*> f t1 <*> f u1 <*> f v1 <*> f w1 <*> f x1 <*> f y1 <*> f z1
        <*> f a2 <*> f b2 <*> f c2 <*> f d2 <*> f e2 <*> f f2 <*> f g2 <*> f h2 <*> f i2 <*> f j2 <*> f k2 <*> f l2 <*> f m2
        <*> f n2 <*> f o2 <*> f p2 <*> f q2 <*> f r2 <*> f s2 <*> f t2 <*> f u2 <*> f v2 <*> f w2 <*> f x2 <*> f y2 <*> f z2

-- TODO this is almost certainly not the best way to represent+effect this mapping?
toRaw EmptyLayer = EmptyRawLayer
toRaw (Layer
--      +----+           +----+           +----+           +----+
          a                b                c                d
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     e    f    g     h     i    j      k    l    m      n    o    p      q    r    s
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
          t                u                v                w           x    y    z
--      +----+           +----+           +----+           +----+      +----+----+----+

--                       +----+           +----+           +----+           +----+
                           aa               bb               cc               dd
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     ee   ff   gg     hh   ii   jj     kk   ll   mm     nn   oo   pp     qq   rr   ss
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     tt   uu   vv          ww               xx               yy               zz )
-- +----+----+----+      +----+           +----+           +----+           +----+     
    = (RawLayer
--      +----+           +----+           +----+           +----+
          hh               aa               e                a 
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     ii   jj   f      g    ww   xx     t    u    kk     bb   h    b      ll   mm   i 
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
          j                nn               cc               k           c    oo   pp
--      +----+           +----+           +----+           +----+      +----+----+----+

--                       +----+           +----+           +----+           +----+
                           l                m                yy               zz
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     v    w    qq     dd   n    d      rr   ss   o      p    tt   ee     z    s    uu
-- +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+ +----+----+----+
     ff   y    r           gg               vv               q                x  )
-- +----+----+----+      +----+           +----+           +----+           +----+     
