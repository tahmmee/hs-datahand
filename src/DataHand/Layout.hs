{-# LANGUAGE DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
module DataHand.Layout where
import Data.Traversable
import Data.Foldable
import DataHand.Keys

data Layout = Layout { normal :: Layer Key
                     , nas :: Layer Key
                     , function :: Layer Key
                     , tenk :: Layer Key
    } deriving (Show, Eq)

data Layer t = EmptyLayer
             -- Each layer has 26x2 values, usually of type Key or Int
             | Layer t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t
             deriving (Show, Eq, Functor, Traversable, Foldable)

data RawLayer t = EmptyRawLayer -- TODO should be all nulls version of the next?
                | RawLayer t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t t
                deriving (Show, Eq, Functor, Traversable, Foldable)

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
