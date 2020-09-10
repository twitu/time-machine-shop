module Apriori where

import           Data.Set                       ( Set )
import qualified Data.Set                      as S

-- Client information
data Client = GovOrg { clientName :: String }
            | Company { clientName :: String
                      , person :: Person, duty :: String }
            | Individual { person :: Person }
            deriving (Show, Eq, Ord)

data ClientKind = KindGovOrg | KindCompany | KindIndividual deriving (Show, Eq, Ord)

data Person = Person { firstName :: String, lastName :: String, gender :: Gender } deriving (Show, Eq, Ord)

data Gender = Male | Female | Unknown deriving (Show, Eq, Ord)

-- Product information
data Product = Product { productId :: Integer, productType :: ProductType } deriving (Show, Eq, Ord)

data ProductType = TimeMachine | TravelGuide | Tool | Trip deriving (Show, Eq, Ord)

data Purchase = Purchase { client :: Client, products :: [Product] } deriving (Show, Eq, Ord)

data PurchaseInfo = InfoClientKind ClientKind
                  | InfoClientDuty String
                  | InfoClientGender Gender
                  | InfoPurchasedProduct Integer
                  | InfoPurchasedProductType ProductType
                  deriving (Show, Eq, Ord)


newtype Transaction = Transaction (Set PurchaseInfo) deriving (Eq, Ord)

productsToPurchaseInfo :: [Product] -> Set PurchaseInfo
productsToPurchaseInfo = foldr
  (\(Product i t) pinfos -> S.insert (InfoPurchasedProduct i)
    $ S.insert (InfoPurchasedProductType t) pinfos
  )
  S.empty

clientToPurchaseInfo :: Client -> Set PurchaseInfo
clientToPurchaseInfo (GovOrg n) = S.insert (InfoClientKind KindGovOrg) S.empty
clientToPurchaseInfo (Company n p d) =
  S.insert (InfoClientKind KindCompany)
    . S.insert (InfoClientDuty d)
    . S.insert (InfoClientGender $ gender p)
    $ S.empty
clientToPurchaseInfo (Individual p) =
  S.insert (InfoClientGender $ gender p) S.empty
