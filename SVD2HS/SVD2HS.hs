{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module SVD2HS
where
import Prelude hiding ((<>))
import Text.XML.Lens hiding (text)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Text.XML.Lens as TXL
import Text.XML(readFile,def)
import Text.PrettyPrint
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

selectPeripheral ::
  Applicative f => (Element -> f Element) -> Document -> f Document

selectPeripheral = root
                  . el "device"
                  ./ el "peripherals"
                  ./ el "peripheral"

selectPeripheralName :: Applicative f => Over (->) f Document Document Text Text
selectPeripheralName = selectPeripheral ./ el "name" . TXL.text

selectRegister :: Applicative f => Over (->) f Document Document Element Element
selectRegister = selectPeripheral ./ el "registers" ./ el "register"

selectRegisterName :: Applicative f => Over (->) f Document Document Text Text
selectRegisterName = selectRegister ./ el "name" . TXL.text

selectField :: Applicative f => Over (->) f Document Document Element Element
selectField = selectRegister ./ el "fields" ./ el "field"

selectFieldName :: Applicative f => Over (->) f Document Document Text Text
selectFieldName = selectField ./ el "name" . TXL.text

svdFile :: FilePath
svdFile ="STM32F103xx.svd"

main :: IO ()
main = do
  svd <- Text.XML.readFile def svdFile
  Prelude.writeFile "Device.hs" $ svd2hs svd

svd2hs :: Document -> String
svd2hs svd = render hsModule
  where
     fieldTable = Map.fromList
           $ map (\(register,field,offset,width)
                  -> ((register,field),(offset,width)))
           $ concat
           $ svd ^.. selectRegister
           . to collectFields

     hsModule = vcat [
        text "-- Generated from"<+> text svdFile
       ,text "module Device"
       ,text "where"
       ,text "import Data.Word (Word32)"
       ,blankLine

       ,dataType
          "Peripheral"
           (Set.fromList $ svd ^.. selectPeripheralName)
       ,blankLine

       ,dataType
          "Register"
           (Set.fromList $ svd ^.. selectRegisterName)
       ,blankLine

       ,dataType
          "Field"
          (Set.fromList
           $ map (\(r,f) -> Text.concat [r,"_",f])
           $ Map.keys fieldTable)

       ,blankLine

       ,text "peripheralBase :: Peripheral -> Word32"
       ,funTable
          "peripheralBase"
          (svd ^.. selectPeripheral . to foldBaseAddress)
       ,blankLine

       ,text "registerOffset :: Peripheral -> Register -> Word32"
       ,funTable
          "registerOffset"
          (concat $ svd ^.. selectPeripheral . to foldRegisterOffset)
       ,text "--derived peripherals"
       ,funTable
          "registerOffset"
          (svd ^.. selectPeripheral . attributeSatisfies "derivedFrom" (const True)
          . to derivedPeripheral)
       ,text "--catch all"
       ,text "registerOffset p r = error $ show (\"undefined registerOffset\",p ,r)"      ,blankLine

       ,text "fieldToRegister :: Field -> Register"
       ,funTable
           "fieldToRegister"
           (map (\(reg,field)
                 -> (textText $ Text.concat [reg , "_" , field],textText reg))
           $ Map.keys fieldTable)
       ,blankLine

       ,text "fieldBitOffset :: Field -> Int"
       ,funTable
           "fieldBitOffset"
           (map (\((reg,field),(offset,_))
                 -> (textText $ Text.concat [reg , "_" , field],textText offset))
           $ Map.assocs fieldTable)
       ,blankLine

       ,text "fieldBitWidth :: Field -> Int"
       ,funTable
           "fieldBitWidth"
           (map (\((reg,field),(_,width))
                 -> (textText $ Text.concat [reg , "_" , field],textText width))
           $ Map.assocs fieldTable)

       ]

blankLine :: Doc
blankLine = text ""

foldBaseAddress :: Element -> (Doc, Doc)
foldBaseAddress x
  = (sel "peripheral" "name" x, sel "peripheral" "baseAddress" x)

derivedPeripheral :: Element -> (Doc,Doc)
derivedPeripheral p
  = (sel "peripheral" "name" p <+> text "reg"
    ,text "registerOffset"
     <+> head ( p ^.. el "peripheral" . attr "derivedFrom" . to textText)
     <+> text "reg"
    )


foldRegisterOffset :: Element -> [(Doc, Doc)]
foldRegisterOffset p
  = p ^.. el "peripheral" ./ el "registers" ./ el "register" . to fo
  where
     peri = sel "peripheral" "name" p
     fo x= (peri <+> sel "register" "name" x,
            sel "register" "addressOffset" x)

sel :: Name -> Name -> Element -> Doc
sel n c p
  = textText $ selText n c p

selText :: Name -> Name -> Element -> Text
selText n c p
  = head $ p ^.. el n ./ el c . TXL.text

collectFields :: Element -> [(Text, Text, Text, Text)]
collectFields r
  = r ^.. el "register" ./ el "fields" ./ el "field" . to fo
  where
    register = selText "register" "name" r
    fo x =(register
          ,selText "field" "name" x
          ,selText "field" "bitOffset" x
          ,selText "field" "bitWidth" x)


textText :: Data.Text.Text -> Doc
textText = text . Text.unpack

dataType :: String -> Set Text -> Doc
dataType typeName constructors = vcat [
    text "data" <+> text typeName
   ,nest 4 $ vcat $ zipWith (<>) seps $ map textText $ Set.toList constructors
   ,nest 4 $ text "deriving (Show,Eq,Ord)"
   ]
   where
     seps = text "=" : repeat (text "|")

funTable :: String -> [(Doc,Doc)] -> Doc
funTable funName assocs
    = vcat $ map mkCase assocs
  where
    mkCase (patterns, value) = hsep [
       text funName
      ,patterns
      ,equals
      ,value
      ]
