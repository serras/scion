
module Scion.Types.JSONDictionary where

import qualified Data.ByteString.Char8 as S
import qualified Data.Map as M

import Text.JSON.AttoJSON

lookupKey :: JSON a => JSValue -> S.ByteString -> Maybe a
lookupKey = flip Text.JSON.AttoJSON.lookup

makeObject :: [(S.ByteString, JSValue)] -> JSValue
makeObject = JSObject . M.fromList

file :: S.ByteString
file = S.pack "file"
other :: S.ByteString
other=S.pack "other"
region :: S.ByteString
region=S.pack "region"

output :: S.ByteString
output = S.pack "output"
forcerecomp :: S.ByteString
forcerecomp = S.pack "forcerecomp"
cabalfile ::  S.ByteString
cabalfile=S.pack "cabal-file"

error ::  S.ByteString
error=S.pack "error"
warnings :: S.ByteString
warnings=S.pack "warnings"
warning ::  S.ByteString
warning=S.pack "warning"
info ::  S.ByteString
info=S.pack "info"

name :: S.ByteString
name=S.pack  "name"
version :: S.ByteString
version=S.pack "version"
exposed  :: S.ByteString
exposed=S.pack "exposed"
dependent :: S.ByteString
dependent=S.pack "dependent"
library :: S.ByteString
library=S.pack "library"
executable :: S.ByteString
executable=S.pack "executable"
testsuite :: S.ByteString
testsuite=S.pack "testsuite"
buildable :: S.ByteString
buildable=S.pack "buildable"
result :: S.ByteString
result=S.pack "result"

method :: S.ByteString
method=S.pack "method"
params :: S.ByteString
params=S.pack "params"
id :: S.ByteString
id=S.pack "id"
version01 :: S.ByteString
version01=S.pack "0.1"
message :: S.ByteString
message=S.pack "message"
pid :: S.ByteString
pid=S.pack "pid"
succeeded :: S.ByteString
succeeded=S.pack "succeeded"
notes :: S.ByteString
notes=S.pack "notes"
duration :: S.ByteString
duration=S.pack "duration"

kind :: S.ByteString
kind=S.pack "kind"
location :: S.ByteString
location=S.pack "location"

leftC :: S.ByteString
leftC=S.pack "Left"
rightC :: S.ByteString
rightC=S.pack "Right"
nothingC :: S.ByteString
nothingC=S.pack "Nothing"
justC :: S.ByteString
justC=S.pack "Just"

noLocation :: S.ByteString
noLocation=S.pack "no-location"
block :: S.ByteString
block=S.pack "block"
typ :: S.ByteString
typ=S.pack "type"
parent :: S.ByteString
parent=S.pack "parent"
quit :: S.ByteString
quit=S.pack "quit"

empty :: S.ByteString
empty=S.pack ""
scionPrefix :: S.ByteString
scionPrefix = S.pack "scion:"
newline :: S.ByteString
newline = S.pack "\n"

modules :: S.ByteString
modules=S.pack "modules"
