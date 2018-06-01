--
-- Copyright 2017, 2018 Warlock <internalmike@gmail.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

import Control.Monad hiding (fail)
import Test.HUnit.Base hiding (Label)
import Test.HUnit.Text
import qualified Data.Conduit.Parsers.Binary.Get.Spec
import qualified Data.Conduit.Parsers.Binary.Put.Spec
import qualified Data.Conduit.Parsers.Text.Parser.Spec
import qualified Data.Conduit.Parsers.Text.Gen.Spec

main :: IO ()
main = void $ runTestTT tests

tests :: Test
tests = TestList
  [ Data.Conduit.Parsers.Binary.Get.Spec.tests
  , Data.Conduit.Parsers.Binary.Put.Spec.tests
  , Data.Conduit.Parsers.Text.Parser.Spec.tests
  , Data.Conduit.Parsers.Text.Gen.Spec.tests
  ]
