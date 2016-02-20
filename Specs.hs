import Test.Hspec
import BankOcr

main = hspec $ do
    describe "convert" $ do
        it "should convert one digit" $ do
            convert [" _ "
                    ,"| |"
                    ,"|_|"] `shouldBe` "0"
            convert ["   "
                    ,"  |"
                    ,"  |"] `shouldBe` "1"

        it "should convert to ? when digit not found" $ do
            convert [" _ "
                    ,"  |"
                    ,"|_|"] `shouldBe` "?"

        it "should convert several digits" $ do
            convert [" _     _  _     _  _  _  _  _ "
                    ,"| |  | _| _||_||_ |_   ||_||_|"
                    ,"|_|  ||_  _|  | _||_|  ||_| _|"] `shouldBe` "0123456789"

        it "should compute the checksum" $ do
            checksum "457508000" `shouldBe` True
            checksum "457518000" `shouldBe` False

