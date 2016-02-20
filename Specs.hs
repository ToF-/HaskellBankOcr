import Test.Hspec
import BankOcr

main = hspec $ do
    describe "Bank OCR" $ do
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
        describe "checksum" $ do
            it "should compute the checksum" $ do
                checksum "457508000" `shouldBe` True
                checksum "457518000" `shouldBe` False

        describe "process" $ do
            it "should output the accunt with mentions if illegible or erroneous" $ do
                process [" _     _  _     _  _  _  _ "
                        ,"| |  | _| _||_||_ |_   ||_|"
                        ,"|_|  ||_  _|  | _||_|  ||_|"
                        ,"    _  _  _  _  _  _  _  _ "
                        ,"|_||_   ||_ | ||_|| || || |"
                        ,"  | _|  | _||_||_||_||_||_|"
                        ,"       _  _     _  _  _  _ "
                        ,"| |  | _| _||_||_ |_   ||_|"
                        ,"|_|  ||_  _|  | _||_|  ||_|"] `shouldBe` ["012345678 ERR","457508000","?12345678 ILL"]


        

