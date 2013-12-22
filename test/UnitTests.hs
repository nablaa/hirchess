import Test.HUnit

test1, test2 :: Test
test1 = TestCase $ assertEqual "test1" (2 :: Integer) (1 + 1 :: Integer)
test2 = TestCase $ assertEqual "test2" "foobar" ("foo" ++ "bar")

tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

main :: IO ()
main = do _ <- runTestTT $ tests
          return ()
