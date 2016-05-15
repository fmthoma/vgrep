import Test.DocTest

main :: IO ()
main =
    let extensions =
            [ "-XLambdaCase"
            , "-XMultiWayIf"
            , "-XOverloadedStrings" ]
        sourceFolders =
            [ "src"
            , "app" ]
    in  doctest (extensions ++ sourceFolders)
