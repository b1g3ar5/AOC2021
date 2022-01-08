module Covid where

import Utils
import Data.Monoid

type Deaths = (String, Int)
type Vax = (String, Double, Double)
type NoVax = (String, Double)


parseDeaths :: String -> Deaths
parseDeaths s
  | null ws = error $ "Error in parseDeaths: " ++ show s
  | otherwise = (unwords $ init ws, read $ last ws)
  where
    ws = words $ (\c -> if c=='\t' then ' ' else c) <$> s


parseVax :: String -> Vax
parseVax s
  | null ws = error $ show s
  | otherwise = (unwords $ init $ init ws, read $ init (last $ init ws), read $ init $ last ws)
  where
    ws = words $ (\c -> if c=='\t' then ' ' else c) <$> s



covidMain :: IO ()
covidMain = do
  deaths <- readFile "/home/nick/Documents/Spreadsheets/ExcessDeaths"
  vax <- readFile "/home/nick/Documents/Spreadsheets/VaccinationPercentage"
  let ds :: [Deaths]
      ds = parseDeaths <$> filter (\l -> l /="\t" && l /="") (lines deaths)
      vs :: [Vax]
      vs =  parseVax <$> filter (\l -> l /="\t\t" && l /="\t" && l /="") (lines vax)
      nvs = (\(c,v1,_) -> (c,100-v1)) <$> vs
      cs = (fst <$> ds) `intersect`  (fst <$> nvs)
      filteredNvs = sort $ filter ((`elem` cs) . fst) nvs
      filteredDs = sort $ filter ((`elem` cs) . fst) ds
      
      dat :: [(Double, Double)]
      dat = zip(snd <$> filteredNvs) (fromIntegral . snd <$> filteredDs)

  --putStrLn $ "Deaths: " ++ unlines (show <$>  ds)
  putStrLn $ "Vax: " ++ unlines (show <$> filteredDs)
  putStrLn $ "Vax: " ++ unlines (show <$> filteredNvs)
  putStrLn $ "Vax: " ++ show dat
  putStrLn $ "Correlation: " ++ show (correlation dat)
  putStrLn $ "Regression death ->novax: " ++ show (simpleLinearRegression dat)
  putStrLn $ "Regreesion novax -> death: " ++ show (simpleLinearRegression $ swap <$> dat)


  return ()


simpleLinearRegression :: Fractional a => [(a, a)] -> (a, a)
simpleLinearRegression points =
    (slope, intercept)
    where
    avg (x, y) = (Sum 1, Sum x, Sum y)

    reg (x, y) (avg_x, avg_y) =
        (Sum $ x' * y', Sum $ x' * x')
        where
        x' = x - avg_x
        y' = y - avg_y

    ((Sum n, Sum xs, Sum ys), getReg) =
        foldMap (\p -> (avg p, reg p)) points

    avg_x = xs / n
    avg_y = ys / n
    (Sum xys, Sum xxs) = getReg (avg_x, avg_y)
    slope = xys / xxs
    intercept = avg_y - slope * avg_x


correlation :: [(Double, Double)] -> Double
correlation ns =(xymean - xmean*ymean)/xstd/ystd
  where
    n = fromIntegral $ length ns
    xs, ys, xys :: [Double]
    (xs, ys) = unzip ns
    x2 = (\x -> x*x) <$> xs
    y2 = (\x -> x*x) <$> ys
    xys = zipWith (*) xs ys
    xmean = sum xs / n
    ymean = sum ys / n
    xymean = sum xys / n
    xstd = sqrt (sum x2 / n - xmean*xmean)
    ystd = sqrt (sum y2 / n - ymean*ymean)




