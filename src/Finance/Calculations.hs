module Finance.Calculations (
    compoundInterest,
    annuityPayment,
    futureValue,
    breakEvenAnalysis
) where

-- Расчет сложного процента
compoundInterest :: Double -> Double -> Int -> Double
compoundInterest principal rate periods =
    principal * (1 + rate) ** fromIntegral periods

-- Расчет аннуитетного платежа
annuityPayment :: Double -> Double -> Int -> Double
annuityPayment loan rate periods =
    loan * rate / (1 - (1 + rate) ** (- fromIntegral periods))

-- Оценка будущей стоимости
futureValue :: Double -> Double -> Double -> Int -> Double
futureValue principal contribution rate periods =
    principal * (1 + rate) ** fromIntegral periods +
    contribution * (((1 + rate) ** fromIntegral periods - 1) / rate)

-- Расчет точки безубыточности
breakEvenAnalysis :: Double -> Double -> Double -> Double
breakEvenAnalysis fixedCosts price variableCosts =
    fixedCosts / (price - variableCosts)
