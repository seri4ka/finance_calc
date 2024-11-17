module Finance.IO (
    runCalculator
) where

import Finance.Calculations (compoundInterest, annuityPayment, futureValue, breakEvenAnalysis)

-- Основное меню калькулятора
runCalculator :: IO ()
runCalculator = do
    putStrLn "Выберите операцию:"
    putStrLn "1. Расчет сложного процента"
    putStrLn "2. Расчет аннуитетного платежа"
    putStrLn "3. Оценка будущей стоимости"
    putStrLn "4. Расчет точки безубыточности"
    putStrLn "5. Выход"
    choice <- getLine
    case choice of
        "1" -> calculateCompoundInterest
        "2" -> calculateAnnuityPayment
        "3" -> calculateFutureValue
        "4" -> calculateBreakEven
        "5" -> putStrLn "Выход из программы"
        _   -> do
            putStrLn "Неверный выбор, попробуйте снова."
            runCalculator

-- Ввод данных и расчет сложного процента
calculateCompoundInterest :: IO ()
calculateCompoundInterest = do
    putStrLn "Введите начальную сумму:"
    principal <- readLn
    putStrLn "Введите годовую процентную ставку (например, 0.05 для 5%):"
    rate <- readLn
    putStrLn "Введите количество периодов:"
    periods <- readLn
    let result = compoundInterest principal rate periods
    putStrLn $ "Итоговая сумма: " ++ show result
    runCalculator

-- Ввод данных и расчет аннуитетного платежа
calculateAnnuityPayment :: IO ()
calculateAnnuityPayment = do
    putStrLn "Введите сумму займа:"
    loan <- readLn
    putStrLn "Введите ставку за период (например, 0.05 для 5%):"
    rate <- readLn
    putStrLn "Введите количество периодов:"
    periods <- readLn
    let result = annuityPayment loan rate periods
    putStrLn $ "Аннуитетный платеж: " ++ show result
    runCalculator

-- Ввод данных и расчет будущей стоимости
calculateFutureValue :: IO ()
calculateFutureValue = do
    putStrLn "Введите начальную сумму:"
    principal <- readLn
    putStrLn "Введите регулярные вложения:"
    contribution <- readLn
    putStrLn "Введите процентную ставку (например, 0.05 для 5%):"
    rate <- readLn
    putStrLn "Введите количество периодов:"
    periods <- readLn
    let result = futureValue principal contribution rate periods
    putStrLn $ "Будущая стоимость: " ++ show result
    runCalculator

-- Ввод данных и расчет точки безубыточности
calculateBreakEven :: IO ()
calculateBreakEven = do
    putStrLn "Введите фиксированные издержки:"
    fixedCosts <- readLn
    putStrLn "Введите цену продажи за единицу:"
    price <- readLn
    putStrLn "Введите переменные издержки на единицу:"
    variableCosts <- readLn
    let result = breakEvenAnalysis fixedCosts price variableCosts
    putStrLn $ "Точка безубыточности: " ++ show result ++ " единиц"
    runCalculator
