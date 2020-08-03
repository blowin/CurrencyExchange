open System
open CurrencyExchange

[<EntryPoint>]
let main argv =
    let result = Currency.byDate
                     (DateTime.Today.Subtract(TimeSpan.FromDays(float 1)))
                     (SelectCurrency.Only([| CurrencyCode.USD; CurrencyCode.BYN |]))
    
    match result with
        | ValueSome converter ->
            let convertCurrency = converter.Convert CurrencyCode.USD CurrencyCode.BYN 100m
            printf "%A" convertCurrency
        | ValueNone -> ()
        
    0
