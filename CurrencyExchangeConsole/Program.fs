open System
open CurrencyExchange

[<EntryPoint>]
let main argv =
    let result = Currency.byDate DateTime.Today
    match result with
        | ValueSome converter ->
            let convertCurrency = converter.Convert CurrencyCode.USD CurrencyCode.BYN 100m
            printf "%A" convertCurrency
        | ValueNone -> ()
        
    0
