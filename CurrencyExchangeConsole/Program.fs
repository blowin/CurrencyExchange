open System
open CurrencyExchange

[<EntryPoint>]
let main argv =
    let date = DateTime.Today - TimeSpan.FromDays 1.0
    
    let fluctuation = Currency.fluctuationBetweenWithAllCurrency date DateTime.Today
    
    let selectCurrency = (
        seq {
            yield CurrencyCode.BYN
            yield CurrencyCode.USD
        } |> SelectCurrency.Only
    )
        
    let result = Currency.byDate selectCurrency date
        
    match result with
        | ValueSome converter ->
            let convertCurrency = converter.Convert CurrencyCode.USD CurrencyCode.BYN 100m
            printf "%A" convertCurrency
        | ValueNone -> ()
        
    0
