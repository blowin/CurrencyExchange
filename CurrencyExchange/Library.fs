namespace CurrencyExchange

open System
open System.Collections.Generic
open FSharp.Data
open Microsoft.FSharp.Reflection

type CurrencyName = string

type CurrencyCode =
    | AED
    | AFN
    | ALL
    | AMD
    | ANG
    | AOA
    | ARS
    | AUD
    | AWG
    | AZN
    | BAM
    | BBD
    | BDT
    | BGN
    | BHD
    | BIF
    | BMD
    | BND
    | BOB
    | BRL
    | BSD
    | BTC
    | BTN
    | BWP
    | BYN
    | BZD
    | CAD
    | CDF
    | CHF
    | CLF
    | CLP
    | CNH
    | CNY
    | COP
    | CRC
    | CUC
    | CUP
    | CVE
    | CZK
    | DJF
    | DKK
    | DOP
    | DZD
    | EGP
    | ERN
    | ETB
    | EUR
    | FJD
    | FKP
    | GBP
    | GEL
    | GGP
    | GHS
    | GIP
    | GMD
    | GNF
    | GTQ
    | GYD
    | HKD
    | HNL
    | HRK
    | HTG
    | HUF
    | IDR
    | ILS
    | IMP
    | INR
    | IQD
    | IRR
    | ISK
    | JEP
    | JMD
    | JOD
    | JPY
    | KES
    | KGS
    | KHR
    | KMF
    | KPW
    | KRW
    | KWD
    | KYD
    | KZT
    | LAK
    | LBP
    | LKR
    | LRD
    | LSL
    | LYD
    | MAD
    | MDL
    | MGA
    | MKD
    | MMK
    | MNT
    | MOP
    | MRO
    | MRU
    | MUR
    | MVR
    | MWK
    | MXN
    | MYR
    | MZN
    | NAD
    | NGN
    | NIO
    | NOK
    | NPR
    | NZD
    | OMR
    | PAB
    | PEN
    | PGK
    | PHP
    | PKR
    | PLN
    | PYG
    | QAR
    | RON
    | RSD
    | RUB
    | RWF
    | SAR
    | SBD
    | SCR
    | SDG
    | SEK
    | SGD
    | SHP
    | SLL
    | SOS
    | SRD
    | SSP
    | STD
    | STN
    | SVC
    | SYP
    | SZL
    | THB
    | TJS
    | TMT
    | TND
    | TOP
    | TRY
    | TTD
    | TWD
    | TZS
    | UAH
    | UGX
    | USD
    | UYU
    | UZS
    | VEF
    | VES
    | VND
    | VUV
    | WST
    | XAF
    | XAG
    | XAU
    | XCD
    | XDR
    | XOF
    | XPD
    | XPF
    | XPT
    | YER
    | ZAR
    | ZMW
    | ZWL

[<Struct>]
type CurrencyInfo = {
    code: CurrencyCode
    description: string
}

[<Struct>]
type CurrencyRate = {
    code: CurrencyCode
    rate: decimal
}

module private Types =
        type CurrencyInfoApi = JsonProvider<"https://api.exchangerate.host/symbols">
        type CurrencyByDateApi = JsonProvider<"https://api.exchangerate.host/2020-08-03">
        type CurrencyByDateRangeApi = JsonProvider<"https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04">
        type public CurrencyRateStore = Dictionary<CurrencyCode, decimal>
        
        let CurrencyCodeMap =
            let map = Dictionary<CurrencyName, CurrencyCode>()
            let currencyCodeType = typeof<CurrencyCode>
            FSharpType.GetUnionCases(currencyCodeType)
                |> Array.iter (fun x ->
                        let currencyCode = (currencyCodeType.GetProperty(x.Name).GetValue(null) :?> CurrencyCode)
                        map.Add(x.Name, currencyCode)
                    )
            map

type CurrencyConverter(baseCurrency: CurrencyRate, rates: Types.CurrencyRateStore, date: DateTime) =
    member this.Date = date
    member this.Rates = rates |> Seq.map (fun x -> {code = x.Key; rate = x.Value})
    member this.CurrencyList = rates |> Seq.map (fun x -> x.Key)
        
    member this.Convert (currencyFrom: CurrencyCode) (currencyTo: CurrencyCode) sum =
        let convertCore (sum: decimal) rateTo rateFrom = Math.Round(sum * rateTo / rateFrom, 2)
        
        let (toFound, toRate) = rates.TryGetValue(currencyTo)
        if toFound then 
            if currencyFrom = baseCurrency.code then
                ValueSome (convertCore sum toRate 1m)
            else
                let (found, value) = rates.TryGetValue(currencyFrom)
                if found then
                    ValueSome (convertCore sum toRate value)
                else
                    ValueNone
        else
            ValueNone
        
module Currency =        
    module private Factory =        
        let makeRateStore baseCurrency date (exchangeRates: struct(CurrencyName * decimal) seq) =
            let rates = Types.CurrencyRateStore()
           
            let (isExtract, baseCurrencyCode) = Types.CurrencyCodeMap.TryGetValue(baseCurrency)
            if isExtract then
                exchangeRates |> Seq.iter (fun struct(name, rate) ->
                    let code = Types.CurrencyCodeMap.GetValueOrDefault(name)
                    rates.Add(code, rate))
                        
                ValueSome (CurrencyConverter({ code = baseCurrencyCode; rate = 1m }, rates, date))
            else
                ValueNone
            
        let convertJsonToExchangeRateSeq (jsonValue: JsonValue) =
            (jsonValue.Properties()
                |> Seq.map (
                        fun (name, value) -> struct(name, (value.AsDecimal()))
                    )
               )
    
    let allCurrency =
        let parseInfo = Types.CurrencyInfoApi.GetSample()
        if parseInfo.Success then
            let result = (parseInfo.Symbols.JsonValue.Properties()
                    |> Seq.map (fun (strCode, obj) ->
                            match obj.TryGetProperty("description") with
                                | Some description ->
                                    let (isOk, code) = Types.CurrencyCodeMap.TryGetValue(strCode)
                                    if isOk then 
                                        ValueSome { code = code; description = description.AsString() }
                                    else
                                        ValueNone
                                | None -> ValueNone
                        )
                    |> Seq.filter (fun x -> x.IsSome)
                    |> Seq.map (fun x -> x.Value)
                    |> Seq.toArray
                )
            
            ValueSome result
        else
            ValueNone
    
    let between (dateFrom: DateTime) (dateTo: DateTime) =
        let url = sprintf "https://api.exchangerate.host/timeseries?start_date=%s&end_date=%s" (dateFrom.ToString "yyyy-MM-dd") (dateTo.ToString "yyyy-MM-dd")
        let info = Types.CurrencyByDateRangeApi.Load(url)
        
        if info.Success then
            let res = (info.Rates.JsonValue.Properties()
                |> Seq.map (fun (name, prop) ->
                        match name.Split('-') with
                            | [| year; month; day |] ->
                                let date = DateTime(Int32.Parse(year), Int32.Parse(month), Int32.Parse(day))
                                let exchangeRateSeq = Factory.convertJsonToExchangeRateSeq prop
                                Factory.makeRateStore info.Base date exchangeRateSeq
                            | _ ->
                                ValueNone
                    )
                  |> Seq.filter (fun x -> x.IsSome)
                  |> Seq.map (fun x -> x.Value)
                  |> Seq.toArray)
            
            ValueSome res
        else
            ValueNone
    
    let byDate (date: DateTime) =
        let url = sprintf "https://api.exchangerate.host/%s" (date.ToString "yyyy-MM-dd")
        let info = Types.CurrencyByDateApi.Load(url)
        
        if info.Success then
            let exchangeRateSeq = Factory.convertJsonToExchangeRateSeq info.Rates.JsonValue
            Factory.makeRateStore info.Base info.Date exchangeRateSeq
        else
            ValueNone
        
