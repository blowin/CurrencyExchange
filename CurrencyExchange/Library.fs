namespace CurrencyExchange

open System
open System.Collections.Generic
open FSharp.Data

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

type CurrencyRateStore = Dictionary<CurrencyName, decimal>

[<Struct>]
type CurrencyRate = {
    name: CurrencyName
    rate: decimal
}

type CurrencyConverter(baseCurrency: CurrencyRate, rates: CurrencyRateStore, date: DateTime) =
    member this.BaseCurrency = baseCurrency.name
    member this.Date = date
    member this.Rates = rates |> Seq.map (fun x -> {name = x.Key; rate = x.Value})
    member this.CurrencyList = rates |> Seq.map (fun x -> x.Key)
        
    member this.Convert (currencyFrom: CurrencyCode) (currencyTo: CurrencyCode) sum =
        let convertCore (sum: decimal) rateTo rateFrom = Math.Round(sum * rateTo / rateFrom, 2)
        
        let (toFound, toRate) = rates.TryGetValue(currencyTo.ToString())
        if toFound then 
            if currencyFrom.ToString() = this.BaseCurrency then
                ValueSome (convertCore sum toRate 1m)
            else
                let (found, value) = rates.TryGetValue(currencyFrom.ToString())
                if found then
                    ValueSome (convertCore sum toRate value)
                else
                    ValueNone
        else
            ValueNone
        
module Currency =
    module private Types =
        type CurrencyByDateApi = JsonProvider<"https://api.exchangerate.host/2020-08-03">
        type CurrencyByDateRangeApi = JsonProvider<"https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04">
        
    module private Factory =
        let makeRateStore baseCurrency date (exchangeRates: struct(CurrencyName * decimal) seq) =
            let rates = CurrencyRateStore()
            exchangeRates |> Seq.iter (fun struct(name, rate) -> rates.Add(name, rate))
                    
            CurrencyConverter({ name = baseCurrency; rate = 1m }, rates, date)
            
        let convertJsonToExchangeRateSeq (jsonValue: JsonValue) =
            (jsonValue.Properties()
                |> Seq.map (
                        fun (name, value) -> struct(name, (value.AsDecimal()))
                    )
               )
    
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
                                ValueSome (Factory.makeRateStore info.Base date exchangeRateSeq)
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
            ValueSome (Factory.makeRateStore info.Base info.Date exchangeRateSeq)
        else
            ValueNone
        
