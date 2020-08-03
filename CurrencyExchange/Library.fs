namespace CurrencyExchange

open System
open System.Collections.Generic
open FSharp.Data

type CurrencyApi = JsonProvider<"https://api.exchangerate.host/2020-08-03">

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
    let byDate (date: DateTime) =
        let url = sprintf "https://api.exchangerate.host/%s" (date.ToString "yyyy-MM-dd")
        let info = CurrencyApi.Load(url)
        
        if info.Success then
            let rates = CurrencyRateStore()
            info.Rates.JsonValue.Properties() |>
                Array.iter (fun x ->
                    let (name, value) = x                    
                    rates.Add(name, value.AsDecimal()))
                    
            ValueSome (
                          CurrencyConverter(
                                               { name = info.Base; rate = 1m },
                                               rates,
                                               info.Date
                                           )
                      )
        else
            ValueNone
        
