namespace CurrencyExchange

open System
open System.Collections.Generic
open System.Text
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
type SelectCurrency =
    | All
    | Only of CurrencyCode seq

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

type CurrencyFluctuationRate = {
    code: CurrencyCode
    change: decimal
    changePct: decimal
    startRate: decimal
    endRate: decimal
}

type CurrencyFluctuation = {
    startDate: DateTime
    endDate: DateTime
    rates: CurrencyFluctuationRate array
}

module private Api =
    type CurrencyConvert = JsonProvider<"https://api.exchangerate.host/convert?from=USD&to=BYN&date=2020-08-04&places=2">
    type CurrencyInfo = JsonProvider<"https://api.exchangerate.host/symbols">
    type CurrencyByDate = JsonProvider<"https://api.exchangerate.host/2020-08-03">
    type CurrencyByDateRange = JsonProvider<"https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04">
    type FluctuationByDateRange = JsonProvider<"https://api.exchangerate.host/fluctuation?start_date=2020-01-01&end_date=2020-01-04">

module private Types =
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
                    let (ok, code) = Types.CurrencyCodeMap.TryGetValue(name)
                    
                    if ok then                        
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
    
    let private dateTimeAsString (date: DateTime) = date.ToString("yyyy-MM-dd")
    
    let private AsString obj = obj.ToString()
    
    let private appendCurrencyInfo (currencyInfo: SelectCurrency) url =
        
        match currencyInfo with
            | All -> url
            | Only arr ->
                if arr = null then url
                else
                    let builder = StringBuilder(value = url)
                    arr |> Seq.iteri (fun index obj ->
                            if index = 0 then builder.Append("?symbols=").Append(obj) |> ignore
                            else builder.Append(',').Append(obj) |> ignore
                                
                        )
                    builder |> AsString
    
    let allCurrencyInfo =
        let parseInfo = Api.CurrencyInfo.GetSample()
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
    
    let convertRate (fromCurrency: CurrencyCode) (toCurrency: CurrencyCode) (date: DateTime) =
        let url = (sprintf "https://api.exchangerate.host/convert?from=%s&to=%s&date=%s&places=2"
                    (fromCurrency |> AsString)
                    (toCurrency |> AsString)
                    (date |> dateTimeAsString) 
                  )
        
        let info = Api.CurrencyConvert.Load(url)
        
        if info.Success then ValueSome info.Info.Rate else ValueNone
    
    let fluctuationBetween (selectCurrency: SelectCurrency) (dateFrom: DateTime) (dateTo: DateTime) =
        let url = (sprintf "https://api.exchangerate.host/fluctuation?start_date=%s&end_date=%s"
                       (dateFrom |> dateTimeAsString)
                       (dateTo |> dateTimeAsString)
                  ) |> appendCurrencyInfo selectCurrency
        
        let info = Api.FluctuationByDateRange.Load(url)
        
        if info.Success then
            
            let asCurrencyFluctuation currency (jsonObject: JsonValue) =
                let (ok, code) = Types.CurrencyCodeMap.TryGetValue(currency)
                if ok then
                    ValueSome
                        {
                            code = code; 
                            change = jsonObject.GetProperty("change").AsDecimal(); 
                            changePct = jsonObject.GetProperty("change_pct").AsDecimal(); 
                            startRate = jsonObject.GetProperty("start_rate").AsDecimal(); 
                            endRate = jsonObject.GetProperty("end_rate").AsDecimal()
                        }
                else
                    ValueNone
            
            ValueSome ({
                           startDate = info.StartDate
                           endDate = info.EndDate
                           rates = info.Rates.JsonValue.Properties()
                                        |> Seq.map(fun (currencyName, obj) -> asCurrencyFluctuation currencyName obj)
                                        |> Seq.filter (fun x -> x.IsSome)
                                        |> Seq.map (fun x -> x.Value)
                                        |> Seq.toArray
                       })
        else
            ValueNone
    
    let fluctuationBetweenWithAllCurrency = fluctuationBetween (SelectCurrency.All)
    
    let between (selectCurrency: SelectCurrency) (dateFrom: DateTime) (dateTo: DateTime) =
        let url = (sprintf "https://api.exchangerate.host/timeseries?start_date=%s&end_date=%s"
                       (dateFrom |> dateTimeAsString)
                       (dateTo |> dateTimeAsString)
                  ) |> appendCurrencyInfo selectCurrency
                  
        let info = Api.CurrencyByDateRange.Load(url)
        
        let parseInt (str: string) =
            let (ok, result) = Int32.TryParse(str)
            if ok then ValueSome result else ValueNone
        
        if info.Success then
            let res = (info.Rates.JsonValue.Properties()
                |> Seq.map (fun (name, prop) ->
                        match name.Split('-') with
                            | [| year; month; day |] ->
                                match struct(parseInt year, parseInt month, parseInt day) with
                                    | struct(ValueSome year, ValueSome month, ValueSome day) ->
                                        let date = DateTime(year, month, day)
                                        let exchangeRateSeq = Factory.convertJsonToExchangeRateSeq prop
                                        
                                        Factory.makeRateStore info.Base date exchangeRateSeq
                                    | _ -> ValueNone
                            | _ ->
                                ValueNone
                    )
                  |> Seq.filter (fun x -> x.IsSome)
                  |> Seq.map (fun x -> x.Value)
                  |> Seq.toArray)
            
            ValueSome res
        else
            ValueNone
    
    let betweenWithAllCurrency = between SelectCurrency.All
    
    let byDate (selectCurrency: SelectCurrency) (date: DateTime) =
        let url = (sprintf "https://api.exchangerate.host/%s" (date |> dateTimeAsString)) |> appendCurrencyInfo selectCurrency
        let info = Api.CurrencyByDate.Load(url)
        
        if info.Success then
            let exchangeRateSeq = Factory.convertJsonToExchangeRateSeq info.Rates.JsonValue
            Factory.makeRateStore info.Base info.Date exchangeRateSeq
        else
            ValueNone
        
    let byDateWithAllCurrency = byDate SelectCurrency.All
    