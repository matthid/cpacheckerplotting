// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Cpacheckerplotting

module Seq =    
    open System.Collections.Generic
    let fromEnum (input : 'a IEnumerator) = 
        seq {
           while input.MoveNext() do
               yield input.Current
        }

    let getMore (input : 'a IEnumerator) = 
        if input.MoveNext() = false then None
        else Some ((input |> fromEnum) |> Seq.append [input.Current])
        
    let splitBy (f : 'a -> bool) (input : 'a seq)  = 
       use s = input.GetEnumerator()
       let rec loop (acc : 'a seq seq) = 
           match s |> getMore with 
           | None -> acc
           | Some x ->[x |> Seq.takeWhile (f >> not) |> Seq.toList |> List.toSeq]
                       |> Seq.append acc
                       |> loop
       loop Seq.empty
    let splitIn items (input: 'a seq) =
       use s = input.GetEnumerator()
       let rec loop (acc : 'a seq seq) = 
           match s |> getMore with 
           | None -> acc
           | Some x ->[x |> Seq.take items |> Seq.toList |> List.toSeq]
                       |> Seq.append acc
                       |> loop
       loop Seq.empty

open System.IO
module CpaBenchmark =    
    let splitLine (line:string) = 
        line.Split('\t', ',')
    let parseLine parser (line:string) = 
        let splits = splitLine line
        [|
            for (s, p) in Seq.zip splits parser do
                yield p s
        |]
    let parseNaN f s =
        if s = "-" then
            System.Double.NaN
        else
            f s
    let parseSecounds (s:string) = 
        let raw = 
            if s.EndsWith "s" then
                s.Substring(0, s.Length - 1)
            else
                s
        System.Double.Parse(raw, System.Globalization.CultureInfo.InvariantCulture)
        
    let parseMB (s:string) = 
        if not <| s.EndsWith "MB max" then
            failwith (sprintf "%s not a MB Line" s)
        let mbs = s.Substring(0, s.Length - 6)
        System.Double.Parse(mbs, System.Globalization.CultureInfo.InvariantCulture)
    let lift f i = 
        f i :> obj
        
    let idParser = lift id    
    let secoundsParser = parseSecounds |> parseNaN |> lift
    let mbParser = parseMB |> parseNaN |> lift
    let toDict s = 
        s 
        |> Seq.mapi (fun i item -> i,item)
        |> Map.ofSeq
    let DefaultParsers = 
        [ 
            "status", idParser
            "cputime", secoundsParser
            "walltime", secoundsParser
            "total", secoundsParser
            "cpa time", secoundsParser
            "memory", mbParser
            "totalmemory", mbParser
        ] |> Map.ofSeq
    let parseDataLine (parser:Map<string, string -> obj>) (columns:Map<int,string>) (line:string) =
        let parsers = [
            for i in 0.. columns.Count - 1 do                
                let colName =
                    match columns |> Map.tryFind i with
                    | None -> failwith (sprintf "No valid column %d" i)
                    | Some name -> name
                yield
                    match parser |> Map.tryFind colName with
                    | None -> failwith (sprintf "No parser found for column %s" colName)
                    | Some p -> p 
            ]
        parseLine parsers line
    // Benchmark : Map<string, Map<string, int>>
    let parseCsvSteam (parser:Map<string, string -> obj>) (stream:Stream) = 
        let reader = new StreamReader(stream)
        let readLine () = 
            reader.ReadLine() 
        let readAllLines () = 
            let rec loopReader () =
              seq {
                let line = readLine()
                if line <> null then
                    yield line
                    yield! loopReader() }
            loopReader()
        let toolHeader = readLine()
        let runSetLine = readLine()
        let columnLine = readLine()
        let simpleParse line = 
            parseLine (Seq.initInfinite (fun i -> id)) line
            |> toDict
        let sets = simpleParse runSetLine
        let columns = simpleParse columnLine
        sets,
        columns,
        readAllLines ()
        |> Seq.map (parseDataLine parser columns)







