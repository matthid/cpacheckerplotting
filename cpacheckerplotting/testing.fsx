// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------

#silentCd "/home/reddragon/projects/cpacheckerplotting/cpacheckerplotting"
#load "CpaBenchmark.fs"
open Cpacheckerplotting
open System
open System.IO
open System.Collections.Generic
 
            
Environment.CurrentDirectory <- "/home/reddragon/mydata/Studium/Vorlesungen/Bachelor Arbeit"
//let file = "results.13-02-11_1521.table.csv"
//let file = "compareLegacy.table.csv"
let file = "compareSimpleLatest_Status.table.csv" 

let parser = 
    CpaBenchmark.DefaultParsers |> Map.add "test/programs/simple/" CpaBenchmark.idParser

let rawBenchmarkTable = CpaBenchmark.parseCsvFile parser file

let colorize isCorrect (s:string) = 
    match isCorrect with
    | Some correct ->
        let c = if correct then "green" else "red"
        sprintf "{\color{%s}%s}" c s
    | None -> s
        
let printer = 
    CpaBenchmark.DefaultPrinter 
        |> Map.add "test/programs/simple/" (CpaBenchmark.toStringPrinter |> CpaBenchmark.fromPrimitivePrinter)
        |> Map.remove "status"
        |> Map.add "status" 
            (fun c -> 
                let result = c.PrintItem |> CpaBenchmark.toStringPrinter                
                let lower = result.ToLowerInvariant()
                let fileName = CpaBenchmark.getFileNameFromLine c.Line
                let isUnsafe = fileName.Contains "_unsafe"
                let isSafe = fileName.Contains "_safe"
                let actualUnsafe = lower = "unsafe"
                let actualSafe = lower = "safe"
                let isCorrect =
                    if (actualUnsafe || actualSafe) && (isSafe || isUnsafe) then
                        let correct = (actualUnsafe && isUnsafe) || (actualSafe && isSafe)
                        Some correct
                    else 
                        None
                colorize isCorrect lower
                )
        |> Map.remove "header"
        |> Map.add "header" 
            (fun c -> 
                let result = c.PrintItem |> CpaBenchmark.toStringPrinter   
                result.Replace("legacyCasts", "legacy\\-Casts").Replace("fieldaliasing", "field\\-aliasing")
                )

let colLength = 
    rawBenchmarkTable.Columns.Count        
let cols = 
    Seq.init colLength (fun i -> @">{\centering\arraybackslash}p{2cm}")
    |> Seq.skip 1
    |> Seq.append ["p{8cm}"]
CpaBenchmark.splitTable 16 rawBenchmarkTable
    |> Seq.iteri (fun i table ->
        CpaBenchmark.printLatexTableFile 
            cols
            (sprintf "compareSimpleLatest_Status_%d.tex" i |> CpaBenchmark.openCreate) 
            printer 
            table)
            
CpaBenchmark.printLatexTableFile 
            cols
            (sprintf "compareSimpleLatest_Status.tex" |> CpaBenchmark.openCreate) 
            printer 
            rawBenchmarkTable

(*
let benchmarkTable = 
    let t =
        rawBenchmarkTable
        |> CpaBenchmark.filter
            (fun l -> not <| (CpaBenchmark.getFileNameFromLine l).Contains "/memsafety/")
    { t with
        Sets = 
            [
            yield (0, "run set")
            for i in 1..7 do yield (i, "oldimpl")
            for i in 1..7 do yield (i + 7, "newimpl")
            ] |> Map.ofSeq }
            
let rawFilePrefix = "results_legacy"

CpaBenchmark.writeStatisticData true printer rawFilePrefix benchmarkTable
CpaBenchmark.writeStatisticData false printer rawFilePrefix benchmarkTable

let userOverride = """set style line 1 lt 1 lw 1 pt 1 lc rgb "red"
set style line 2 lt 3 lw 1 pt 1 lc rgb "red"
set style line 3 lt 5 lw 1 pt 1 lc rgb "red"
set style line 4 lt 1 lw 1 pt 2 lc rgb "blue"
set style line 5 lt 3 lw 1 pt 2 lc rgb "blue"
set style line 6 lt 5 lw 1 pt 2 lc rgb "blue"
set style line 7 lt 1 lw 1 pt 1 lc rgb "green"
set style line 8 lt 3 lw 1 pt 1 lc rgb "green"
set style line 9 lt 5 lw 1 pt 1 lc rgb "green"
set key right bottom
"""

CpaBenchmark.writePlotFiles true rawFilePrefix userOverride benchmarkTable
CpaBenchmark.writePlotFiles false rawFilePrefix userOverride benchmarkTable
*)
"Finished"





    