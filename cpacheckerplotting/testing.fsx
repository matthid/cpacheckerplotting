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
let file = File.OpenRead("results.13-02-11_1521.table.csv")

let parser = 
    CpaBenchmark.DefaultParsers |> Map.add "test/programs/" CpaBenchmark.idParser

let rawBenchmarkTable = CpaBenchmark.parseCsvSteam parser file

let printer = 
    CpaBenchmark.DefaultPrinter |> Map.add "test/programs/" CpaBenchmark.toStringPrinter

    
let benchmarkTable = 
    rawBenchmarkTable
    |> CpaBenchmark.filter
        (fun l -> not <| (CpaBenchmark.getFileNameFromLine l).Contains "/memsafety/")

let rawFilePrefix = "results_sorted"

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

"Finished"





    