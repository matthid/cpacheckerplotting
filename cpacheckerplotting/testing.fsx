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
let file = File.OpenRead("results.13-02-11_1521.diff.csv")

let parser = 
    CpaBenchmark.DefaultParsers |> Map.add "test/programs/" CpaBenchmark.idParser
    
type Cat = {
    Status :string
    CpuTime : float
    WallTime : float
    TotalTime : float
    CpaTime : float
    Memory : float
    TotalMemory : float }
let fromAr (ar:obj array) = 
    {
        Status = ar.[0] :?> string
        CpuTime = ar.[1] :?> float
        WallTime = ar.[2] :?> float
        TotalTime = ar.[3] :?> float
        CpaTime = ar.[4] :?> float
        Memory = ar.[5] :?> float
        TotalMemory = ar.[6] :?> float
    }
type Setups = {
    Legacy : Cat
    Field : Cat
    FieldAliasing : Cat }
let fromSeq cats = 
    let c = cats|> Seq.toArray
    {
        Legacy = c.[0]
        Field = c.[1]
        FieldAliasing = c.[2]
    }
    
let fromRawSeq s = 
    s
        |> Seq.splitIn 7
        |> Seq.map Seq.toArray
        |> Seq.map fromAr
        |> fromSeq
        
type Benchmark = {
    File : string
    Bv : Setups
    Legacy : Setups
    LegacyCasts : Setups }
    
let fromRawArray (raw:obj array) = 
    let length = 7*3
    {
        File = raw.[0] :?> string
        Bv = raw |> Seq.skip 1 |> Seq.take length |> fromRawSeq
        Legacy = raw |> Seq.skip (1 + length) |> Seq.take length |> fromRawSeq
        LegacyCasts = raw |> Seq.skip (1 + 2*length) |> Seq.take length|> fromRawSeq
    }

let benchmarkFiles = 
    let sets, columns, data = CpaBenchmark.parseCsvSteam parser file
    data
    |> Seq.map fromRawArray
    |> Seq.toList

    
