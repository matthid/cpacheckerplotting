// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Cpacheckerplotting
open System

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
       
    let tryTake n seq =
        Seq.append (seq |> Seq.map Some) (Seq.initInfinite (fun i -> None))
            |> Seq.take n
            |> Seq.filter Option.isSome
            |> Seq.map Option.get
    let trySplitIn items (input: 'a seq) =
       use s = input.GetEnumerator()
       let rec loop (acc : 'a seq seq) = 
           match s |> getMore with 
           | None -> acc
           | Some x ->[x |> tryTake items |> Seq.toList |> List.toSeq]
                       |> Seq.append acc
                       |> loop
       loop Seq.empty   
            
    let combinedEnumerable (input : 'a IEnumerable list) = 
        { new IEnumerable<'a list> with
            member x.GetEnumerator() = 
                let enumerators = input |> List.map (fun e -> e.GetEnumerator())
                { new IEnumerator<'a list> with
                    member enum.Current 
                        with get() =
                            enumerators 
                                |> List.map (fun e -> e.Current)
                    member enum.Dispose() = 
                        enumerators 
                            |> List.iter (fun e -> e.Dispose())
                   interface System.Collections.IEnumerator with
                        member enum.Current
                            with get() =
                                (enum :?> IEnumerator<'a list>).Current :> obj
                        member enum.MoveNext() =
                            enumerators 
                                |> Seq.map (fun e -> e.MoveNext())
                                |> Seq.reduce (&&)
                        member enum.Reset() = 
                            enumerators 
                                |> List.iter (fun e -> e.Reset())
                }
          interface System.Collections.IEnumerable with
                member x.GetEnumerator() = (x :?> IEnumerable<'a list>).GetEnumerator() :> System.Collections.IEnumerator
        }
        
    let zipN (listOfSeq:'a seq list) =
        combinedEnumerable listOfSeq : 'a list seq
        

open System.IO
module CpaBenchmark =    
    let splitLine (line:string) = 
        line.Split('\t')
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
    let parseFloat (s:string) = 
        System.Double.Parse(s, System.Globalization.CultureInfo.InvariantCulture)
    let parseSecounds (s:string) = 
        if not <| s.EndsWith "s" then
            failwithf "%s not a secound Line" s
        s.Substring(0, s.Length - 1) |> parseFloat
        
    let parseMB (s:string) = 
        if not <| s.EndsWith "MB max" then
            failwith (sprintf "%s not a MB Line" s)
        s.Substring(0, s.Length - 6) |> parseFloat
        
    let lift f i = 
        f i :> IComparable
        
    let idParser = lift id    
    let secoundsParser = parseSecounds |> parseNaN |> lift
    let floatParser = parseFloat |> parseNaN |> lift
    let mbParser = parseMB |> parseNaN |> lift
    let toDict s = 
        s 
        |> Seq.mapi (fun i item -> i,item)
        |> Map.ofSeq
    let DefaultParsers = 
        [ 
            "status", idParser
            "cputime", floatParser
            "walltime", floatParser
            "total", secoundsParser
            "cpa time", secoundsParser
            "memory", mbParser
            "totalmemory", mbParser
        ] |> Map.ofSeq
        
    let getItemOverColumn (columns:Map<int,string>) (items:Map<string,'a>) key = 
        let colName = 
            match columns |> Map.tryFind key with
            | None -> failwithf "Column %d could not be found" key
            | Some col -> col
        match items |> Map.tryFind colName with
        | None -> failwithf "Item for column %s was not found" colName
        | Some p -> p
    let parseDataLine (parser:Map<string, string -> IComparable>) (columns:Map<int,string>) (line:string) =
        let parsers = [
            for i in 0.. columns.Count - 1 do           
                yield getItemOverColumn columns parser i
            ]
        parseLine parsers line
        
    type BenchmarkTable = {
        Sets : Map<int, string>
        Columns : Map<int, string>
        Lines : IComparable array seq
    }
    
    let filter f table = { 
          table with
            Lines = table.Lines |> Seq.filter f |> Seq.cache
        } 
        
    let parseCsvSteam (parser:Map<string, string -> IComparable>) (stream:Stream) = 
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
        {
            Sets = sets
            Columns = columns
            Lines =
                readAllLines ()
                |> Seq.map (parseDataLine parser columns)
                |> Seq.toList
                |> Seq.ofList
        }
        
    let parseCsvFile (parser:Map<string, string -> IComparable>) (file:string) = 
        use fileStream = File.OpenRead file
        parseCsvSteam parser fileStream
        
    let sortColumn i benchmarkTable = 
        { benchmarkTable with
            Lines = 
                benchmarkTable.Lines
                |> Seq.sortBy (fun line -> Array.get line i) }
       
    let splitTable maxItems (benchmarkTable:BenchmarkTable) = 
        benchmarkTable.Lines
            |> Seq.trySplitIn maxItems
            |> Seq.map 
                (fun partLines ->
                    { benchmarkTable with Lines = partLines })
   
    type PrintContext = {
        Line : IComparable array
        PrintItem : IComparable
        Index : int }
              
    type Printer = PrintContext -> String                                      
    let toStringPrinter (compareable:IComparable) = compareable.ToString()
    let floatPrinter (compareable:IComparable) = (compareable :?> float).ToString(System.Globalization.CultureInfo.InvariantCulture)
    let makeReplacePrinter (old:string) rep printer s = 
        let res = printer s : string
        res.Replace(old, rep)
        
    let makeCsvPrinter printer = 
        printer
            |> makeReplacePrinter " " "_"
            |> makeReplacePrinter "\t" "_"
         
    let makeLatexTablePrinter printer = 
        printer
            |> makeReplacePrinter "&" "_"
            |> makeReplacePrinter "\\\\" "_"
            |> makeReplacePrinter "_" "\-{\\textunderscore}\-"
            |> makeReplacePrinter "/" "/\-"
    let mapPrinters printermap map = 
        map 
            |> Map.map (fun k v -> printermap v)
            
    let fromPrimitivePrinter prim context = 
        prim context.PrintItem
    let DefaultPrinter : Map<string, Printer> = 
        [ 
            "header", toStringPrinter
            "status", toStringPrinter
            "cputime", floatPrinter
            "walltime", floatPrinter
            "total", floatPrinter
            "cpa time", floatPrinter
            "memory", floatPrinter
            "totalmemory", floatPrinter
        ] |> Map.ofSeq
          |> mapPrinters fromPrimitivePrinter
    let mapToPrintContext line = 
        line
            |> Seq.mapi (fun i item -> {Line = line; PrintItem = item; Index = i})
    let formatLine joinLine (printer:Map<string,Printer>) (columns:Map<int,string>) (line:IComparable array) : string= 
        line
            |> mapToPrintContext
            |> Seq.mapi (getItemOverColumn columns printer)
            |> joinLine
        //String.Join("\t", formatedItems)
        
    let openCreate filePath = 
        File.Open(filePath, FileMode.Create)
        
    let getWriter (file:Stream) = 
        new StreamWriter(file)
        
    let printRawFile joinLine headerJoin writeLine (printer:Map<string,Printer>) (table:BenchmarkTable) =        
        let mapToStrings map = 
            map 
                |> Map.toSeq
                |> Seq.sortBy fst
                |> Seq.map snd
                |> Seq.cast<IComparable>
                |> Seq.toArray
                |> mapToPrintContext
                |> Seq.map (printer |> Map.find "header")
        let sets = table.Sets |> mapToStrings
        let setLine = headerJoin sets
        writeLine setLine
        
        let columns = table.Columns |> mapToStrings
        let columnLine = headerJoin columns       
        writeLine columnLine
        
        table.Lines
            |> Seq.map (formatLine joinLine printer table.Columns)
            |> Seq.iter writeLine
    let getLineWriter (writer:StreamWriter) =
        let writeLine (line:string) = 
            writer.WriteLine( line)
        writeLine
    let printCvsFile dataFile (printer:Map<string,Printer>) (table:BenchmarkTable) = 
        use writer = getWriter dataFile 
        printRawFile 
            (fun formatedItems -> String.Join("\t", formatedItems)) 
            (fun set -> "# " + String.Join("\t", set)) 
            (writer |> getLineWriter) 
            (printer |> mapPrinters makeCsvPrinter)
            table
    let printLatexTableFile (cols:string seq) dataFile (printer:Map<string,Printer>) (table:BenchmarkTable) = 
        use writer = getWriter dataFile        
        let writeLine = getLineWriter writer   
        let colDefs = ("| " + String.Join(" | ", cols) + " |")
        writeLine (sprintf "\\begin{tabular}{%s}" colDefs)
        writeLine "\\hline"
        printRawFile 
            (fun formatedItems -> "\t" + String.Join(" & ", formatedItems) + " \\\\ \\hline") 
            (fun set -> "\t" + String.Join(" & ", set) + " \\\\ \\hline") 
            writeLine 
            (printer |> mapPrinters makeLatexTablePrinter)
            table
        
        writeLine "\\end{tabular}"
        
    let getFileNameFromLine (line:IComparable array) = line.[0]:?>string    
    let isValidOnCol i (line:IComparable array) =
        let dataItem = Array.get line i :?> float
        dataItem |> System.Double.IsNaN |> not
    let getExpectedResult (line:IComparable array) =
        let fileName = getFileNameFromLine line
        if fileName.Contains "_safe" then
            Some true
        else if fileName.Contains "_unsafe" then
            Some false
        else None
    let isCorrectOnCol (columns:Map<int,string>) i (line:IComparable array) =
        let currentStatusCol = 
            columns
                |> Map.toSeq
                |> Seq.filter (fun (key, value) -> key < i)
                |> Seq.filter (fun (key, value) -> value = "status")
                |> Seq.map (fun (key, value) -> key)
                |> Seq.last
        let dataItem = Array.get line i :?> float
        let status =Array.get line currentStatusCol :?> string
        let expected = getExpectedResult line
        match expected with
        | None -> false
        | Some exp ->
            (exp && status = "SAFE") || (not exp && status = "UNSAFE")
        
    let writeStatisticData onlyCorrect printer rawFilePrefix (benchmarkTable:BenchmarkTable) = 
        for (i, set) in benchmarkTable.Sets |> Map.toSeq do
            if i = 0 then ()
            else
            let col = 
                match benchmarkTable.Columns |> Map.tryFind i with
                | None -> failwithf "Column %d was not found" i
                | Some c -> c
            if col = "status" then ()
            else
            
            use fileToWrite = 
                (if onlyCorrect then sprintf "%s_correct_%s_%s.csv" else sprintf "%s_%s_%s.csv") rawFilePrefix set col
                |> openCreate
            benchmarkTable
                |> filter (isValidOnCol i)
                |> filter (if onlyCorrect then isCorrectOnCol benchmarkTable.Columns i else (fun _ -> true))
                |> sortColumn i 
                |> printCvsFile fileToWrite printer
        
            
    let template = """# Template
set term epslatex
set output "{0}"

set logscale y

set xlabel '{3}'
set ylabel '{4}'

# User Override
{1}
# Plot
plot {2}"""
    
    let latexClean (s:string) = 
        s.Replace ("_", "{\\textunderscore}")
        
    open System.Diagnostics
    
    let executeGnuPlot (file:string) = 
        let proc = Process.Start("gnuplot", sprintf "\"%s\"" file)
        proc.WaitForExit()
        if proc.ExitCode <> 0 then
            failwithf "GnuPlot exited with %d" proc.ExitCode
        
    let writePlotFiles onlyCorrect rawFilePrefix userOverride (benchmarkTable:BenchmarkTable) =
        // two benchmarks for each column!
        for v in benchmarkTable.Columns |> Map.toSeq |> Seq.map snd |> Seq.distinct do    
            // write gnuplot file
            let plotBaseName = 
                let raw = (if onlyCorrect then sprintf "%s_plot_%s_correct" else sprintf "%s_plot_%s") rawFilePrefix v
                raw.Replace(" ", "_") // Because latex fails to import files with spaces
            let plotFile = sprintf "%s.plot" plotBaseName 
            let epsFile = sprintf "%s.eps" plotBaseName 
            let cols =
                benchmarkTable.Columns |> Map.toSeq |> Seq.filter (fun (k, value) -> value = v) |> Seq.map fst
            let firstLine = benchmarkTable.Lines |> Seq.head
            let isValidColumn = 
                cols
                    |> Seq.map (fun index -> Array.get firstLine index)
                    |> Seq.map (fun item -> match item with | :? float -> true | _ -> false)
                    |> Seq.reduce (&&)
            if not isValidColumn then ()
            else
            let plotArgs = 
                cols 
                    |> Seq.map (fun colIndex -> colIndex, benchmarkTable.Sets |> Map.find colIndex)
                    |> Seq.map 
                        (fun (colIndex, runset) -> 
                            colIndex, 
                            runset, 
                            (if onlyCorrect then sprintf "%s_correct_%s_%s.csv" else sprintf "%s_%s_%s.csv") rawFilePrefix runset v)
                    |> Seq.mapi (fun i (colIndex, runset, file) -> sprintf "'%s' using :%d t '%s' w l ls %d" file (colIndex+1) (latexClean runset) (i+1))
            let args = String.Join(sprintf ", \\%s     " Environment.NewLine, plotArgs)
            let xLabel = sprintf "%s sorted by %s" (if onlyCorrect then "Correct results" else "Results") v
            let yLabel = "Time in seconds"
            let contents = String.Format (template, epsFile, userOverride, args, xLabel, yLabel)
            use file = File.Open(plotFile, FileMode.Create)
            use writer = new StreamWriter(file)
            writer.WriteLine (contents)
            writer.Close()
            file.Close()
            executeGnuPlot plotFile
