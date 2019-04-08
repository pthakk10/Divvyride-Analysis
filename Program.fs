//
// F# program to analyze Divvy daily ride data.
//
// << Prachi Thakkar >>
// U. of Illinois, Chicago
// CS 341, Spring 2019
// Project #04
//

#light

module project04

//
// ParseLine and ParseInput
//
// Given a sequence of strings representing Divvy data, 
// parses the strings and returns a list of lists.  Each
// sub-list denotes one bike ride.  Example:
//
//   [ [176;74;1252;21;595;1986;1]; ... ]
//
// The values are station id (from), station id (to), bike
// id, starting hour (0..23), trip duration (secs), birth
// year (0=>not specified), and gender (0=>not specified, 
// 1=>identifies as male, 2=>identifies as female).
//
//
let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints

let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides
 
//this fucntion will take average age
let printAvgAge (entries: int list list) =
  let getYear (e: int list) = e.Item(5)
  let nonZero i = i <> 0
  let nowYear = System.DateTime.Now.Year
  let ages =
    entries |>
    List.map getYear |>
    List.filter nonZero |>
    List.map (fun year -> nowYear - year)
  let avgAge =
    if (List.isEmpty ages) then 0.0 else List.averageBy float ages
  printfn "Average age: %A" avgAge

let countBy projection (entries: int list list) =
  List.countBy projection entries |> Map.ofList

let findOrDefault key def map =
  Map.tryFind key map |> Option.defaultValue def
  
//this will print percentage
let printPercents (entries: int list list) projection pairs =
  let total = entries.Length
  let percent count = float count / float total * 100.0
  let counts = countBy projection entries
  let printPercent (key, string) =
    let count = findOrDefault key 0 counts
    printfn "%s: %A (%A%%)" string count (percent count)
  List.iter printPercent pairs

//this will print male and female count
let printGenders (entries: int list list) =
  let getGender (e: int list) =
    e.Item(6)
  let strings = [
    1, "% of riders identifying as male";
    2, "% of riders identifying as female"
  ]
  printPercents entries getGender strings


//this will print the duration time for different mins separetly.
let printDurations (entries: int list list) =
  let getCategory (e: int list) =
    match e.Item(4) with
    | s when 0 < s && s <= 30 * 60 -> 1
    | s when 30 * 60 < s && s <= 60 * 60 -> 2
    | s when 60 * 60 < s && s <= 120 * 60 -> 3
    | s when 120 * 60 < s && s <= 24 * 60 * 60 -> 4
    | _ -> -1
  let strings = [
    1, " 0..30 mins";
    2, " 30..60 mins";
    3, " 60..120 mins";
    4, " > 2 hours"
  ]
  printPercents entries getCategory strings

//this will print histogram with the time duration
let printHours (entries: int list list) =
  let getHour (e: int list) = e.Item(3)
  let counts = countBy getHour entries
  let printHour hour =
    let count = findOrDefault hour 0 counts
    printfn " %A: %s%A" hour (String.replicate (count / 10) "*") count
  List.iter printHour [0..23]


     
[<EntryPoint>]
let main argv =
  //
  // input file name, then input divvy ride data and build
  // a list of lists:
  //
  printf "filename> "
  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents
 

  //printfn "%A" ridedata
  let N = List.length ridedata
  printfn ""
  printfn "# of riders: %A" N
  printfn ""
  
  printGenders ridedata
  printfn ""

  printAvgAge ridedata
  printfn ""

  printfn "** Ride Durations:"
  printDurations ridedata
  printfn ""

  printfn "** Ride Start Time Histogram:"
  printHours ridedata


  
  0 
