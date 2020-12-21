module day21.IO

open System
open System.IO
open day21.BaseTypes

let readFile (filePath:String) = seq {
    use sr = new StreamReader(filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let parseLine (s:String) =
    let foodAndContains : String[] = s.Split ('(')
    let ingredients = foodAndContains.[0].Trim().Split ' '
    let containsString : String = foodAndContains.[1]
    let containsString = containsString.[8..containsString.Length-2]
    let contains = containsString.Split ','
                   |> Array.map (fun (item:String) -> item.Trim())
                   
    Food (ingredients,contains)
   
    

let readInput (filePath: String) : Food[] =
    let lines = readFile filePath
    let foods = lines |> Seq.map parseLine |> Seq.toArray 
    foods 