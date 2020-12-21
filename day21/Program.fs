open System

open day21.BaseTypes
open day21.IO

let allIngredients (foods:Food[]):String[] =
        foods
        |> Seq.map (fun food -> food.Ingredients)
        |> Seq.concat
        |> Seq.distinct 
        |> Seq.toArray 

let allAllergenes (foods:Food[]):String[] =
        foods
        |> Seq.map (fun food -> food.Contains)
        |> Seq.concat
        |> Seq.distinct
        |> Seq.toArray

type AllergeneCanBeIn (name:String, ingredients: String[]) as self =
    override this.ToString () = sprintf "AllergeneCanBeIn(%A ingredients:%A" name ingredients
    member this.Name = name
    member this.Ingredients = ingredients
    member this.isInFood (food:Food) =
        food.Contains |> Seq.contains name 
    member this.filterIngredientsByFood (food: Food) =
        let ingredientIsInFood (s:String) = food.Ingredients |> Seq.contains s 
        if this.isInFood food then
            let ing = ingredients |> Array.filter ingredientIsInFood
            AllergeneCanBeIn(name, ing)
        else
            self
    member this.filterIngredientsByFoods (foods:Food[]) : AllergeneCanBeIn =
        let acc (a:AllergeneCanBeIn) (food:Food) = a.filterIngredientsByFood food
        foods |> Seq.fold acc self 

let task1 (foods:Food[]) : AllergeneCanBeIn[] =
    let ingredients = allIngredients foods
    let allergenes = allAllergenes foods
    let allergenes = allergenes |> Array.map (fun a -> AllergeneCanBeIn(a,ingredients))
    printfn "Allergenes: %A" allergenes 
    let allergenes = allergenes |> Array.map (fun (a:AllergeneCanBeIn) -> a.filterIngredientsByFoods foods)
    printfn "Allergenes: %A" allergenes
    let allergeneCandidates =
        allergenes
        |> Seq.map (fun (a:AllergeneCanBeIn) -> a.Ingredients)
        |> Seq.concat
        |> Seq.distinct
        |> Seq.toArray
    let safeIngredients =
        ingredients
        |> Seq.filter (fun (i:String) -> allergeneCandidates |> Seq.contains i |> not)
        |> Seq.toArray
    printfn "Safe: %A" safeIngredients   
    let safeOcc = 
        safeIngredients
        |> Seq.map (fun (safe:String) -> foods
                                         |> Seq.filter (fun (food:Food) -> food.Ingredients |> Seq.contains safe)
                                         |> Seq.length)
        |> Seq.sum
    printfn "Safe occurences: %A" safeOcc
    allergenes

    
type AllergeneState (allergenes:AllergeneCanBeIn[],map:Map<String,String>) as self =
    override this.ToString () = sprintf "AllergenState(%A found: %A" allergenes map
    member this.Resolved = allergenes.Length = 0
    member this.Allergenes = allergenes
    member this.Map = map
//    member this.findSingleAllergene () =
//        allergenes
//        |> Seq.filter (fun (a:AllergeneCanBeIn) -> (a.Ingredients.Length = 1))
//        |> Seq.head // if no found, it crashes, but meh
    member this.updateForAllergene (a:AllergeneCanBeIn) =
        let rmIngredient (toRm:String) (a:AllergeneCanBeIn)  =
            let newIng = a.Ingredients |> Array.filter (fun (i:String) -> i <> toRm)
            AllergeneCanBeIn(a.Name,newIng)
        if a.Ingredients.Length = 1 then
            let name = a.Name
            let value = a.Ingredients.[0]
            let newMap = map.Add (name,value)
            let newAllergenes = allergenes
                                |> Seq.filter (fun (a:AllergeneCanBeIn) -> a.Name <> name)
                                |> Seq.map (rmIngredient value)
                                |> Seq.toArray 
            AllergeneState(newAllergenes,newMap)
        else
            self
    member this.scanAndUpdate () : AllergeneState =
        let acc (state:AllergeneState) (a:AllergeneCanBeIn) : AllergeneState= state.updateForAllergene a             
        allergenes
        |> Seq.fold acc self
    member this.resolve () : AllergeneState =
        if this.Resolved then self
        else this.scanAndUpdate().resolve()
    
let sortAllergenes (map:Map<String,String>) =
    map
    |> Map.toSeq
    |> Seq.sortBy (fst)
    |> Seq.map (snd)
    |> String.concat ","   
    
let task2 (allergenes:AllergeneCanBeIn[]) =
    let state : AllergeneState = AllergeneState(allergenes,Map.empty)
    printfn "State0 %A" state 
    let state = state.scanAndUpdate () 
    printfn "State1 %A" state
    let state = state.resolve ()
    printfn "StateN %A" state
    let sorted = sortAllergenes state.Map
    printfn "Sorted: %s" sorted 
    
    

[<EntryPoint>]
let main argv =
    let foods = readInput "/Users/xeno/projects/aoc2020/day21_fs/input.txt"
    printfn "Foods: %A" foods
    let allergeneCandidates = task1 foods
    task2 allergeneCandidates
    0
    
    