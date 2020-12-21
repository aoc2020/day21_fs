open System

open day21.BaseTypes
open day21.IO

let allIngredients (foods:Food[]):String[] =
        foods
        |> Seq.map (fun food -> food.Ingredients)
        |> Seq.concat
        |> Seq.distinct 
        |> Seq.toArray 

let allAllergens (foods:Food[]):String[] =
        foods
        |> Seq.map (fun food -> food.Contains)
        |> Seq.concat
        |> Seq.distinct
        |> Seq.toArray

type AllergenCanBeIn (name:String, ingredients: String[]) as self =
    override this.ToString () = sprintf "AllergenCanBeIn(%A ingredients:%A" name ingredients
    member this.Name = name
    member this.Ingredients = ingredients
    member this.isInFood (food:Food) =
        food.Contains |> Seq.contains name 
    member this.filterIngredientsByFood (food: Food) =
        let ingredientIsInFood (s:String) = food.Ingredients |> Seq.contains s 
        if this.isInFood food then
            let ing = ingredients |> Array.filter ingredientIsInFood
            AllergenCanBeIn(name, ing)
        else
            self
    member this.filterIngredientsByFoods (foods:Food[]) : AllergenCanBeIn =
        let acc (a:AllergenCanBeIn) (food:Food) = a.filterIngredientsByFood food
        foods |> Seq.fold acc self 

let task1 (foods:Food[]) : AllergenCanBeIn[] =
    let ingredients = allIngredients foods
    let allergens = allAllergens foods
    let allergens = allergens |> Array.map (fun a -> AllergenCanBeIn(a,ingredients))
    let allergens = allergens |> Array.map (fun (a:AllergenCanBeIn) -> a.filterIngredientsByFoods foods)
    let allergenCandidates =
        allergens
        |> Seq.map (fun (a:AllergenCanBeIn) -> a.Ingredients)
        |> Seq.concat
        |> Seq.distinct
        |> Seq.toArray
    let safeIngredients =
        ingredients
        |> Seq.filter (fun (i:String) -> allergenCandidates |> Seq.contains i |> not)
        |> Seq.toArray
    let safeOcc = 
        safeIngredients
        |> Seq.map (fun (safe:String) -> foods
                                         |> Seq.filter (fun (food:Food) -> food.Ingredients |> Seq.contains safe)
                                         |> Seq.length)
        |> Seq.sum
    printfn "Safe occurences: %A" safeOcc
    allergens

    
type AllergenState (allergenes:AllergenCanBeIn[],map:Map<String,String>) as self =
    override this.ToString () = sprintf "AllergenState(%A found: %A" allergenes map
    member this.Resolved = allergenes.Length = 0
    member this.Allergenes = allergenes
    member this.Map = map
    member this.updateForAllergene (a:AllergenCanBeIn) =
        let rmIngredient (toRm:String) (a:AllergenCanBeIn)  =
            let newIng = a.Ingredients |> Array.filter (fun (i:String) -> i <> toRm)
            AllergenCanBeIn(a.Name,newIng)
        if a.Ingredients.Length = 1 then
            let name = a.Name
            let value = a.Ingredients.[0]
            let newMap = map.Add (name,value)
            let newAllergenes = allergenes
                                |> Seq.filter (fun (a:AllergenCanBeIn) -> a.Name <> name)
                                |> Seq.map (rmIngredient value)
                                |> Seq.toArray 
            AllergenState(newAllergenes,newMap)
        else
            self
    member this.scanAndUpdate () : AllergenState =
        let acc (state:AllergenState) (a:AllergenCanBeIn) : AllergenState= state.updateForAllergene a             
        allergenes
        |> Seq.fold acc self
    member this.resolve () : AllergenState =
        if this.Resolved then self
        else this.scanAndUpdate().resolve()
    
let sortAllergenes (map:Map<String,String>) =
    map
    |> Map.toSeq
    |> Seq.sortBy (fst)
    |> Seq.map (snd)
    |> String.concat ","   
    
let task2 (allergenes:AllergenCanBeIn[]) =
    let state : AllergenState = AllergenState(allergenes,Map.empty)
    let state = state.resolve ()
    let sorted = sortAllergenes state.Map
    printfn "Sorted: %s" sorted 

[<EntryPoint>]
let main argv =
    let foods = readInput "/Users/xeno/projects/aoc2020/day21_fs/input.txt"
    let allergeneCandidates = task1 foods
    task2 allergeneCandidates
    0