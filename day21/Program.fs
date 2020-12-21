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

let task1 (foods:Food[]) =
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
        

[<EntryPoint>]
let main argv =
    let foods = readInput "/Users/xeno/projects/aoc2020/day21_fs/input.txt"
    printfn "Foods: %A" foods
    task1 foods 
    0
    
    