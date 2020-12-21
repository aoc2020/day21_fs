module day21.BaseTypes

open System

type Food (ingredients: String[], contains: String[]) as self =
    override this.ToString () = sprintf "Food(%A contains=%A)" ingredients contains
    member this.Ingredients = ingredients 
    member this.Contains = contains 

