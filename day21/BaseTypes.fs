module day21.BaseTypes

open System

type Input (foods: String[], contains: String[]) as self =
    override this.ToString () = sprintf "Food(%A contains=%A)" foods contains
    member this.Foods = foods
    member this.Contains = contains 

