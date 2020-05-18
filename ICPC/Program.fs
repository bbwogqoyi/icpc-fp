module ICPC
open System

let SPACE, COMMA = ' ', ','

/// Given a char separator and string, it splits it up into List of parts
let stringSplit (input:string) (separator:char) = 
  let words = input.Split separator |> List.ofArray
  words |> List.filter (fun x-> String.length x > 0)

let stripComma (word:string) : string = word.Trim ','

let containsComma (word:string) : bool = (word.IndexOf COMMA) <> -1

let getIndexesOfOccanrances (word:string) (words:string list) =
  let rec helper (index:int) _out words =
    match words with
    | [] -> _out
    | h::t -> 
      match h=word with
      | true -> helper (index+1) (index::_out) t
      | _ -> helper (index+1) _out t
  helper 0 [] words

let commaSprinkler (input:string) =
  let words = stringSplit input SPACE
  let wordsWithComma = 
    List.filter containsComma words
    |> List.map stripComma

  getIndexesOfOccanrances 


// commaSprinkler "one, two. one tree."

let rivers input =
  failwith "Not implemented"

[<EntryPoint>]
let main argv =
  printfn "Hello World from F#!"
  0 // return an integer exit code
