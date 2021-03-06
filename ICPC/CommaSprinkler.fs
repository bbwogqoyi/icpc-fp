﻿namespace CommaSprinkler
module Utils =

  open System

  type TokenCase<'a> =
    | Add of 'a
    | Skip

  type Token = {
    input: string
    word: string
    preceded: bool
    succeeded: bool
  }

  type PunctuationIndex =
    | Nothing
    | End
    | Invalid

  type SplitOption =
    | Default
    | RemoveEmpty

  let COMMA, SPACE, PERIOD = ',', ' ', '.'
    
  /// Given a char separator and string, it splits it up into List of parts
  let stringSplit (input:string) (separator:char) (splitOption: SplitOption) = 
    let words = input.Split separator |> List.ofArray
    match splitOption with
    | Default -> words
    | RemoveEmpty -> words |> List.filter (fun x-> String.length x > 0)

  /// Check if last sentence is terminated with a period
  let isStringTerminationValid (input:string) =
    let periodIndex = input.LastIndexOf PERIOD
    periodIndex = (String.length input - 1)

  /// Check if sentence meets the minimum length requirements
  let isInRangeOfMinRequirement (input:string) (minLength:int) =
    match String.length input >= minLength with 
    | true -> System.Char.IsLetter input.[0]
    | _ -> false
    
  /// Only lowercase letter, comma, period and space are vaild charactors
  let isValidChar (ch:char) = 
    match  System.Char.IsLetter ch with
    | true -> System.Char.IsLower ch
    | _ -> ",. ".Contains((string ch))

  /// validates a complete string to check it has valid chars
  let validCharsInString (input:string) =
    String.forall isValidChar input

  /// check if the string has a punctuation and if exists; return the index
  let punctuationPosition (input:string) (punctuation:char) =
    let str = input.Trim()
    let lastIndex = (str.IndexOf punctuation) = ((String.length str) - 1)
    let missing = (str.IndexOf punctuation) = -1
    match missing, lastIndex with 
    | true, _ -> PunctuationIndex.Nothing
    | _, true -> PunctuationIndex.End
    | _ -> PunctuationIndex.Invalid

  /// Given a string return only the word without any comma or period
  let getWordOnly (input:string) =
    String.filter Char.IsLetter input

  /// validates whether the string is properly punctuated
  let isPunctuationValid (words: string list) =
    let predicate (punctuation:char) (word:string) =
      let punctuationCount = 
        String.filter Char.IsPunctuation word 
        |> String.length 
        |> (fun count -> count <= 1)

      let validIndex =
        match punctuationPosition word punctuation with
        | Nothing | End -> true
        | _ -> false

      not (validIndex && punctuationCount)

    match List.tryFind (predicate COMMA) words with
    | Some _ -> false
    | _ -> true

  /// Checks for empty entries or entries with punctuation only
  let emptyEntriesOrPunctuationOnly (words: string list) =
    let isNullOrWhiteSpace = 
      List.length (List.filter String.IsNullOrWhiteSpace words) > 0

    let predicate (word:string) = Char.IsPunctuation word.[0]

    let singleCharString = List.filter (fun word -> String.length word = 1) words
    match List.tryFind predicate (singleCharString) with
    | None -> isNullOrWhiteSpace
    | Some _ -> true

  /// easy utility to create a single token instance
  let createToken (input:string) (preceded:bool) (succeeded:bool) =
    let sanitizedInput = getWordOnly input
    Add { input=input; word=sanitizedInput; preceded=preceded; succeeded=succeeded }
  
  /// create word token -- TODO
  let wordTokenBuilder (prevWord: string) (current:string) = 
    let prevWordToken = punctuationPosition prevWord COMMA
    let wordToken = punctuationPosition current COMMA
    match prevWordToken, wordToken with
    | End, End -> createToken current true true
    | Nothing, End -> createToken current false true
    | End, Nothing  -> createToken current true false
    | _ -> Skip

  let retrieveTokenFromWatchList (word:string) (wList:Token list) =
    List.tryFind (fun x -> x.word = word) wList

  let aggregateTokenAndCase (token: Token) (case: Token) =
    let mergedToken = 
      createToken (token.input) (token.preceded || case.preceded) (token.succeeded || case.succeeded)
    mergedToken

  let updateWatchList (wList: Token list) (previousWord: string) (currentWord:string) =
    let previousToken =  retrieveTokenFromWatchList (getWordOnly previousWord) wList
    let currentToken = retrieveTokenFromWatchList (getWordOnly currentWord) wList

    let token =
      match previousToken, currentToken with
      | None, None -> wordTokenBuilder previousWord currentWord
      | Some previous, None -> wordTokenBuilder previous.input currentWord
      | None, Some current -> 
        let case = wordTokenBuilder previousWord current.input
        match case with
        | Skip -> createToken previousWord false current.preceded
        | Add v -> aggregateTokenAndCase current v
      | Some previous, Some current ->
        let case = 
          match previous.word=current.word && previous.succeeded with
          | true -> wordTokenBuilder (previous.input+",") current.input
          | false -> wordTokenBuilder previous.input current.input
        match case with
        | Skip -> case
        | Add v -> aggregateTokenAndCase current v

    match token with
    | Add v -> v::wList
    | Skip -> wList

  /// create a watchlist
  let createWatchList (words:string list) =
    let rec helper _in wList prevItem = 
      match _in with
      | [] -> wList
      | elem::[] -> updateWatchList wList prevItem elem
      | elem::rest -> 
        let watchList = updateWatchList wList prevItem elem
        helper rest watchList elem
    helper words [] String.Empty

  let removeAdjacentComma (_out:string) (word:string) =
    match String.IsNullOrEmpty _out with
    | true -> word
    | false ->
      match _out.[(String.length _out) - 1]=COMMA, word.[0]=COMMA with
      | true, true -> _out+" "+word.[1..]
      | _, true -> _out+word
      | _ -> _out+" "+word

  let stringBuilder (token:Token) =
    match token.preceded, token.succeeded with
    | true, true -> ", "+token.word+","
    | true, false -> ", "+token.word+""
    | false, false -> token.word+""
    | false, true -> token.word+","

  let watchListFolder (wList:Token list) (state:string) (entry:string) =
    let tokenOp = retrieveTokenFromWatchList (getWordOnly entry) wList
    let text =
      match tokenOp with
      | Some token -> 
        match entry.Contains (string PERIOD) with 
        | false -> stringBuilder token
        | true -> stringBuilder { token with word=entry }
      | None -> entry
    removeAdjacentComma (state.Trim()) (text.Trim())

  let formatAndCleanText (input:string) =
    let helper (word:string) =
      match word.IndexOf "." with
      | -1 -> word
      | idx -> word.[..idx]

    let words = List.filter (fun x -> x<>",") (stringSplit input SPACE RemoveEmpty)
    List.map helper words
    |> String.concat " " 

  /// Validates the cse if its valid
  let processInput (input:string) =
    let words = stringSplit input SPACE Default
    let isValid =
      true 
      |> (&&) (validCharsInString input)
      |> (&&) (isStringTerminationValid input)
      |> (&&) (isInRangeOfMinRequirement input 2)
      |> (&&) (isPunctuationValid words)
      |> (&&) (not (emptyEntriesOrPunctuationOnly words))
  
    match isValid with
    | false -> None
    | _ -> 
      let wList = createWatchList words
      let str = List.fold (watchListFolder wList) String.Empty words 
      Some (formatAndCleanText str)
