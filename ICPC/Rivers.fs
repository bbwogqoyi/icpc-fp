namespace Rivers
module Utils =
  open System

  type Info = {
    line_width:int
    length:int
  }

  type Head = {
    coord: (int*int)
    count: int
  }

  type SplitOption =
  | Default
  | RemoveEmpty
    
  let MAX_WORD_LENGTH, MIN_NUMBER_OF_WORDS = 80, 2
  let WHITESPACE = " "

  let max (a:int) (b:int) =
    match a>b with
    | true -> a
    | _ -> b

  let maxInfo (a:Info) (b:Info) =
    match a.length>b.length with
    | true -> a
    | _ -> b

  let abs (a:int) =
    match a<0 with
    | true -> -1*a
    | _ -> a

  /// Given a char separator and string, it splits it up into List of parts
  let stringSplit (input:string) (separator:char) (splitOption: SplitOption) = 
    let words = input.Split separator |> List.ofArray
    match splitOption with
    | Default -> words
    | RemoveEmpty -> words |> List.filter (fun x-> String.length x > 0)

  /// Given a sentance/paragraph as a single string, it finds length of the longest word
  let findLongestWordLength (words:string list) =
    let largestNumber (iLen:int) (cLen:int) = 
      match iLen < cLen with
      | true -> cLen
      | _ -> iLen

    List.fold (fun maxLength word -> largestNumber maxLength (String.length word) ) 0 words

  /// Given a list of strings, checks if an empty entries exists 
  let emptyEntriesExist (words:string list) : bool =
    let result = List.tryFind (fun word -> String.Empty=word) words 
    match result with
    | None -> false
    | Some _ -> true

  /// Given two strings, concatenates and returns the new string
  let stringConcat (state:string) (entry:string) =
    (String.concat WHITESPACE (state::entry::[])).Trim()

  /// Checks if a given string has lowercase, uppercase letters and whitespaces (SPACE)
  let isAllowedCharacters (input:string) = 
    let isValid = (fun ch -> Char.IsLetter ch || Char.IsSeparator ch)
    (String.forall isValid input)

  /// Reshapes the words list by lin-width
  let reshape (lineWidth:int) (words:string list) =
    let rec helper _in _out state =
      match _in with
      | [] -> (_out@[state])
      | entry::rest ->
        let newState = stringConcat state entry
        match (String.length newState) <= lineWidth with
        | true -> helper rest _out newState
        | false -> helper _in (_out@[state]) WHITESPACE
    helper words [] String.Empty

  /// Gets a string, and finds all the indexes of whitespaces in that string 
  let getWhitespaceCoordinates (line:string) (row:int) = 
    let rec helper (start:int) _out =
      let index = line.IndexOf(WHITESPACE, start)
      match index = -1 with
      | true -> _out
      | _ -> 
        let coordinate = (row, index)
        helper (index+1) (coordinate::_out)
    helper 0 []

  /// Imagine a list of strings as jaggered matrix, this finds all the whitespaces and rep
  let flattenListToCoordinates (words:string list) =
    let rec helper words index _out =
      match words with 
      | [] -> _out
      | h::t -> 
        let coords = (getWhitespaceCoordinates h index)@_out
        helper t (index+1) coords
    helper words 0 []

  // Searchs a list with (x,y) coordinate entries, return an adjacent entry to the input
  let tryFindAdjacentFromList coords (r, c) = 
    let rec helper candidates entry =
      match candidates, entry=None with
      | [], _ | _, false -> entry
      | (row, column)::t, _ ->  
        let entry = List.tryFind (fun coordEntry -> coordEntry = (row, column) ) coords
        helper t entry

    let row = r+1
    let candidates = (row, c-1)::(row, c)::(row, c+1)::[]
    helper candidates None

  let merge (a:Head) (b:Head) =
    let (r0,_), (r1,_) = a.coord, b.coord
    let _count = (a.count+b.count) - 1
    match r0>r1 with
    | true -> { a with count=_count }
    | false -> { b with count=_count }

  let appendDistinct key _out = 
    let predicate entry = entry.coord = key.coord
    match (List.tryFind predicate _out) with
    | None -> key::_out
    | Some v ->
      match v.count > key.count with
      | true -> _out
      | false -> key::(List.filter predicate _out)

  // Checks if (x,y) coordinate entries are adjacent to each other
  let coordsAjacent (r0, c0) (r1, c1) : bool = 
    let isUnder = abs(r1-r0) = 1 
    let isAdjacent = abs(c1-c0) = 0 || abs(c1-c0) = 1
    (isUnder && isAdjacent)

  (*
     |roses_are_red|
     |eish*I*like__|*
     *So, two is the maximum number of adacent items
  *)
  let newFlowEntry (entries:(int*int) list) = 
    let key = List.last entries
    let length = List.length entries
    { coord=key; count=length }

  let rec updateFlowEntries (key:int*int) (entries:(int*int) list) (_out:Head list) =
    let predicate key entry = coordsAjacent key.coord entry.coord
    match _out with
    | [] -> (newFlowEntry (key::entries))::[]
    | _ ->
      let entry = newFlowEntry (key::entries)
      let root = List.tryFind (predicate entry) _out
      match root with
      | None -> appendDistinct entry _out
      | Some v -> 
        let newEntry = merge entry v
        appendDistinct newEntry _out

  let pathfinder coords = 
    let rec helper _in _out =
      match _in with
      | [] -> _out
      | key::rest ->
        let entries = List.filter (coordsAjacent key) rest
        let _newOut = updateFlowEntries key entries _out
        helper rest _newOut
    helper coords []

  let getLengthOfLongest (words:string list) =
    let rec helper coords maxLength =
      match coords with
      | [] -> maxLength
      | _ ->
        let _out = pathfinder coords
        List.fold (fun count entry -> max entry.count count) maxLength _out

    let coords = List.rev (flattenListToCoordinates words)
    helper coords 0 

  let searchForRivers (lineWidth:int) (words:string list) =
    let rec helper width _info =
      let resized = reshape width words
      let length = getLengthOfLongest resized
      let _out = { line_width=width; length=length}::_info
      match (List.length resized) = 1 with
      | true -> _out
      | _ ->helper (width+1) _out

    let _results = helper lineWidth []
    let _default = { line_width=lineWidth; length=0 }
    let _max = List.fold (fun a b -> maxInfo a b) _default _results
    _max.line_width, _max.length

  let processInput (input:string) =
    let words = stringSplit input ' ' Default
    let longestWordLength = findLongestWordLength words

    let isInputValid =
      true
      |> (&&) (isAllowedCharacters input)
      |> (&&) (List.length words >= MIN_NUMBER_OF_WORDS)
      |> (&&) (longestWordLength <= MAX_WORD_LENGTH)
      |> (&&) (not (emptyEntriesExist words))

    match isInputValid with
    | false -> None
    | true -> 
      // TODO -- Finalize the logic
      // let rivers = searchForRivers longestWordLength words
      Some (searchForRivers longestWordLength words)
      
      // Defaulting with random values
      //Some (1, 5)

//processInput "The Yangtze is the third longest river in Asia and the longest in the world to flow entirely in one country"
//processInput "When two or more rivers meet at a confluence other than the sea the resulting merged river takes the name of one of those rivers"
    