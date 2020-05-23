namespace Rivers
module Utils =
  open System

  type Info = {
    width:int
    length:int
  }

  type River = {
    head: (int*int)
    path: (int*int) list
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
    match a.length>=b.length with
    | true -> a
    | _ -> b

  let abs (a:int) =
    match a<0 with
    | true -> -1*a
    | _ -> a

  /// Given a char separator and string, it splits it up into List of parts
  let stringSplit (input:string) (separator:char) (option: SplitOption) = 
    let words = input.Split separator |> List.ofArray
    match option with
    | Default -> words
    | RemoveEmpty -> words |> List.filter (fun x-> String.length x > 0)

  /// Given a sentance/paragraph as a single string, it finds length of the longest word
  let findLongestWordLength (words:string list) =
    let helper maxLength word = max maxLength (String.length word)  
    List.fold helper 0 words

  /// Given a list of strings, checks if an empty entries exists 
  let emptyEntriesExist (words:string list) : bool =
    List.exists (fun word -> String.Empty=word) words

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
  let flattenToCoordinates (words:string list) =
    let rec helper words index _out =
      match words with 
      | [] -> _out
      | h::t -> 
        let coords = (getWhitespaceCoordinates h index)@_out
        helper t (index+1) coords
    List.sort ( helper words 0 [] )

  /// Takes 2 River's, joins them and gets the new head
  let merge (a:River) (b:River) =
    let (r0,_), (r1,_) = a.head, b.head
    let _out = List.sort (List.distinct (a.path@b.path))
    match r0>r1 with
    | true -> { a with path=_out }
    | false -> { b with path=_out }

  /// Adds new river to the collection, ensuring all entries are unique
  let appendDistinct key _out = 
    let predicate func entry = func entry.head key.head
    match (List.tryFind (predicate (=))  _out) with
    | None -> key::_out
    | Some v -> 
      let _new = List.distinct (v.path@key.path)
      let entry = { key with path=_new }
      let rest = (List.filter (predicate (<>)) _out)
      entry::rest

  /// Checks if (x,y) coordinate entries are adjacent to each other
  let coordsAjacent (r0, c0) (r1, c1) : bool = 
    let isUnder = abs(r1-r0) = 1 
    let isAdjacent = abs(c1-c0) = 0 || abs(c1-c0) = 1
    (isUnder && isAdjacent)

  /// Creates a new instance of 'River' record
  let newFlowEntry (entries:(int*int) list) =
    let key = List.last (List.sort entries)
    { head=key; path=entries }

  /// Updates rivers; joining and growth
  let rec updateFlowEntries (key:int*int) (entries:(int*int) list) (_out:River list) =
    let predicate key entry = coordsAjacent key.head entry.head
    match _out with
    | [] -> (newFlowEntry (key::entries))::[]
    | _ ->
      let entry = newFlowEntry (key::entries)
      let root = List.tryFind (predicate entry) _out
      match root with
      | None -> appendDistinct entry _out
      | Some v -> 
        let newEntry = merge entry v
        let _newOut = appendDistinct newEntry _out
        List.filter (fun item -> item.head <> v.head) _newOut

  // Using a set of (x,y) coordinates, finds all the possible paths
  let pathfinder coords = 
    let rec helper _in _out =
      match _in with
      | [] -> _out
      | key::rest ->
        let entries = List.filter (coordsAjacent key) rest
        let _newOut = 
          match (List.length entries) = 2 with
          | false -> updateFlowEntries key entries _out
          | _ ->
            let a::b::_ = entries
            let _left = updateFlowEntries key [a] _out
            let _right = updateFlowEntries key [b] _out
            List.distinct (_left@_right)
        helper rest _newOut
    let _results = helper coords []
    let maxLen = List.fold (fun len x -> max (List.length x.path) len) 0 _results
    let _reduced = List.filter (fun x -> (List.length x.path)>=maxLen) _results
    _reduced

  /// Finds the longest river path and returns its length
  let getLongestRiverPath (rivers:River list) =
    let maxPath (count:int) (entry:River) =
      max count (List.length entry.path)
    List.fold maxPath 0 rivers

  let searchForRivers (lineWidth:int) (words:string list) (maxWidth:int)=
    let rec helper width _info _min=
      let wordGrid = reshape width words
      let coordinates = flattenToCoordinates wordGrid
      let riverPaths = pathfinder coordinates
      let length = getLongestRiverPath riverPaths
      let _out =  
        match length>_min with 
        | true -> { width=width; length=length}::_info
        | false -> _info

      match (List.length wordGrid)<_min || width>=maxWidth with
      | true -> _out
      | _ ->helper (width+1) _out (max _min length)

    let _results = helper lineWidth [] 0
    let _default = { width=lineWidth; length=0 }
    let _max = List.fold (fun a b -> maxInfo a b) _default _results
    _max.width, _max.length

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
      let maxWidth = (String.length input)
      Some (searchForRivers longestWordLength words maxWidth)