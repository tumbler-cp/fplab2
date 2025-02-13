namespace PrefixTreeDictionary

module PreDict =

    type TrieNode<'T> =
        { Value: 'T option
          Children: Map<char, TrieNode<'T>>
          IsTerminal: bool }

    type Trie<'T> = { Root: TrieNode<'T> }

    let empty: Trie<'T> =
        { Root =
            { Value = None
              Children = Map.empty
              IsTerminal = false } }

    let insert (key: string) (value: 'T) (trie: Trie<'T>) : Trie<'T> =
        let rec insertHelper (node: TrieNode<'T>) (key: string) (index: int) : TrieNode<'T> =
            match index = key.Length with
            | true ->
                { node with
                    Value = Some value
                    IsTerminal = true }
            | false ->
                let char = key.[index]

                let childNode =
                    match Map.tryFind char node.Children with
                    | Some n -> n
                    | None ->
                        { Value = None
                          Children = Map.empty
                          IsTerminal = false }

                let updatedChild = insertHelper childNode key (index + 1)

                { node with
                    Children = Map.add char updatedChild node.Children }

        { trie with
            Root = insertHelper trie.Root key 0 }

    let remove (key: string) (trie: Trie<'T>) : Trie<'T> =
        let rec removeHelper (node: TrieNode<'T>) (key: string) (index: int) : TrieNode<'T> option =
            match index = key.Length with
            | true ->
                match node.IsTerminal with
                | true ->
                    let updatedNode =
                        { node with
                            Value = None
                            IsTerminal = false }

                    if updatedNode.Children.IsEmpty then
                        None
                    else
                        Some updatedNode
                | false -> Some node
            | false ->
                let char = key.[index]

                match Map.tryFind char node.Children with
                | Some childNode ->
                    match removeHelper childNode key (index + 1) with
                    | Some updatedChild ->
                        Some
                            { node with
                                Children = Map.add char updatedChild node.Children }
                    | None ->
                        let updatedChildren = Map.remove char node.Children

                        if node.IsTerminal || not updatedChildren.IsEmpty then
                            Some { node with Children = updatedChildren }
                        else
                            None
                | None -> Some node

        match removeHelper trie.Root key 0 with
        | Some updatedRoot -> { trie with Root = updatedRoot }
        | None ->
            { trie with
                Root =
                    { Value = None
                      Children = Map.empty
                      IsTerminal = false } }

    let filter (predicate: 'T -> bool) (trie: Trie<'T>) : Trie<'T> =
        let rec filterHelper (node: TrieNode<'T>) : TrieNode<'T> option =
            let filteredChildren =
                node.Children
                |> Map.toSeq
                |> Seq.choose (fun (char, childNode) ->
                    filterHelper childNode |> Option.map (fun filteredChild -> char, filteredChild))
                |> Map.ofSeq

            match node.IsTerminal, node.Value with
            | true, Some value when predicate value ->
                Some
                    { node with
                        Children = filteredChildren
                        IsTerminal = true
                        Value = Some value }
            | _, _ when not filteredChildren.IsEmpty ->
                Some
                    { node with
                        Children = filteredChildren
                        IsTerminal = false
                        Value = None }
            | _ -> None

        match filterHelper trie.Root with
        | Some filteredRoot -> { trie with Root = filteredRoot }
        | None -> empty

    let map (transform: 'T -> 'U) (trie: Trie<'T>) : Trie<'U> =
        let rec mapHelper (node: TrieNode<'T>) : TrieNode<'U> =
            let mappedValue = Option.map transform node.Value

            let mappedChildren =
                node.Children |> Map.map (fun _ childNode -> mapHelper childNode)

            { Value = mappedValue
              Children = mappedChildren
              IsTerminal = node.IsTerminal }

        { Root = mapHelper trie.Root }

    let fold (folder: 'State -> 'T -> 'State) (state: 'State) (trie: Trie<'T>) : 'State =
        let rec foldHelper (state: 'State) (node: TrieNode<'T>) : 'State =
            let stateWithNode =
                match node.Value with
                | Some value when node.IsTerminal -> folder state value
                | _ -> state

            node.Children
            |> Map.fold (fun acc _ childNode -> foldHelper acc childNode) stateWithNode

        foldHelper state trie.Root

    let foldBack (folder: 'T -> 'State -> 'State) (state: 'State) (trie: Trie<'T>) : 'State =
        let rec foldBackHelper (state: 'State) (node: TrieNode<'T>) : 'State =
            let stateWithChildren =
                node.Children
                |> Map.fold (fun acc _ childNode -> foldBackHelper acc childNode) state

            match node.Value with
            | Some value when node.IsTerminal -> folder value stateWithChildren
            | _ -> stateWithChildren

        foldBackHelper state trie.Root

    let merge (trie1: Trie<'T>) (trie2: Trie<'T>) : Trie<'T> =
        let rec mergeNodes (node1: TrieNode<'T>) (node2: TrieNode<'T>) : TrieNode<'T> =
            let mergedValue =
                match node1.Value, node2.Value with
                | Some v1, Some v2 when node1.IsTerminal && node2.IsTerminal -> Some v1
                | Some v, _ when node1.IsTerminal -> Some v
                | _, Some v when node2.IsTerminal -> Some v
                | _ -> None

            let mergedChildren =
                node1.Children
                |> Map.fold
                    (fun acc key childNode1 ->
                        match Map.tryFind key node2.Children with
                        | Some childNode2 -> Map.add key (mergeNodes childNode1 childNode2) acc
                        | None -> Map.add key childNode1 acc)
                    node2.Children

            { Value = mergedValue
              Children = mergedChildren
              IsTerminal = node1.IsTerminal || node2.IsTerminal }

        { Root = mergeNodes trie1.Root trie2.Root }

    let rec compareNodes (node1: TrieNode<'T>) (node2: TrieNode<'T>) : bool =
        node1.Value = node2.Value
        && node1.IsTerminal = node2.IsTerminal
        && Map.forall
            (fun key child1 ->
                match Map.tryFind key node2.Children with
                | Some child2 -> compareNodes child1 child2
                | None -> false)
            node1.Children
        && Map.forall (fun key _ -> Map.containsKey key node1.Children) node2.Children

    let compareTrees (tree1: Trie<'T>) (tree2: Trie<'T>) : bool = compareNodes tree1.Root tree2.Root

    let rec printTrieNode
        (node: TrieNode<'T>)
        (label: string)
        (indent: string)
        (isLast: bool)
        (printValue: 'T -> string)
        : unit =
        let nodeLabel = match label with
                        | "" -> "(root)"
                        | _ -> label

        let branch = match indent, isLast with
                     | "", _ -> ""
                     | _, true -> "└── "
                     | _, false -> "├── "

        let terminalInfo = match node.IsTerminal, node.Value with
                           | true, Some value -> sprintf " -> %s" (printValue value)
                           | true, None -> " -> None"
                           | false, _ -> ""

        printfn "%s%s%s%s" indent branch nodeLabel terminalInfo
        let newIndent = indent + (if isLast then "    " else "│   ")
        let children = node.Children |> Map.toList
        let count = List.length children

        children
        |> List.iteri (fun i (key, childNode) ->
            let isLastChild = i = (count - 1)
            printTrieNode childNode (string key) newIndent isLastChild printValue)

    let printTrie (trie: Trie<'T>) (printValue: 'T -> string) : unit =
        printTrieNode trie.Root "" "" true printValue
