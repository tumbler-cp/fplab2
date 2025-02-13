open PrefixTreeDictionary

let fastCheck () =
    let trie =
        PreDict.empty
        |> PreDict.insert "hello" 3
        |> PreDict.insert "hell" 1
        |> PreDict.insert "hell" 5
        |> PreDict.insert "world" 2
        |> PreDict.insert "win" 4


    PreDict.printTrie trie string

fastCheck ()
