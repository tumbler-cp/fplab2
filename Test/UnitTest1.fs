module Test

open NUnit.Framework
open PrefixTreeDictionary.PreDict
open FsCheck
open FsCheck.NUnit

[<SetUp>]
let Setup () = ()

let find key trie =
    let rec findHelper (node: TrieNode<'TValue>) (key: string) (index: int) =
        if index = key.Length then
            node.Value
        else
            match Map.tryFind key.[index] node.Children with
            | Some childNode -> findHelper childNode key (index + 1)
            | None -> None

    findHelper trie.Root key 0

[<Test>]
let ``Insert and Find Test`` () =
    let trie =
        empty
        |> insert "hello" 1
        |> insert "hell" 2
        |> insert "he" 3
        |> insert "hero" 4
        |> insert "her" 5

    Assert.That(find "hello" trie, Is.EqualTo(Some 1))
    Assert.That(find "hell" trie, Is.EqualTo(Some 2))
    Assert.That(find "he" trie, Is.EqualTo(Some 3))
    Assert.That(find "hero" trie, Is.EqualTo(Some 4))
    Assert.That(find "her" trie, Is.EqualTo(Some 5))
    Assert.That(find "hi" trie, Is.EqualTo None)

[<Test>]
let ``Remove Test`` () =
    let trie =
        empty
        |> insert "hello" 1
        |> insert "hell" 2
        |> insert "he" 3
        |> insert "hero" 4
        |> insert "her" 5
        |> remove "hell"
        |> remove "hero"

    Assert.That(find "hello" trie, Is.EqualTo(Some 1))
    Assert.That(find "hell" trie, Is.EqualTo None)
    Assert.That(find "he" trie, Is.EqualTo(Some 3))
    Assert.That(find "hero" trie, Is.EqualTo None)
    Assert.That(find "her" trie, Is.EqualTo(Some 5))

[<Test>]
let ``Fold Test`` () =
    let trie =
        empty
        |> insert "hello" 1
        |> insert "hell" 2
        |> insert "he" 3
        |> insert "hero" 4
        |> insert "her" 5

    let sumFolder acc value = acc + value
    let sum = fold sumFolder 0 trie
    Assert.That(sum, Is.EqualTo 15)

[<Test>]
let ``FoldBack Test`` () =
    let trie =
        empty
        |> insert "hello" 1
        |> insert "hell" 2
        |> insert "he" 3
        |> insert "hero" 4
        |> insert "her" 5

    let sumFolder value acc = acc + value
    let sum = foldBack sumFolder 0 trie
    Assert.That(sum, Is.EqualTo 15)

[<Test>]
let ``Filter Test`` () =
    let trie =
        empty
        |> insert "hello" 1
        |> insert "hell" 2
        |> insert "he" 3
        |> insert "hero" 4
        |> insert "her" 5

    let isEven value = value % 2 = 0
    let filteredTrie = filter isEven trie

    Assert.That(find "hello" filteredTrie, Is.EqualTo None)
    Assert.That(find "hell" filteredTrie, Is.EqualTo(Some 2))
    Assert.That(find "he" filteredTrie, Is.EqualTo None)
    Assert.That(find "hero" filteredTrie, Is.EqualTo(Some 4))
    Assert.That(find "her" filteredTrie, Is.EqualTo None)

[<Test>]
let ``Map Test`` () =
    let trie =
        empty
        |> insert "hello" 1
        |> insert "hell" 2
        |> insert "he" 3
        |> insert "hero" 4
        |> insert "her" 5

    let double value = value * 2
    let mappedTrie = map double trie
    Assert.That(find "hello" mappedTrie, Is.EqualTo(Some 2))
    Assert.That(find "hell" mappedTrie, Is.EqualTo(Some 4))
    Assert.That(find "he" mappedTrie, Is.EqualTo(Some 6))
    Assert.That(find "hero" mappedTrie, Is.EqualTo(Some 8))
    Assert.That(find "her" mappedTrie, Is.EqualTo(Some 10))

[<Test>]
let ``Monoid Test`` () =
    let trie1 = empty |> insert "hello" 1 |> insert "hell" 2

    let trie2 = empty |> insert "hero" 3 |> insert "her" 4

    let mergedTrie = merge trie1 trie2

    Assert.That(find "hello" mergedTrie, Is.EqualTo(Some 1))
    Assert.That(find "hell" mergedTrie, Is.EqualTo(Some 2))
    Assert.That(find "hero" mergedTrie, Is.EqualTo(Some 3))
    Assert.That(find "her" mergedTrie, Is.EqualTo(Some 4))

    let emptyTrie = empty
    let mergedWithEmpty = merge mergedTrie emptyTrie
    Assert.That(compareTrees mergedTrie mergedWithEmpty)

[<Property>]
let ``Test monoid: empty is neutral element INSERT`` (key: string) (value: int) =
    let trie1 = insert key value empty
    let trie2 = insert key value trie1
    let result = merge trie1 trie2
    compareTrees trie1 trie2 && compareTrees trie2 result

[<Property>]
let ``Test monoid: empty is neutral element for MERGE`` (key: string) (value: int) =
    let trie1 = insert key value empty
    let trie2 = insert key value empty
    compareTrees (merge trie1 empty) trie1 && compareTrees (merge empty trie2) trie2


[<Property>]
let ``Test monoid: merge is associative`` (key1: string) (key2: string) (key3: string) =
    let trie1 = insert key1 1 empty
    let trie2 = insert key2 2 empty
    let trie3 = insert key3 3 empty
    let left = merge (merge trie1 trie2) trie3
    let right = merge trie1 (merge trie2 trie3)
    compareTrees left right

[<Property>]
let ``Test insertion`` (key: string) (value: int) =
    let trie = insert key value empty
    Assert.That(find key trie, Is.EqualTo(Some value))

[<Property>]
let ``Test removing`` (key: string) (value: int) =
    let trie = insert key value empty
    let trieAfterRemove = remove key trie
    compareTrees trieAfterRemove empty
