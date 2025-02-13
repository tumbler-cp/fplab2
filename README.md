### Ходжаев, P3308, 368994
## Функциональное программирование
# Лабораторная работа №2
## Вариант

`pre-dict` - PrefixTreeDictionary

`PrefixTree` (`trie`) — это структура данных, которая используется для хранения набора строк. Основная идея заключается в том, что строки с общими префиксами (начальными частями) хранятся вместе, что позволяет эффективно выполнять операции поиска, вставки и удаления.

Вот основные моменты:

Узел (Node): Каждый узел в дереве представляет один символ строки.
Корень (Root): Начальный узел дерева, от которого начинаются все строки.
Потомки (Children): Узлы, которые следуют за текущим узлом, представляя следующие символы строк.
Терминальный узел (Terminal Node): Узел, который обозначает конец строки.

Пример `pre-dict` (из моей реализации):

Такое дерево
```fsharp
let trie =
    PreDict.empty
    |> PreDict.insert "hello" 3
    |> PreDict.insert "hell" 1
    |> PreDict.insert "hell" 5
    |> PreDict.insert "world" 2
    |> PreDict.insert "win" 4
```

Выглядит так:
```
(root)
    ├── h
    │   └── e
    │       └── l
    │           └── l -> 5
    │               └── o -> 3
    └── w
        ├── i
        │   └── n -> 4
        └── o
            └── r
                └── l
                    └── d -> 2
```

---

### 1. Добавление элементов

**Код:**  
```fsharp
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
```

**Объяснение:**
- Рекурсивная функция `insertHelper` проходит по символам строки `key`.  
- При достижении конца строки (`index = key.Length`) обновляется узел, устанавливая `Value = Some value` и флаг `IsTerminal = true`.
- Если текущего символа ещё нет в потомках, создаётся новый узел.
- Возвращается новый объект `Trie`, что обеспечивает неизменяемость структуры.

---

### 2. Удаление элементов

**Код:**  
```fsharp
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
                    Some { node with
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
```

**Объяснение:**
- Рекурсивная функция `removeHelper` спускается по дереву до конца ключа.
- При достижении конца ключа, если узел является терминальным, он «очищается» (удаляется значение и сбрасывается `IsTerminal`).
- Если у узла нет детей после удаления, возвращается `None` для «очищения» пути.
- Новый `Trie` возвращается без изменения исходного, что соответствует принципу неизменяемости.

---

### 3. Фильтрация

**Код:**  
```fsharp
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
            Some { node with
                   Children = filteredChildren
                   IsTerminal = true
                   Value = Some value }
        | _, _ when not filteredChildren.IsEmpty ->
            Some { node with
                   Children = filteredChildren
                   IsTerminal = false
                   Value = None }
        | _ -> None
    match filterHelper trie.Root with
    | Some filteredRoot -> { trie with Root = filteredRoot }
    | None -> empty
```

**Объяснение:**
- Функция `filterHelper` рекурсивно проверяет каждый узел.
- Если узел является терминальным и его значение удовлетворяет предикату, узел сохраняется.
- Если узел не удовлетворяет условию, но имеет удовлетворяющих потомков, он сохраняется как промежуточный.
- В противном случае узел исключается из результирующего дерева.

---

### 4. Отображение (map)

**Код:**  
```fsharp
let map (transform: 'T -> 'U) (trie: Trie<'T>) : Trie<'U> =
    let rec mapHelper (node: TrieNode<'T>) : TrieNode<'U> =
        let mappedValue = Option.map transform node.Value
        let mappedChildren =
            node.Children |> Map.map (fun _ childNode -> mapHelper childNode)
        { Value = mappedValue
          Children = mappedChildren
          IsTerminal = node.IsTerminal }
    { Root = mapHelper trie.Root }
```

**Объяснение:**
- Функция `map` преобразует значения узлов при помощи функции `transform`.
- Рекурсивная функция `mapHelper` сохраняет топологию дерева, изменяя только тип значений.
- Новое дерево создаётся неизменяемым способом.

---

### 5. Свертки (fold и foldBack)

**Левая свертка (`fold`):**  
```fsharp
let fold (folder: 'State -> 'T -> 'State) (state: 'State) (trie: Trie<'T>) : 'State =
    let rec foldHelper (state: 'State) (node: TrieNode<'T>) : 'State =
        let stateWithNode =
            match node.Value with
            | Some value when node.IsTerminal -> folder state value
            | _ -> state
        node.Children
        |> Map.fold (fun acc _ childNode -> foldHelper acc childNode) stateWithNode
    foldHelper state trie.Root
```

**Правая свертка (`foldBack`):**  
```fsharp
let foldBack (folder: 'T -> 'State -> 'State) (state: 'State) (trie: Trie<'T>) : 'State =
    let rec foldBackHelper (state: 'State) (node: TrieNode<'T>) : 'State =
        let stateWithChildren =
            node.Children
            |> Map.fold (fun acc _ childNode -> foldBackHelper acc childNode) state
        match node.Value with
        | Some value when node.IsTerminal -> folder value stateWithChildren
        | _ -> stateWithChildren
    foldBackHelper state trie.Root
```

**Объяснение:**
- Функции `fold` и `foldBack` рекурсивно обходят дерево.
- Они аккумулируют результат, проходя сначала по узлам, а затем обрабатывая детей (или наоборот для `foldBack`).
- Это позволяет сворачивать структуру дерева в единственное значение, не изменяя само дерево.

---

### 6. Моноидальная структура (merge)

**Код:**  
```fsharp
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
            |> Map.fold (fun acc key childNode1 ->
                match Map.tryFind key node2.Children with
                | Some childNode2 -> Map.add key (mergeNodes childNode1 childNode2) acc
                | None -> Map.add key childNode1 acc)
                node2.Children
        { Value = mergedValue
          Children = mergedChildren
          IsTerminal = node1.IsTerminal || node2.IsTerminal }
    { Root = mergeNodes trie1.Root trie2.Root }
```

**Объяснение:**
- Функция `merge` объединяет два `Trie` в один, что и соответствует требованию моноидальности.
- Нейтральным элементом является `empty` (пустое дерево).
- Операция объединения выполняется рекурсивно в `mergeNodes`, комбинируя значения и детей из обоих деревьев.

---

### 7. Полиморфизм и неизменяемость

- **Полиморфизм:**  
  Все типы и функции используют параметризированные типы (`'T`, `'U`), что позволяет работать с любыми данными. Например, функция `map` имеет сигнатуру:
  ```fsharp
  let map (transform: 'T -> 'U) (trie: Trie<'T>) : Trie<'U>
  ```
- **Неизменяемость:**  
  Все операции (insert, remove, map, filter, fold) возвращают новые экземпляры структур `Trie` и `TrieNode` без изменения исходного объекта.
