open System

type Tree<'a> =
    | Nill
    | Node of 'a * Tree<'a> * Tree<'a>

let rec insert (value: float) tree =
    match tree with
    | Nill -> Node(value, Nill, Nill)
    | Node(v, left, right) ->
        if value < v then 
            Node(v, insert value left, right)
        elif value > v then 
            Node(v, left, insert value right)
        else 
            Node(v, left, right)

let fillTree (count: int) : Tree<float> =
    let rnd = Random()
    let randomFloat () = Math.Round(rnd.NextDouble() * 100.0, 2)
    List.init count (fun _ -> randomFloat ())
    |> List.fold (fun tree value -> insert value tree) Nill

let rec inorder tree =
    match tree with
    | Nill -> []
    | Node(v, left, right) -> inorder left @ [v] @ inorder right

let rec fold f acc tree =
    match tree with
    | Nill -> acc
    | Node(v, left, right) ->
        let acc1 = fold f acc left
        let acc2 = f acc1 v left right
        fold f acc2 right

let rec printTree tree space =
    match tree with
    | Nill -> ()
    | Node(v, left, right) ->
        printTree right (space + "    ")
        printfn "%s%.2f" space v
        printTree left (space + "    ")

let rec readCount () =
    printf "Введите количество элементов: "
    match Int32.TryParse(Console.ReadLine()) with
    | (true, n) when n <= 0 ->
        printfn "Ошибка: число должно быть больше 0"
        readCount ()
    | (true, n) -> n
    | _ ->
        printfn "Ошибка: введите целое число"
        readCount ()

let oneLeafNodes tree =
    fold (fun acc v left right ->
        match left, right with
        | Nill, Node(_, Nill, Nill) -> acc @ [v]
        | Node(_, Nill, Nill), Nill -> acc @ [v]
        | _ -> acc) [] tree


[<EntryPoint>]
let main _ =
    let n    = readCount ()
    let tree = fillTree n
    printfn "Дерево: %A" (inorder tree)
    printTree tree ""
    let result = oneLeafNodes tree
    printfn "Узлы с одним листом: %A" result
    0