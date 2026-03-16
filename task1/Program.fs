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

let printList (lst: float list) =
    printfn " Дерево: %A" lst

[<EntryPoint>]
let main _ =
    let n    = readCount ()
    let tree = fillTree n
    printfn "Дерево: %A" (inorder tree)
    let result = inorder tree |> List.map floor
    printfn "Результат: %A" result
    0