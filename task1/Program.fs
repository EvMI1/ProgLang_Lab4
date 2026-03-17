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

let rec map f tree =
    match tree with
    | Nill -> Nill
    | Node(v, left, right) -> Node(f v, map f left, map f right)

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

let rec printTree tree space =
    match tree with
    | Nill -> ()
    | Node(v, left, right) ->
        printTree right (space + "    ")
        printfn "%s%.2f" space v
        printTree left (space + "    ")

[<EntryPoint>]
let main _ =
    let n = readCount ()
    let tree = fillTree n
    printfn "Исходное дерево: %A" (inorder tree)
    printTree tree ""
    let result = map floor tree
    printfn "Результирующее дерево: %A" (inorder result)
    printTree result ""
    0