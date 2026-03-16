open System

type Tree =
    | Empty
    | Node of int * Tree * Tree

// Функция для вставки элемента в бинарное дерево поиска
let rec insertIntoTree (value: int) (tree: Tree) =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(nodeValue, left, right) ->
        if value <= nodeValue then
            Node(nodeValue, insertIntoTree value left, right)
        else
            Node(nodeValue, left, insertIntoTree value right)

// Функция для построения дерева из списка чисел
let buildTreeFromList (values: int list) =
    let rec build values acc =
        match values with
        | [] -> acc
        | head :: tail -> build tail (insertIntoTree head acc)
    build values Empty

// Функция для проверки, является ли узел листом
let isLeaf node =
    match node with
    | Empty -> false
    | Node(_, left, right) -> left = Empty && right = Empty

// Функция для подсчета суммы четных значений в листьях
let rec sumEvenInLeaves tree =
    match tree with
    | Empty -> 0
    | Node(value, left, right) ->
        let sumFromChildren = sumEvenInLeaves left + sumEvenInLeaves right
        // Проверяем, является ли текущий узел листом и четно ли его значение
        if isLeaf (Node(value, left, right)) && value % 2 = 0 then
            value + sumFromChildren
        else
            sumFromChildren

// Функция для вывода дерева
let rec printTree indent tree =
    match tree with
    | Empty -> printfn "%sEmpty" indent
    | Node(value, left, right) ->
        // Добавляем пометку для листьев
        printfn "%sNode(%d)" indent value
        printTree (indent + "  ") left
        printTree (indent + "  ") right

// Функция для чтения целого числа
let readInt prompt =
    let rec loop () =
        printf "%s" prompt
        match Int32.TryParse(Console.ReadLine()) with
        | true, value when value > 0 -> value
        | _ -> 
            printfn "Ошибка! Введите положительное число."
            loop ()
    loop ()

// Функция для генерации случайных чисел
let generateRandomNumbers count =
    let rnd = Random()
    printfn "\nГенерируем %d случайных чисел от 1 до 100" count
    
    [ for i in 1..count -> 
        let number = rnd.Next(1, 101)  // генерация от 1 до 100
        printf "%d " number
        number ]
    |> fun numbers -> printfn ""; numbers

[<EntryPoint>]
let main argv =    
    printfn "=== ПОИСК СУММЫ ЧЕТНЫХ ЧИСЕЛ В ЛИСТЬЯХ ДЕРЕВА ==="
    // Запрашиваем количество элементов
    let count = readInt "Введите количество элементов в дереве: "
    // Генерируем случайные числа
    let numbers = generateRandomNumbers count
    // Построение дерева
    let tree = buildTreeFromList numbers
    // Вывод дерева
    printfn "\nПостроенное дерево:"
    printTree "" tree
    // Подсчет суммы четных чисел в листьях
    let sum = sumEvenInLeaves tree
    printfn "Сумма четных значений в листьях: %d" sum    
    0