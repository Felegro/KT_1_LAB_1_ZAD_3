module ListOps =

    /// Добавление элемента x в начало списка xs
    let add x xs =
        x :: xs

    /// Удаление первого вхождения x из списка xs
    let rec remove x = function
        | [] -> []
        | y::ys when y = x -> ys
        | y::ys           -> y :: remove x ys

    /// Поиск элемента x в списке xs (true, если найден)
    let rec contains x = function
        | []    -> false
        | y::ys -> (y = x) || contains x ys

    /// Сцепка двух списков: сначала xs, затем ys
    let rec concat xs ys =
        match xs with
        | []    -> ys
        | z::zs -> z :: concat zs ys

    /// Получение элемента по индексу (0-based)
    let rec getAt index = function
        | [] -> failwith "Index out of range"
        | x::_ when index = 0 -> x
        | _::xs when index > 0 -> getAt (index - 1) xs
        | _ -> failwith "Negative index not allowed"


open System
open ListOps

[<EntryPoint>]
let main _ =
    let lst = [2; 4; 6]

    // 1) add
    let lst1 = add 1 lst
    printfn "add 1: %A" lst1        // [1;2;4;6]

    // 2) remove
    let lst2 = remove 4 lst1
    printfn "remove 4: %A" lst2     // [1;2;6]

    // 3) contains
    printfn "contains 6? %b" (contains 6 lst2)
    printfn "contains 5? %b" (contains 5 lst2)

    // 4) concat
    let lst3 = concat lst2 [7;8]
    printfn "concat [7;8]: %A" lst3 // [1;2;6;7;8]

    // 5) getAt
    printfn "getAt 2: %d" (getAt 2 lst3) // 6

    0
