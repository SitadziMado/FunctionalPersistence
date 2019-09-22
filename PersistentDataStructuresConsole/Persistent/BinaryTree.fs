module BinaryTree

open System.ComponentModel
open System

    /// <summary>
    /// Двоичное дерево поиска
    /// </summary>
    type 't BinaryTree = Branch of 't * 't BinaryTree * 't BinaryTree
                       | Leaf

    let rec internal go value leaf less greater equals tree =
        let recall = go value leaf less greater equals
        match tree with
        | Leaf -> leaf Leaf
        | Branch(x, _, _) when value = x -> equals recall tree
        | Branch(x, left, _) when value < x -> less recall tree
        | Branch(x, _, right) when value > x -> greater recall tree
        | Branch(_, _, _) -> failwith "Данный шаблон никогда не должен сопоставиться"
    
    /// <summary>
    /// Получить левого ребенка текущего узла
    /// </summary>
    let left = function
    | Leaf -> failwith "Концевая вершина не имеет потомков."
    | Branch(_, left, _) -> left
    
    /// <summary>
    /// Получить правого ребенка текущего узла
    /// </summary>
    let right = function
    | Leaf -> failwith "Концевая вершина не имеет потомков."
    | Branch(_, _, right) -> right

    
    /// <summary>
    /// Получить значение текущего узла
    /// </summary>
    let value = function
    | Leaf -> failwith "Концевая вершина не имеет потомков."
    | Branch(x, _, _) -> x

    /// <summary>
    /// Вставить элемент в дерево
    /// </summary>
    /// <param name="value">Значение, которое следует вставить в дерево</param>
    let insert value = 
        let leaf _ = Branch(value, Leaf, Leaf)
        let less cont (Branch(x, left, right)) = Branch(x, cont left, right)
        let greater cont (Branch(x, left, right)) = Branch(x, left, cont right)
        let equals = greater

        go value leaf less greater equals

    /// <summary>
    /// Создать дерево из последовательности
    /// </summary>
    /// <param name="s">Исходная последовательность</param>
    let construct (s : 't seq) =
        s
        |> Seq.fold (fun acc x -> insert x acc) Leaf

    /// <summary>
    /// Найти значение в дереве, если значения нет - возвращается Leaf
    /// </summary>
    /// <param name="value">Значение для поиска</param>
    let find value =
        go value 
            (fun _ -> Leaf)                                 // Leaf
            (fun ctn (Branch(_, left, _)) -> ctn left)      // <
            (fun ctn (Branch(_, _, right)) -> ctn right)    // >
            (fun ctn tree -> tree)                          // =
    
    /// <summary>
    /// Проверить, содержится ли искомое значение в дереве поиска
    /// </summary>
    /// <param name="value">Значение для поиска</param>
    /// <param name="tree">Дерево, по которому нужно искать</param>
    let contains value (tree : 't BinaryTree) = 
        match find value tree with
        | Leaf -> false
        | Branch(_, _, _) -> true

    /// <summary>
    /// Удалить экземпляр значения из дерева
    /// </summary>
    /// <param name="value">Значение, подлежащее удалению</param>
    let rec remove value =       
        let rec least acc = function
        | Leaf -> acc
        | Branch(x, left, _) -> least x left

        let equals _ = function
        | Leaf -> failwith "Некорректное дерево."
        | Branch(_, Leaf, Leaf) -> Leaf     // V У вершины нет детей
        | Branch(_, Leaf, right) -> right   // V У вершины только правый ребенок
        | Branch(_, left, Leaf) -> left     // V У вершины только левый ребенок
        | Branch(x, left, right) ->         // X У вершины оба ребенка
            let m = least x right
            Branch(m, left, remove m right)

        go value 
            (fun _ -> failwith "Элемент не найден в дереве.")
            (fun cont (Branch(x, left, right)) -> Branch(x, cont left, right))    // <
            (fun cont (Branch(x, left, right)) -> Branch(x, left, cont right))    // >
            equals                                                                // =
    
    /// <summary>
    /// Порядок обхода дерева
    /// </summary>
    type TraversalOrder = Prefix
                        | Infix
                        | Postfix
    
    /// <summary>
    /// Обход дерева в указанном порядке
    /// </summary>
    /// <param name="f">Функция обратного вызова (текущий узел)</param>
    /// <param name="order">Порядок обхода</param>
    let traverse (f : 't BinaryTree -> unit) order =
        let getOrder = function
        | Prefix -> (fun left right x -> x(); left(); right())
        | Infix -> (fun left right x -> left(); x(); right())
        | Postfix -> (fun left right x -> left(); right(); x())

        let rec traverse' = function
        | Leaf -> ()
        | Branch(_, left, right) as node -> 
            getOrder order
                (fun () -> traverse' left)
                (fun () -> traverse' right)
                (fun () -> f node)

        traverse'
    
    (*let internal getOrder = function
    | Prefix -> (fun left right x -> right (left (x())))
    | Infix -> (fun left right x -> right(x (left())))
    | Postfix -> (fun left right x -> x(right (left())))*)

    /// <summary>
    /// Произвести свертку дерева по функции
    /// </summary>
    /// <param name="f">Функция для свертки (левое значение, правое значение)</param>
    let accumulate (f : 'a -> 'a -> 'a) = 
        let rec accumulate' = function
        | Branch(x, Leaf, Leaf) -> x
        | Branch(x, left, Leaf) -> f x (accumulate' left)
        | Branch(x, Leaf, right) -> f x (accumulate' right)
        | Branch(x, left, right) as node -> f x (f (accumulate' left) (accumulate' right))
        | Leaf -> failwith "Недопустимая структура дерева"

        accumulate'

    /// <summary>
    /// Найти максимальное значение в дереве
    /// </summary>
    /// <param name="tree"></param>
    let max (tree : 't BinaryTree) = 
        tree 
        |> accumulate (fun x y -> if x > y then x else y) 
    
    /// <summary>
    /// Найти минимальное значение в дереве
    /// </summary>
    /// <param name="tree"></param>
    let min (tree : 't BinaryTree) = 
        tree 
        |> accumulate (fun x y -> if x < y then x else y)
     
    /// <summary>
    /// Преобразование дерева в плоскую последовательность с учетом порядка обхода
    /// </summary>
    /// <param name="order">Порядок обхода</param>
    /// <param name="tree">Дерево для обхода</param>
    let rec toSeq (order : TraversalOrder) (tree : 't BinaryTree) =
        let mutable lst = new System.Collections.ArrayList(10)
        traverse (fun x -> lst.Add(value x) |> ignore) order tree
        Seq.cast lst