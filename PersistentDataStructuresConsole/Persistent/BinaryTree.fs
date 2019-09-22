module BinaryTree

open System.ComponentModel

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
    
    let left = function
    | Leaf -> failwith "Концевая вершина не имеет потомков."
    | Branch(_, left, _) -> left

    let right = function
    | Leaf -> failwith "Концевая вершина не имеет потомков."
    | Branch(_, _, right) -> right

    let value = function
    | Leaf -> failwith "Концевая вершина не имеет потомков."
    | Branch(x, _, _) -> x

    let insert value = 
        let leaf _ = Branch(value, Leaf, Leaf)
        let less cont (Branch(x, left, right)) = Branch(x, cont left, right)
        let greater cont (Branch(x, left, right)) = Branch(x, left, cont right)
        let equals = greater

        go value leaf less greater equals

    let construct (s : 't seq) =
        s
        |> Seq.fold (fun acc x -> insert x acc) Leaf

    let find value =
        go value 
            (fun _ -> Leaf)                                 // Leaf
            (fun ctn (Branch(_, left, _)) -> ctn left)      // <
            (fun ctn (Branch(_, _, right)) -> ctn right)    // >
            (fun ctn tree -> tree)                          // =
    
    let contains value (tree : 't BinaryTree) = 
        match find value tree with
        | Leaf -> false
        | Branch(_, _, _) -> true

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
    
    type TraversalOrder = Prefix
                        | Infix
                        | Postfix
    
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

    let rec toSeq (order : TraversalOrder) (tree : 't BinaryTree) =
        let mutable lst = new System.Collections.ArrayList(10)
        traverse (fun x -> lst.Add(value x) |> ignore) order tree
        Seq.cast lst