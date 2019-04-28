module BinaryTree

open System.ComponentModel

    type 't BinaryTree = Branch of 't * 't BinaryTree * 't BinaryTree
                       | Leaf

    let rec internal go value leaf less greater equals tree =
        let recall = go value leaf less greater equals
        match tree with
        | Leaf -> leaf Leaf
        | Branch(value, _, _) -> equals recall tree
        | Branch(x, left, _) when value < x -> less recall tree
        | Branch(x, _, right) when value > x -> greater recall tree
    
    let left = function
    | Leaf -> failwith "Концевая вершина не имеет потомков."
    | Branch(_, left, _) -> left

    let right = function
    | Leaf -> failwith "Концевая вершина не имеет потомков."
    | Branch(_, _, right) -> right

    let value = function
    | Leaf -> failwith "Концевая вершина не имеет потомков."
    | Branch(x, _, _) -> x

    let ins value = 
        let leaf _ = Branch(value, Leaf, Leaf)
        let less cont (Branch(x, left, right)) = Branch(x, cont left, right)
        let greater cont (Branch(x, left, right)) = Branch(x, left, cont right)
        let equals = less

        go value leaf less greater equals
    
    (*let insert =
        let rec insert' value = function
        | Leaf -> Branch(Leaf, Leaf, value)
        | Branch(left, right, x) when value <= x ->             
            Branch(insert' value left, right, x)
        | Branch(left, right, x) when value > x -> 
            Branch(left, insert' value right, x)
        
        insert'*)

    let find value =
        go value 
            (fun _ -> Leaf)                                 // Leaf
            (fun ctn (Branch(_, left, _)) -> ctn left)      // <
            (fun ctn (Branch(_, _, right)) -> ctn right)    // >
            (fun ctn tree -> tree)                          // =


    (*let find = 
        let rec find' value = function
        | Leaf -> Leaf
        | Branch(_, _, x) as tree when value = x -> tree
        | Branch(left, _, x) when value < x ->
            find' value left
        | Branch(_, right, x) when value > x ->
            find' value right
        
        find'*)
    
    let contains value (tree : 't BinaryTree) = 
        match find value tree with
        | Leaf -> false
        | Branch(_, _, _) -> true

    let rec remove value =       
        let rec least acc = function
        | Leaf -> acc
        | Branch(x, left, _) -> least x left

        let equals ctn = function
        | Leaf -> failwith "Структура дерева не предполагает такого исхода."
        | Branch(_, Leaf, Leaf) -> Leaf
        | Branch(_, Leaf, right) -> right
        | Branch(_, left, Leaf) -> left
        | Branch(x, left, right) -> 
            match right with
            | Branch(y, Leaf, rightRight) -> rightRight
            | right -> 
                let m = least x right
                Branch(m, left, remove m right)

        go value 
            (fun _ -> failwith "Элемент не найден в дереве.")
            (fun ctn (Branch(_, left, _)) -> ctn left)      // <
            (fun ctn (Branch(_, _, right)) -> ctn right)    // >
            equals                                          // =
                
    (*let remove = 
        let rec least acc = function
        | Leaf -> acc
        | Branch(left, _, x) -> least x left

        let rec remove' value = function
        | Leaf -> failwith "Элемент не найден в дереве."
        | Branch(_, _, x) as tree when value = x ->
            match tree with
            | Leaf -> failwith "Структура дерева не предполагает такого исхода."
            | Branch(Leaf, Leaf, _) -> Leaf
            | Branch(Leaf, right, _) -> right
            | Branch(left, Leaf, _) -> left
            | Branch(left, right, x) -> 
                match right with
                | Branch(Leaf, rightRight, y) -> rightRight
                | right -> 
                    let m = least x right
                    Branch(left, remove' m right, m)
        | Branch(left, right, x) when value < x -> 
            Branch(remove' value left, right, x)
        | Branch(left, right, x) when value > x -> 
            Branch(left, remove' value right, x)

        remove'*)
    
    type TraversalOrder = Prefix
                        | Infix
                        | Postfix

    let traverse (f : 't -> unit) order =
        let getOrder = function
        | Prefix -> (fun left right x -> x(); left(); right())
        | Infix -> (fun left right x -> left(); x(); right())
        | Postfix -> (fun left right x -> left(); right(); x())

        let rec traverse' = function
        | Leaf -> ()
        | Branch(x, left, right) -> 
            getOrder order
                (fun () -> traverse' left)
                (fun () -> traverse' right)
                (fun () -> f x)

        traverse'