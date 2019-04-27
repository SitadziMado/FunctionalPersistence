module BinaryTree

open System.ComponentModel

    type 't BinaryTree = Branch of 't BinaryTree * 't BinaryTree * 't
                       | Leaf

    let rec internal go value leaf less greater equals tree =
        let recall = go value leaf less greater equals
        match tree with
        | Leaf -> leaf Leaf
        | Branch(_, _, value) -> equals recall tree
        | Branch(left, _, x) when value < x -> less recall tree
        | Branch(_, right, x) when value > x -> greater recall tree
    
    let left = function
    | Leaf -> failwith "Концевая вершина не имеет потомков."
    | Branch(left, _, _) -> left

    let right = function
    | Leaf -> failwith "Концевая вершина не имеет потомков."
    | Branch(_, right, _) -> right

    let value = function
    | Leaf -> failwith "Концевая вершина не имеет потомков."
    | Branch(_, _, x) -> x

    let ins value = 
        let leaf _ = Branch(Leaf, Leaf, value)
        let less cont (Branch(left, right, x)) = Branch(cont left, right, x)
        let greater cont (Branch(left, right, x)) = Branch(left, cont right, x)
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
            (fun ctn (Branch(left, _, _)) -> ctn left)      // <
            (fun ctn (Branch(_, right, _)) -> ctn right)    // >
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
        | Branch(left, _, x) -> least x left

        let equals ctn = function
        | Leaf -> failwith "Структура дерева не предполагает такого исхода."
        | Branch(Leaf, Leaf, _) -> Leaf
        | Branch(Leaf, right, _) -> right
        | Branch(left, Leaf, _) -> left
        | Branch(left, right, x) -> 
            match right with
            | Branch(Leaf, rightRight, y) -> rightRight
            | right -> 
                let m = least x right
                Branch(left, remove m right, m)

        go value 
            (fun _ -> failwith "Элемент не найден в дереве.")
            (fun ctn (Branch(left, _, _)) -> ctn left)      // <
            (fun ctn (Branch(_, right, _)) -> ctn right)    // >
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