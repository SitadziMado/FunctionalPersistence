module Persistent
    /// <summary>
    /// Односвязный список с сохранением версий.
    /// </summary>
    type 't List = Cons of 't * 't List
                 | Nil
              
    let internal identity (x : 't) = x

    let rec internal reversingMap (f : 'a -> 'b) acc = function
        | Nil -> acc
        | Cons(x, xs) -> reversingMap f (Cons(f x, acc)) xs
    
    /// <summary>
    /// Обращение направления списка.
    /// </summary>
    /// <param name="list">Список для обработки</param>
    let reverse (list : 't List) = 
        list 
        |> reversingMap identity Nil

    /// <summary>
    /// Создание списка из произвольной последовательности.
    /// </summary>
    /// <param name="s">Последовательность</param>
    let createList (s : 't seq) =
        s
        |> Seq.fold (fun acc x -> Cons(x, acc)) Nil
        |> reverse

    /// <summary>
    /// Свертка списка.
    /// </summary>
    /// <param name="f">Функция, первый аргумент - аккумулятор,
    /// второй аргумент - текущее значение</param>
    /// <param name="init">Начальное значение свертки</param>
    /// <param name="list">Список для свертки</param>
    let fold (f : 't -> 't -> 't) init (list : 't List) =
        let rec fold' f  acc = function
        | Nil -> acc
        | Cons(x, xs) -> fold' f (f acc x) xs

        fold' f init list

    /// <summary>
    /// Отображение списка в другой с помощью функции.
    /// </summary>
    /// <param name="f">Функция, принимает параметр - текущее значение</param>
    /// <param name="list">Список для отображения</param>
    let map (f : 't -> 't) (list : 't List) =
        Nil
        |> reversingMap identity 
        <| reversingMap f Nil list

    let internal checkPos (pos : int) = 
        if pos < 0 then failwith "Указанный индекс не найден в списке."
    
    let internal takeAndProcessTheRest (f : 't List -> 't List) (n : int) (list : 't List) =
        let rec build (acc : 't List) = function
        | Nil -> acc
        | Cons(x, xs) -> build (Cons(x, acc)) xs

        let rec take' (acc : 't List) n = function
        | Nil when n > 0 -> failwith "В списке слишком мало элементов."
        | rest when n = 0 -> build (f rest) acc 
        | Cons(x, xs) -> take' (Cons(x, acc)) (n - 1) xs
        
        checkPos n
        take' Nil n list
        
    let take (n : int) =
        takeAndProcessTheRest (fun x -> Nil) n 

    /// <summary>
    /// Создание новой версии списка, в которой будет
    /// присутствовать элемент value на позиции pos + 1.
    /// </summary>
    /// <param name="value">Значение, которое необходимо вставить</param>
    /// <param name="pos">Позиция вставки</param>
    /// <param name="list">Список для обработки</param>
    let insertAfter value (pos : int) =
        takeAndProcessTheRest (fun xs -> Cons(value, xs)) (pos + 1)

    /// <summary>
    /// Создание новой версии списка, в которой будет
    /// присутствовать элемент value на позиции pos.
    /// </summary>
    /// <param name="value">Значение, которое необходимо вставить</param>
    /// <param name="pos">Позиция вставки</param>
    /// <param name="list">Список для обработки</param>
    let insertBefore value (pos : int) (list : 't List) =
        checkPos pos

        if pos = 0 then Cons(value, list)
                   else insertAfter value (pos - 1) list

    /// <summary>
    /// Создание новой версии списка с новым элементом в начале.
    /// </summary>
    /// <param name="value">Элемент для вставки</param>
    let insertFront value = insertBefore value 0

    /// <summary>
    /// Создание новой версии списка, в которой будет отсутствовать
    /// элемент на позиции pos.
    /// </summary>
    /// <param name="pos">Позиция для удаления</param>
    /// <param name="list">Список для обработки</param>
    let removeAt (pos : int) =
        takeAndProcessTheRest (fun (Cons(x, xs)) -> xs) pos

    /// <summary>
    /// Создание новой версии без первого элемента.
    /// </summary>
    /// <param name="list">Список для обработки</param>
    let removeFront (list : 't List) = removeAt 0 list

    let rec toSeq (list : 't List) = 
        seq {
            match list with
            | Nil -> yield! Seq.empty
            | Cons(x, xs) -> 
                yield x
                yield! toSeq xs
        }

    let rec toList (list : 't List) =
        list
        |> toSeq
        |> Seq.toList

    let print x = printf "%A" x