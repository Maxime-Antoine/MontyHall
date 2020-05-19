open System

let N = 10000
let rnd = Random()

type Step1 = { Prize: int; Choice: int }
type Step2 = { Prize: int; Choice: int; Eliminated: int; }

let generateGames n = Seq.init n (fun _ -> { Prize = rnd.Next(1, 4); Choice = rnd.Next(1, 4)})

let rec eliminateBadChoice (step1: Step1) = 
    let eliminated = rnd.Next(1, 4)
    if eliminated <> step1.Prize && eliminated <> step1.Choice 
    then { Prize = step1.Prize; Choice = step1.Choice; Eliminated = eliminated } 
    else eliminateBadChoice step1

let didWinWithoutChange step2 = step2.Choice = step2.Prize
let didWinWithChange step2 = step2.Prize <> step2.Choice

let toPct n = (float n) / (float N) * 100.
let simulate n winFn = generateGames N |> Seq.map eliminateBadChoice |> Seq.filter winFn |> Seq.length |> toPct

[<EntryPoint>]
let main argv =
    let noChangePct = simulate N didWinWithoutChange
    let changePct = simulate N didWinWithChange

    printfn "After %i simulations of each case:" N
    printfn "Win sticking to initial choice: %f%%" noChangePct
    printfn "Win changing initial choice: %f%%" changePct
    0 
