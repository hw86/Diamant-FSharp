// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Functions

[<EntryPoint>]

let main argv = 
    
    printfn "%A" argv

    let games = [| 1 .. 1 .. 10000|]
    let winners = 
        games
        |> Array.map (fun ii -> 
            let winner = Functions.Game.playGame ii
            winner)

    let countVictories (input:Strategy) (victors:Player[]) = 
        victors
        |> Array.filter(fun player -> player.Strategy = input)
        |> Array.length

    let countJems (input:Strategy) (victors:Player[]) = 
        victors
        |> Array.filter (fun player -> player.Strategy = input)
        |> Array.map (fun player -> float (player.bankedJems))
        |> Array.average
        
    
    printfn "%A" ("Random Robber won " + (((float (countVictories RandomRobber winners))/10000.0)*100.0).ToString() + "times")

    printfn "%A" ("Greedy Grabber won " + (((float (countVictories GreedyGrabber winners))/10000.0)*100.0).ToString() + "times")

    printfn "%A" ("Timid Thief won " + (((float (countVictories TimidThief winners))/10000.00)*100.00).ToString() + "times")

    printfn "%A" ("Plucky Plunderer won " + (((float (countVictories PluckyPlunderer winners))/10000.00)*100.00).ToString() + "times")
   
    printfn "%A" ("Random Robber mean winning " + (countJems RandomRobber winners).ToString())

    printfn "%A" ("Timid Thief mean winning " + (countJems TimidThief winners).ToString())

    printfn "%A" ("Plucky Plunderer mean winning " + (countJems PluckyPlunderer winners).ToString())

    printfn "%A" ("Greedy Grabber mean winning " + (countJems GreedyGrabber winners).ToString())
   
    0 // return an integer exit code
