namespace Functions


type Trap = 
    | Spiders
    | Lava
    | Boulder
    | Spikes
    | Snakes

type CardType = 
    | TrapCard
    | CaveCard

type CardValue = 
    | Jems of int
    | Trap of Trap

type Card (Card_Type:CardType, Card_Value:CardValue) = 
    member this.Card_Type = Card_Type
    member this.Card_Value = Card_Value
    [<DefaultValue>] val mutable InDeck: bool
    [<DefaultValue>] val mutable InPlay: bool
    

type Strategy = 
    | PluckyPlunderer
    | GreedyGrabber
    | TimidThief
    | RandomRobber

type Player(name:string, strategy:Strategy) = 
    member this.Name = name
    [<DefaultValue>] val mutable currentJems:int
    [<DefaultValue>] val mutable bankedJems:int
    [<DefaultValue>] val mutable inCave: bool
    member this.Strategy = strategy

module Deck = 
    
    
    let shuffleArr(input:bool[]) = 

        let rand = new System.Random(Settings.RandomSeed.random_seed)
        Settings.RandomSeed.random_seed <- Settings.RandomSeed.random_seed + 1

        let swap (arr:bool[]) x y = 
            let tmp = arr.[x]
            arr.[x] <- arr.[y]
            arr.[y] <- tmp
        
        Array.iteri (fun i _ -> swap input i (rand.Next(i, Array.length input))) input
        input

    let shuffle(input:Card[]) = 

        let rand = new System.Random(Settings.RandomSeed.random_seed)
        Settings.RandomSeed.random_seed <- Settings.RandomSeed.random_seed + 1

        let swap (deck:Card[]) x y = 
            let tmp = deck.[x]
            deck.[x] <- deck.[y]
            deck.[y] <- tmp
        
        Array.iteri (fun i _ -> swap input i (rand.Next(i, Array.length input))) input
        input



    
    let makeDeck (gameInd:int) = 
        let deck = 
            [|
                Card(CaveCard,Jems 1);
                Card(CaveCard,Jems 2);
                Card(CaveCard,Jems 3);
                Card(CaveCard,Jems 4);
                Card(CaveCard,Jems 5);
                Card(CaveCard,Jems 5);
                Card(CaveCard,Jems 7);
                Card(CaveCard,Jems 7);
                Card(CaveCard,Jems 9);
                Card(CaveCard,Jems 11);
                Card(CaveCard,Jems 11);
                Card(CaveCard,Jems 13);
                Card(CaveCard,Jems 14);
                Card(CaveCard,Jems 15);
                Card(CaveCard,Jems 17);
                Card(TrapCard, Trap Spiders)
                Card(TrapCard, Trap Spiders)
                Card(TrapCard, Trap Spiders)
                Card(TrapCard, Trap Snakes);
                Card(TrapCard, Trap Snakes);
                Card(TrapCard, Trap Snakes);
                Card(TrapCard, Trap Lava);
                Card(TrapCard, Trap Lava);
                Card(TrapCard, Trap Lava);
                Card(TrapCard, Trap Boulder);
                Card(TrapCard, Trap Boulder);
                Card(TrapCard, Trap Boulder);
                Card(TrapCard, Trap Spikes);
                Card(TrapCard, Trap Spikes);
                Card(TrapCard, Trap Spikes);
            |]
        deck |> Array.map (fun f -> f.InDeck <- true) |> ignore
        deck |> Array.map (fun f -> f.InPlay <- false) |> ignore
        (deck |> Array.copy)

module Game  = 
    

    let getJems (currentCard:Card) = 

        match currentCard.Card_Value with 
        | Jems j -> j
        | _ -> 0

    let checkPlayersInCave (players:Player[]) = 
        let remainingPlayers = 
            players
            |> Array.filter (fun player -> player.inCave = true)
            |> Array.length

        remainingPlayers > 0

    let TimidThiefDecision (player:Player) (targetJems:int)=         

        if player.currentJems >= targetJems then 
            false
        else
            true
    
    let RandomRobberDecision (currentCards:int) (maxCards:int)= 
        
        let noIn = 
            match maxCards > currentCards with 
            | true ->  maxCards - currentCards
            | _ -> 0

        let decision = 
            (Array.init currentCards (fun _ -> false))
            |> Array.append (Array.init noIn (fun _ -> true))
            |> Deck.shuffleArr
            |> Array.head
        decision

    let PluckyPlundererDecision (currentPlayers:int) (currentUnclaimed:int) = 

        if currentPlayers < 4 && currentUnclaimed >= (currentPlayers*2) then 
            false 
        else 
            true

    let GreedyGrabberDecision (currentPlayers:int) (currentJems:int) = 
        if (currentPlayers = 1) && (currentJems > 0) then 
            false
        else
            true

    
    let makeDecision (player:Player) (players:Player[]) (currentUnclaimed:int) (jemsThisCard:int) (howManyCards:int) (maxCards:int) = 
        
        let noPlayers = 
            players
            |> Array.filter (fun player -> player.inCave = true)
            |> Array.length
        
        match player.Strategy with 
        | PluckyPlunderer -> PluckyPlundererDecision noPlayers currentUnclaimed 
        | RandomRobber -> RandomRobberDecision howManyCards maxCards
        | TimidThief -> TimidThiefDecision player 5
        | GreedyGrabber -> GreedyGrabberDecision noPlayers jemsThisCard
    
    let rounds = [|1 .. 1 ..  5|]

    let noDuplicateTraps (traps:Trap[]) = 
        let v = (traps |> Array.length) = ((traps |> Array.distinct) |> Array.length)
        v

    let matchTrapCards (card:Card) (trapToMatch:Trap) = 
        match card.Card_Value with
        | Trap tt -> tt = trapToMatch
        | _ -> false


    let playRound (players:Player[]) (deck:Card[]) (roudi:int) = 

        let current_deck = 
            Deck.shuffle deck
            |> Array.filter (fun f -> f.InDeck)
        
        players
        |> Array.map (fun player -> player.inCave <- true)
        |> ignore
        
        let  currentCardInd = ref 0
        let  currentJems = ref 0 
        let  trapsInPlay = ref Array.empty<Trap>
        let  jemsUnClaimed = ref 0
        let noCards = (deck |> Array.length) - 1

        let test1 = checkPlayersInCave players
        let test2 = currentCardInd.Value < noCards
        let test3 = noDuplicateTraps trapsInPlay.Value

        while (checkPlayersInCave players) && currentCardInd.Value < noCards && noDuplicateTraps trapsInPlay.Value do

            current_deck.[currentCardInd.Value].InPlay <- true

            
            let noPlayersIn = 
                players
                |> Array.filter (fun player -> player.inCave = true)
                |> Array.length
            
            currentJems := (getJems current_deck.[currentCardInd.Value])
            let jemsPerPlayer = currentJems.Value / noPlayersIn
            jemsUnClaimed := jemsUnClaimed.Value + (currentJems.Value % noPlayersIn)

            players
            |> Array.filter (fun player -> player.inCave = true)
            |> Array.map (fun player -> player.currentJems <- player.currentJems + jemsPerPlayer)
            |> ignore

            //printfn "Drawing and resolving card"

            // filter out duplicate traps

            match current_deck.[currentCardInd.Value].Card_Value with 
            | Trap tt -> 
                trapsInPlay := [|tt|] |> Array.append trapsInPlay.Value
                ()
            | _ -> ()

            let test1 = noDuplicateTraps trapsInPlay.Value

            let test2 = trapsInPlay.Value

            if not (noDuplicateTraps trapsInPlay.Value) then
            
                //printfn "%A" trapsInPlay.Value
                let duplicateTraps = 
                    trapsInPlay.Value
                    |> Array.distinct
                    |> Array.map (fun trap -> 
                        trapsInPlay.Value 
                        |> Array.filter (fun t -> t = trap)
                        |> Array.length)
                    |> Array.zip (trapsInPlay.Value |> Array.distinct)
                    |> Array.filter (fun tup -> (snd tup) > 1)
                    |> Array.map (fun tup -> fst tup)
                    |> Array.head

                //current_deck 
                //|> Array.filter (fun f -> f.InDeck && f.InPlay)
                //|> Array.map (fun f -> printfn "%A" (f.Card_Value.ToString()))
                //|> ignore
                let trapInd = 
                    deck 
                    |> Array.findIndex (fun card -> 
                        match card.Card_Value with 
                        | Jems jj -> false
                        | Trap tt -> 
                            if card.InDeck = true && matchTrapCards card duplicateTraps then
                                true
                            else
                                false)

                current_deck.[trapInd].InDeck <- false

                players
                |> Array.map (fun player -> player.currentJems <- 0)
                |> ignore



            else 
                let decisions = 
                    players
                    |> Array.map (fun player -> 
                        let decision = makeDecision player players jemsUnClaimed.Value currentJems.Value (currentCardInd.Value + 1) 15
                        decision)
                    |> Array.zip (players |> Array.map (fun p -> p.inCave))

                let no_leaving = 
                    decisions 
                    |> Array.filter (fun t -> 
                        match t with 
                        | (true,false) -> true
                        | _ -> false)
                    |> Array.length

            
                let sharedJems = 
                    match no_leaving with 
                    | a when a > 0 -> jemsUnClaimed.Value / no_leaving
                    | _ -> 0

                decisions
                |> Array.mapi (fun i t -> 
                    match t with 
                    | (true,false) -> 
                        jemsUnClaimed := jemsUnClaimed.Value - sharedJems
                        players.[i].currentJems <- players.[i].currentJems + sharedJems
                        players.[i].bankedJems <- players.[i].bankedJems + players.[i].currentJems
                        players.[i].currentJems <- 0
                        players.[i].inCave <- false
                    

                    | _ -> ())
                |> ignore
        
            
            
            //printfn "%A" currentCardInd.Value
            //printfn "%A" jemsUnClaimed.Value         
            let newInd = (currentCardInd.Value + 1)
            currentCardInd := newInd

            ()

        players
        |> Array.map (fun player -> player.currentJems <- 0)
        |> ignore
        
        let result = (players,current_deck)

        //players
        //|> Array.map (fun f -> printfn "%A" ("Round: " + roudi.ToString() + " " + f.Strategy.ToString() + " banked gems " + f.bankedJems.ToString() ))
        //|> ignore

        result



    let playGame (gameNumber:int) = 

        // generate shuffled deck
        printfn "%A" ("Game " + gameNumber.ToString())
        let deck = ref (Deck.makeDeck gameNumber)

        let playerOne = new Player("Player1", PluckyPlunderer)
        let playerTwo = new Player("Player2",GreedyGrabber)
        let playerThree = new Player("Player 3", TimidThief)
        let playerFour = new Player("Player4",RandomRobber)

        let mutable players = [|playerOne;playerTwo;playerThree;playerFour|]
        
        for round in rounds do 
            // iterate over cards in deck

            let roundResult = playRound players deck.Value round
            players <- fst roundResult
            deck := Deck.shuffle ((snd roundResult) |> Array.filter (fun d -> d.InDeck))
            

           
            ()


        players
        |> Array.sortByDescending (fun player -> player.bankedJems)
        |> Array.item 0

