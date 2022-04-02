module Bandwagoner

type Coach ={
   Name:string
   FormerPlayer: bool
}


type Stats = {
   Wins: int
   Losses: int
}

type Team = {
   Name: string
   Coach: Coach
   Stats: Stats
}

let createCoach (name: string) (formerPlayer: bool): Coach =
    {Name=name;FormerPlayer=formerPlayer}

let createStats(wins: int) (losses: int): Stats =
   {Wins=wins;Losses=losses}

let createTeam(name: string) (coach: Coach)(stats: Stats): Team =
  {Name=name;Coach=coach;Stats=stats}

let replaceCoach(team: Team) (coach: Coach): Team =
   {team with Coach=coach}

let isSameTeam(homeTeam: Team) (awayTeam: Team): bool =
   homeTeam = awayTeam

let itsGreggsTeam team = team.Coach.Name = "Gregg Popovich"
let isFormerPlayer team = team.Coach.FormerPlayer
let isChicago team = team.Name = "Chicago Bulls"
let temHasMOreLossesThanWins team = team.Stats.Losses > team.Stats.Wins

let reasonsToRoot = 
   [
      itsGreggsTeam
      isFormerPlayer
      fun x -> x.Stats.Wins >= 60
      temHasMOreLossesThanWins

   ]

let rootForTeam(team: Team): bool =
   reasonsToRoot |> List.exists (fun p -> p team)
