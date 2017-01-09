bdd [package] acs [ACS]
-----------------------------------------------------------

block Building
  operations
    enter(p: Person)
      pre:  p.aut->includes(self)
      pre:  p.sit.gate->includes(self)
      post: p.sit = self
  references
    gate: Building[*] <- building
    building: Building[*] { unique, nonunique } <- gate
  ports
    incoming: Pers
  owned behaviors
    state machine EnterBehavior -- dest_dom::DoorBehavior
      initial state Waiting
        receive incoming(p) / enter(p) -> Waiting

block Person
  references
    aut: Building[*]
    sit: Building[1] { subsets aut }