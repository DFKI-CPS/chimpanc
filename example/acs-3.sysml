bdd [package] acs [ACS]
-----------------------------------------------------------

block Building
  references
    gate: Building[*] <- building
      derive: org_dom.dest
    building: Building[*] <- gate
    org_dom: Door[*] <- org
    dest_dom: Door[*] <- dest
  constraints
    inv: not (self.gate->includes(self))

block Person
  references
    aut: Building[*]
    sit: Building[1] { subsets aut }
  constraints
    inv: self.aut->forAll(b|self.aut.building->includes(b))

block Door
  operations
    enter(p: Person)
      pre: p.aut->includes(dest)
      pre: p.sit = org
      post: p.sit = dest
  references
    org:  Building[1] <- org_dom
    dest: Building[1] <- dest_dom
  ports
    incoming: Person
  owned behaviors
    state machine EnterBehavior
      initial state Waiting
        receive incoming(p) / enter(p) -> Waiting