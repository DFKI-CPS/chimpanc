bdd [package] acs [ACS]
-----------------------------------------------------------

-- P6: The geometry of buildings serves to define which
-- buildings can communicate between each other, and in
-- which direction.
block Building
  references
    gate: Building[*] <- building
    building: Building[*] <- gate
  constraints
    -- P7: A building does not communicate with itself.
    inv: not (self.gate->includes(self))

block Person
  operations
     passage(b: Person)
      pre: self.aut->includes(b)
      -- P8: A person can only go from one building in
      -- which he is to another where he desires to go if
      -- these two buildings communicate with each other.
      pre: self.sit.gate->includes(b)
      post: self.sit = b
  references
    aut: Building[*]
    sit: Building[1] { subsets aut }
  constraints
    -- P10: Any person authorised to be in a building must
    -- also be authorised to go into another building which
    -- communicates with the first one.
    inv: self.aut->forAll(b|self.aut.building->includes(b))