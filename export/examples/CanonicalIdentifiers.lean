import CCLib.Positive

namespace CanonicalIdentifiers

-- Independent numeric values from Camlcoq.pos_of_string, including the
-- named atoms hit by the old temporary overflow.
theorem character_classes :
    CC.identOfString "0" = CC.Positive.ofNat 64 ∧
    CC.identOfString "9" = CC.Positive.ofNat 73 ∧
    CC.identOfString "a" = CC.Positive.ofNat 74 ∧
    CC.identOfString "z" = CC.Positive.ofNat 99 ∧
    CC.identOfString "A" = CC.Positive.ofNat 100 ∧
    CC.identOfString "Z" = CC.Positive.ofNat 125 ∧
    CC.identOfString "_" = CC.Positive.ofNat 126 ∧
    CC.identOfString "$" = CC.Positive.ofNat 18751 ∧
    CC.identOfString "@" = CC.Positive.ofNat 20543 := by decide +kernel

theorem overflow_alias_names :
    CC.identOfString "op" = CC.Positive.ofNat 5720 ∧
    CC.identOfString "sp" = CC.Positive.ofNat 5724 ∧
    CC.identOfString "lr" = CC.Positive.ofNat 5845 ∧
    CC.identOfString "sk" = CC.Positive.ofNat 5404 ∧
    CC.identOfString "cb" = CC.Positive.ofNat 4812 := by decide +kernel

end CanonicalIdentifiers
