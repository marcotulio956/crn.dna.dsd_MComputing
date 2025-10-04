from biocrnpyler import *
# let's build the following CRN
# A -->[k1] 2B
# B -->[k2] C+D
# Species
A = Species("A")
B = Species("B")
C = Species("C")
D = Species("D")

#Reaction Rates
k1 = 3.
k2 = 1.4

#Reaction Objects
R1 = Reaction.from_massaction([A], [B, B], k_forward = k1)
R2 = Reaction.from_massaction([B], [C, D], k_forward = k2)

#Make a CRN
CRN = ChemicalReactionNetwork(species = [A, B, C, D], reactions = [R1, R2])
print(CRN)