def read_protein():
  s = open("proteins.txt").read()
  codon_map = {}
  for l in s.splitlines():
    name, rest = l.split(None, 1)
    codons = rest.split(", ")
    for c in codons:
      codon_map[c] = name
  return codon_map



codon_map = read_protein()
while True:
  inp = raw_input()
  if not inp:
    break
  print codon_map[inp]
