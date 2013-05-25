import sys

def read_codons():
  s = open(sys.argv[1]).read()
  lst = s.split()
  result = {}
  while (lst):
    name = lst.pop(0)
    freq = float(lst.pop(0))
    result[name] = freq
  return result

def read_protein():
  s = open("proteins.txt").read()
  protein_map = {}
  for l in s.splitlines():
    name, rest = l.split(None, 1)
    codons = rest.split(", ")
    protein_map[name] = codons
  return protein_map

def find_best_codon(codons, codon_map):
  return max(codons, key=lambda c: codon_map[c])

def process_input(protein_map, codon_map):
  s = raw_input()
  proteins = s.split("-")
  codons = []
  for p in proteins:
    codons.append(find_best_codon(protein_map[p], codon_map))
  print "-".join(codons)

def main():
  codon_map = read_codons()
  protein_map = read_protein()
  process_input(protein_map, codon_map)

if __name__ == '__main__':
  main()
