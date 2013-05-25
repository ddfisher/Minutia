# python implementation of "A Linear Algorithm For Generating Random Numbers With a Given Distribution"
# see: http://web.eecs.utk.edu/~vose/Publications/random.pdf
import random

def init(vals):
  processed = []

  large = []
  small = []
  n = len(vals)
  for (v, p) in vals:
    if p > 1./n:
      large.append((v,p))
    else:
      small.append((v,p))

  while large and small:
    (s_val, s_prob) = small.pop()
    (l_val, l_prob) = large.pop()
    processed.append( (n * s_prob, s_val, l_val) )
    l_prob += s_prob - 1./n

    if l_prob > 1./n:
      large.append( (l_val, l_prob) )
    else:
      small.append( (l_val, l_prob) )

  for (v, p) in small + large:
    processed.append( (1., v, v) )


  def rand():
    (p, val, alias) = random.choice(processed)
    if random.random() < p:
      return val
    else:
      return alias

  return rand
