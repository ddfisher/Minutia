parens = ["()", "[]", "{}"]

def is_balanced(s):
  if not s:
    return True
  for p in parens:
    if p in s:
      return is_balanced(s.replace(p, "", 1))
    
  return False
