import re

with open('src/vitambo/dispatch.clj', 'r') as f:
    lines = f.readlines()

for i, line in enumerate(lines):
    in_string = False
    paren = 0
    bracket = 0
    brace = 0
    j = 0
    while j < len(line):
        ch = line[j]
        if ch == '"':
            in_string = not in_string
        elif not in_string:
            if ch == ';' and j+1 < len(line) and line[j+1] == ';':
                break  # rest of line is comment
            elif ch == '(':
                paren += 1
            elif ch == ')':
                paren -= 1
            elif ch == '[':
                bracket += 1
            elif ch == ']':
                bracket -= 1
            elif ch == '{':
                brace += 1
            elif ch == '}':
                brace -= 1
            elif ch == '#' and j+1 < len(line) and line[j+1] == '{':
                brace += 1
                j += 1  # skip the {
        j += 1
    
    net = paren + bracket + brace
    if net != 0 or brace != 0:
        print(f'L{i+1:4d} p={paren:+3d} b={bracket:+3d} br={brace:+3d} net={net:+3d}: {line.rstrip()[:60]}')
