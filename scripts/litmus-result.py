'''
File: litmus-result.py
Project: profiling
File Created: Thursday, 13th May 2021 11:16:54 pm
Author: zyk
-----
Last Modified: Saturday, 22nd May 2021 6:41:17 pm
Modified By: zyk
-----
2021 - HUST
'''

import csv
import sys

assert(len(sys.argv) == 2)

archtype = 5

bar = open('./plots-data/litmus-bar-data.tex', 'w', encoding='utf-8')
count = [{"eq": 0, "st": 0} for i in range(archtype)] # used in bar graph, count eq and st times for each arch


matrix = open('./plots-data/litmus-matrix-data.tex', 'w', encoding='utf-8')

# convert str val to matrix data (0 or 1)
def toC(v: str):
  if v == "eq":
    return 0
  else:
    return 1

xticklabels = list()
yticklabels = list() # y for matrix & x for bar (should be labels for arch, e.g. WR, rWR, ...)

with open(sys.argv[1], newline='\n') as csvfile:
  reader = csv.reader(csvfile, delimiter=",")
  line = 1

  data = [list() for i in range(archtype)] # indexed by y
  for row in reader:
    if line == 2:
      yticklabels = row[1:]
    elif line != 1:
      # name + result
      xticklabels.append(row[0])
      x = line - 3 # in matrix graph
      for (r, y) in zip(row[1:], range(archtype)):
        count[y][r] += 1
        data[y].append(' '.join(map(str, [x, y, toC(row[y + 1])])) + '\n') # matrix data
    line += 1

for k in ['eq', 'st']:
  bar.write("\\addplot+ [ybar] coordinates {\n")
  for i in range(archtype):
    bar.write('(' + yticklabels[i] + ',' + str(count[i][k]) + ')\n')
  bar.write('};\n')
bar.write("\\legend{Equal,Stricter}\n")
bar.write("% symbolic x coords={" + ','.join(yticklabels) + "},\n")
bar.close()

matrix.write('table [meta=C] {\n')
matrix.write("x y C\n")
for y in range(archtype):
  matrix.writelines(data[y])
matrix.write("% xticklabels={0," + ','.join(xticklabels) + "},\n")
matrix.write("% yticklabels={0," + ','.join(yticklabels) + "},\n")
matrix.write("% mesh/cols=" + str(len(xticklabels)) + ',\n')
matrix.write('};\n')
matrix.close()