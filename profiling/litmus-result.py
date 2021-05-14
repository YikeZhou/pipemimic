'''
File: litmus-result.py
Project: profiling
File Created: Thursday, 13th May 2021 11:16:54 pm
Author: zyk
-----
Last Modified: Friday, 14th May 2021 10:34:52 am
Modified By: zyk
-----
2021 - HUST
'''

import csv

bar = open('./profiling/litmus-bar-data.tex', 'w', encoding='utf-8')
count = [{"eq": 0, "st": 0} for i in range(4)]


matrix = open('./profiling/litmus-matrix-data.tex', 'w', encoding='utf-8')
matrix.write("x y C\n")

def toC(v: str):
  if v == "eq":
    return 0
  else:
    return 1

xticklabels = list()
yticklabels = list() # y for matrix & x for bar (should be labels for arch, e.g. WR, rWR, ...)

with open('./profiling/litmus-result.csv', newline='\n') as csvfile:
  reader = csv.reader(csvfile, delimiter=",")
  line = 1

  for row in reader:
    if line == 1:
      line += 1
      continue
    elif line == 2:
      yticklabels = row[1:]
    else:
      # name + result
      xticklabels.append(row[0])
      x = line - 3
      for (r, y) in zip(row[1:], range(4)):
        count[y][r] += 1
        matrix.write(' '.join(map(str, [x, y, toC(row[y + 1])])) + '\n')
    line += 1

for k in ['eq', 'st']:
  bar.write("\\addplot+ [ybar] coordinates {\n")
  for i in range(4):
    bar.write('(' + yticklabels[i] + ',' + str(count[i][k]) + ')\n')
  bar.write('};\n')
bar.write("\\legend{equal,stricter}\n")
bar.write("% symbolic x coords={" + ','.join(yticklabels) + "},\n")
bar.close()


matrix.write("% xticklabels={0," + ','.join(xticklabels) + "},\n")
matrix.write("% yticklabels={0," + ','.join(yticklabels) + "},\n")
matrix.write("% mesh/cols=" + str(len(xticklabels)) + ',\n')
matrix.close()