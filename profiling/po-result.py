'''
File: po-result.py
Project: profiling
File Created: Thursday, 13th May 2021 11:21:45 pm
Author: zyk
-----
Last Modified: Friday, 14th May 2021 10:03:21 am
Modified By: zyk
-----
2021 - HUST
'''

import csv

legend = "\\legend{preserved, not-preserved}\n"
bar_any = open('./profiling/po-any-addr-bar-data.tex', 'w', encoding='utf-8')
any_count = [{"y": 0, "n": 0} for i in range(4)]
bar_same = open('./profiling/po-same-addr-bar-data.tex', 'w', encoding='utf-8')
same_count = [{"y": 0, "n": 0} for i in range(4)]

matrix_any = open('./profiling/po-any-addr-matrix-data.tex', 'w', encoding='utf-8')
matrix_any.write("x y C\n")

matrix_same = open('./profiling/po-same-addr-matrix-data.tex', 'w', encoding='utf-8')
matrix_same.write("x y C\n")

def toC(v: str):
  if v == "y":
    return 0
  else:
    return 1

xticklabels = list()
yticklabels_any = list()
yticklabels_same = list()

with open('./profiling/po-result.csv', newline='\n') as csvfile:
  reader = csv.reader(csvfile, delimiter=",")
  line = 1

  for row in reader:
    if line == 2:
      xticklabels = row[1:]
    
    elif line >= 4 and line <= 7:
      # any address
      yticklabels_any.append(row[0])
      y = line - 4
      for (value, x) in zip(row[1:], range(4)):
        if (value == "y"):
          # update (row[0], y) in any_count
          any_count[y]["y"] += 1
        else:
          # update (row[0], n) in any_count
          any_count[y]["n"] += 1
        matrix_any.write(' '.join(map(str, [x, y, toC(value)])))
        matrix_any.write('\n')
    
    elif line >= 9 and line <= 12:
      # same address
      yticklabels_same.append(row[0])
      y = line - 9
      for (value, x) in zip(row[1:], range(4)):
        if (value == "y"):
          # update (row[0], y) in same_count
          same_count[y]["y"] += 1
        else:
          # update (row[0], n) in same_count
          same_count[y]["n"] += 1
        matrix_same.write(' '.join(map(str, [x, y, toC(value)])))
        matrix_same.write('\n')
    
    line += 1



for k in ["y", "n"]:
  bar_any.write("\\addplot+ [ybar] coordinates {\n")
  for i in range(4):
    bar_any.write('(' + yticklabels_any[i] + ',' + str(any_count[i][k]) + ')\n')
  bar_any.write("};\n")
bar_any.write(legend)
bar_any.write("% symbolic x coords={" + ','.join(yticklabels_any) + "},\n")
bar_any.close()

for k in ["y", "n"]:
  bar_same.write("\\addplot+ [ybar] coordinates {\n")
  for i in range(4):
    bar_same.write('(' + yticklabels_same[i] + ',' + str(same_count[i][k]) + ')\n')
  bar_same.write("};\n")
bar_same.write(legend)
bar_same.write("% symbolic x coords={" + ','.join(yticklabels_same) + "},\n")
bar_same.close()

matrix_any.write("% xticklabels={0," + ','.join(xticklabels) + "},\n")
matrix_any.write("% yticklabels={0," + ','.join(yticklabels_any) + "},\n")
matrix_any.close()

matrix_same.write("% xticklabels={0," + ','.join(xticklabels) + "},\n")
matrix_same.write("% yticklabels={0," + ','.join(yticklabels_same) + "},\n")
matrix_same.close()