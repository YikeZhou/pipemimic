'''
File: litmus-profiling.py
Project: profiling
File Created: Thursday, 13th May 2021 7:54:18 pm
Author: zyk
-----
Last Modified: Thursday, 13th May 2021 10:59:27 pm
Modified By: zyk
-----
2021 - HUST
'''

import csv

f = open('./profiling/litmus-timing.tex', 'w', encoding='utf-8')

legend = list()
matrix = list()

with open("./profiling/litmus-profiling.csv", newline='\n') as csvfile:
  reader = csv.reader(csvfile, delimiter=",")
  
  for row in reader:
    if row[0][0] == '#':
      continue
    elif row[0] == "test":
      legend = row[1:]
    else:
      # name + time(ms)
      matrix.append(row)

xticklabels = list()
for arch in range(4):
  f.write("\\addplot coordinates {\n")

  for (row, idx) in zip(matrix, range(len(matrix))):
    name = row[0]
    time_ms = row[arch + 1]
    
    # \addplot coordinates { (name,t1) (name t2) ... };
    f.write("(" + str(idx) + "," + str(time_ms) + ")\n")
    print(idx, name)
    xticklabels.append(name)
  f.write("(" + str(len(matrix)) + ",0)\n")
  f.write("};\n")
  
f.write("\\legend{" + ','.join(legend) + "}\n")
f.write("% xticklabels={" + ','.join(xticklabels) + "},")
f.close()