'''
File: litmus-profiling.py
Project: profiling
File Created: Thursday, 13th May 2021 7:54:18 pm
Author: zyk
-----
Last Modified: Friday, 14th May 2021 2:12:34 pm
Modified By: zyk
-----
2021 - HUST
'''

import csv

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



split = 4 # split into 4 files
assert len(matrix) % split == 0
segment = int(len(matrix) / split)
print("segment: " + str(segment))

for i in range(split):
  filename = './profiling/litmus-timing-' + str(i) + '.tex'
  f = open(filename, 'w', encoding='utf-8')

  xticklabels = list()
  
  for arch in range(4):
    f.write("\\addplot coordinates {\n")

    for (row, idx) in zip(matrix[i * segment:(i + 1) * segment], range(segment)):
      name = row[0] 
      time_ms = row[arch + 1]
      
      # \addplot coordinates { (name,t1) (name t2) ... };
      f.write("(" + str(idx) + "," + str(time_ms) + ")\n")
      # print(idx, name)
      xticklabels.append(name)
    
    f.write("(" + str(segment) + ",0)\n") # redundant column
    f.write("};\n")
    
  if i == 0:
    f.write("\\legend{" + ','.join(legend) + "}\n") # only show legend on top
  f.write("% xticklabels={" + ','.join(xticklabels) + "},")
  f.close()