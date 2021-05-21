'''
File: po-profiling.py
Project: profiling
File Created: Thursday, 13th May 2021 8:39:16 pm
Author: zyk
-----
Last Modified: Friday, 21st May 2021 4:37:48 pm
Modified By: zyk
-----
2021 - HUST
'''

import csv

same_addr = list()
any_addr = list()
names = list()

with open("./profiling/po-profiling.csv", newline='\n') as csvfile:
  reader = csv.reader(csvfile, delimiter=",")
  line = 1
  
  for row in reader:
    if line == 2:
      names = row[1:]
    elif line >= 4 and line <= 7:
      # any address
      any_addr.append(row[1:])
    elif line >= 9 and line <= 12:
      # same address
      same_addr.append(row[1:])

    line += 1


f_same = open('./plots-data/po-same-addr-timing.tex', 'w', encoding='utf-8')
for arch in range(4):
  f_same.write("\\addplot coordinates {\n")
  for (name, time_ms, idx) in zip(names, same_addr[arch], range(len(names))):
      # \addplot coordinates { (name,t1) (name t2) ... };
      f_same.write("(" + str(idx) + "," + str(time_ms) + ")\n")

  f_same.write("(" + str(len(names)) + ",0)\n")
  f_same.write("};\n")
f_same.write("\legend{WR,rWR,rWM,rMM}\n")
f_same.write("% xticklabels={" + ','.join(names) + "},")
f_same.close()


f_any = open('./plots-data/po-any-addr-timing.tex', 'w', encoding='utf-8')
for arch in range(4):
  f_any.write("\\addplot coordinates {\n")
  for (name, time_ms, idx) in zip(names, any_addr[arch], range(len(names))):
      # \addplot coordinates { (name,t1) (name t2) ... };
      f_any.write("(" + str(idx) + "," + str(time_ms) + ")\n")

  f_any.write("(" + str(len(names)) + ",0)\n")
  f_any.write("};\n")
f_any.write("\legend{WR,rWR,rWM,rMM}\n")
f_any.write("% xticklabels={" + ','.join(names) + "},")
f_any.close()
