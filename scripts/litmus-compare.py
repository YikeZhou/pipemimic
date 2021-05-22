'''
File: litmus-compare.py
Project: profiling
File Created: Friday, 21st May 2021 3:07:44 pm
Author: zyk
-----
Last Modified: Saturday, 22nd May 2021 6:27:46 pm
Modified By: zyk
-----
2021 - HUST
'''

import csv

# read all names
names = list()
with open("./profiling/litmus-result.csv", newline='\n') as csvfile:
  reader = csv.reader(csvfile, delimiter=',')
  line = 1
  for row in reader:
    if line == 1 or line == 2:
      line += 1
      continue
    else:
      names.append(row[0])

# find result in herd logs
result = dict()
updatetime = 0
with open("./litmus-tests-riscv/model-results/herd.logs", "r") as std:
  for line in std.readlines():
    if line.startswith('Observation'):
      info = line.split(' ')
      testname = info[1]
      if testname in names:
        result[testname] = info[2] == 'Sometimes'
        updatetime += 1
assert(updatetime == len(result)) # check if duplicate entry exists in logs file

# append litmus-result with standard model result
with open("./profiling/litmus-result.csv", newline='\n') as csvfile:
  reader = csv.reader(csvfile, delimiter=',')
  line = 1
  ref = open("./profiling/litmus-result-std.csv", "w")
  for row in reader:
    if line == 1 or line == 2:
      ref.write(','.join(row))
      if (line == 2):
        ref.write(',Ref\n')
      else:
        ref.write('\n')
      line += 1
      continue
    else:
      # write origin row with ref at the end
      ref.write(','.join(row))
      ref.write(',' + ('eq' if result[row[0]] else 'st') + '\n')
  ref.close()

labels = list()
with open("./profiling/litmus-result.csv", newline='\n') as csvfile:
  output = open("./profiling/lt-compare-result.csv", "w")
  bar = open("./plots-data/lt-compare-result.tex", "w")
  # record for each arch
  count = [{"eq": 0, "st": 0} for i in range(4)]

  reader = csv.reader(csvfile, delimiter=',')
  line = 1
  
  for row in reader:
    if line == 1 or line == 2:
      output.write(','.join(row) + '\n')
      line += 1
      if row[0][0] != '#':
        labels = row[1:]
      continue
    else:
      buf = list()
      testname = row[0]
      buf.append(testname)
      for (obs, idx) in zip(row[1:], range(4)):
        if (obs == 'eq' and result[testname]) or (obs == 'st' and (not result[testname])):
          # same as standard model
          buf.append('y')
          count[idx]['eq'] += 1
        else:
          if (not result[testname]) and obs == 'eq':
            print("\033[47;30mError: " + testname + ' ' + labels[idx] + '\033[0m')
          else:
            print("Stricter: " + testname + ' ' + labels[idx])
          # stricter than standard model
          buf.append('n')
          count[idx]['st'] += 1
      output.write(','.join(buf) + '\n')
  
  output.close()

  for k in ['st', 'eq']:
    bar.write("\\addplot+ [ybar] coordinates {\n")
    for i in range(4):
      bar.write('(' + labels[i] + ',' + str(count[i][k]) + ')\n')
    bar.write('};\n')
  bar.write("\\legend{Stricter,Equal}\n")
  bar.write("% symbolic x coords={" + ','.join(labels) + "},\n")
  bar.close()