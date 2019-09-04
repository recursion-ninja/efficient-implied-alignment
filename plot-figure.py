from mpl_toolkits import mplot3d
import math
import matplotlib.pyplot as plt
import numpy as np
import sys


log_scale = True

if len(sys.argv) < 2:
  exit("Must provide CSV file path")

timings = np.genfromtxt (sys.argv[1], delimiter=",")
xs  = timings[:,0]
ys  = timings[:,1]
xsl = timings[:,2]
ysl = timings[:,3]
zs  = timings[:,4]
fig = plt.figure()
ax  = fig.add_subplot(111, projection='3d')

if log_scale:
#  ax.set_zscale('log',basex=2)
  for i in range(len(timings)):
    for j in range(len(timings[i])):
      if j == 4:
        timings[i,j] = math.log(timings[i,j],4)

bottom = np.zeros_like(zs)
width  = depth = 1

my_xticks = list(map(str, map(int, sorted(set(xsl.tolist())))))
my_yticks = list(map(str, map(int, sorted(set(ysl.tolist())))))
my_xticks.insert(0,0)

ax.set_yticks(ys)
ax.set_xticklabels(my_xticks)
ax.set_yticklabels(my_yticks)
ax.set_xlabel('|Leafset|')
ax.set_ylabel('|Sequence|')
ax.set_zlabel('log₂ Milliseconds')

ax.bar3d(xs, ys, bottom, width, depth, zs, shade=True)
ax.view_init(30, 225)
plt.show()
