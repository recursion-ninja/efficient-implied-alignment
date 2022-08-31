#!/bin/python3

import math
import matplotlib.pyplot as plt
import numpy as np
import sys


log_scale = True

if len(sys.argv) < 2:
  exit("Must provide CSV file path")

# Read in the raw data from the CVS file
timings = np.genfromtxt (sys.argv[1], delimiter=",")

# x & y values are ordinal in the range [0..n] X [0..k]
# x & y labels are used to made the graph inteligeble
# Likewise the log_2 of the z values (runtimes) is taken
# and the z-tick labels are used to bake the graph inteligeble.
xs  = timings[:,0]
ys  = timings[:,1]
xsl = timings[:,2]
ysl = timings[:,3]
zs  = timings[:,4]
fig = plt.figure()
ax  = fig.add_subplot(111, projection='3d')

# Take the log_2 of the z values
if log_scale:
  for i in range(len(timings)):
    for j in range(len(timings[i])):
      if j == 4:
        timings[i,j] = math.log(timings[i,j],4)

# Set bounds for the 3D graph.
# Without this, the plot will not correctly render the boxes of each data point
# The bottom of the boxes will not exist and we render planes of width 0.
# These planes are essentially invisible and therefore useless.
bottom = np.zeros_like(zs)
width  = depth = 1

# Create ranges of the form [0..n] and [0..k] based on the tick labels.
my_xticks = list(map(str, map(int, sorted(set(xsl.tolist())))))
my_yticks = list(map(str, map(int, sorted(set(ysl.tolist())))))
my_xticks.insert(0,'0')

ax.set_yticks(ys)
ax.set_zticks(range(2,15,2))
ax.set_xticklabels(my_xticks)
ax.set_yticklabels(my_yticks)
ax.set_zticklabels(['2²','2⁴','2⁶','2⁸','2¹⁰','2¹²','2¹⁴'])
ax.set_xlabel('Count (k)')
ax.set_ylabel('Length (n)')
ax.set_zlabel('Milliseconds')
ax.set_zlim(0, 12)

ax.bar3d(xs, ys, bottom, width, depth, zs, shade=True)
ax.view_init(40, 225)

# Show or save the image
if len(sys.argv) < 3:
  plt.show()
else:
  plt.savefig(sys.argv[2], format='eps', dpi=1200)

