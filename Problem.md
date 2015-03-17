# Introduction #

This page contains the description of the problem


# Problem description #

Consider a single large text file F and a system that consists of N computer
nodes. Split the file F into M fragments F\_1, F\_2, … , F\_M and place them at various
nodes of the system. Place each fragment at one or more nodes (that means that you
should replicate some of the fragments). The fragments may not all have the same
size.
Perform the following computational tasks:

1. Find the longest word in F and store it at all the nodes. [Points](100.md)
2. For specific word(s) find in which nodes this/those word(s) is/are stored. Start this task at node 1, with the user specifying the word(s). [Points](150.md)
3. Find the most frequent word in the text file F and store it at node 1. [Points](200.md)
4. Update the contents of fragment i at each node that may have a copy of it.

The update originates at node 1, with the user specifying the fragment
number. [Points](100.md)-(Optional for extra credits)

> In doing these computations, our users are willing to trade-off accuracy for latency and communication costs. The highest accuracy for given latency and
communication costs is highly desired.

### Expectations: ###
> Design and develop a distributed application that performs these
computational tasks. Your solution must utilize gossip and be robust, efficient, and
effective at solving the problem(s) at hand. Your application should be implemented
using the Erlang distributed computing system. You should submit the file(s) of your
implementation for this project. However, please make sure that you also provide a
MAKEFILE for the TA to compile and test your code. Also you should submit a
well-written technical report documenting, explaining, and justifying your
application, problem, and design respectively. Your report should also include a
thoughtful experimental analysis of the effectiveness and efficiency of your solution
to the problem. You should conduct experiments with varying large number of nodes
N, which are assigned to/instantiated at least two networked virtual machines.

### Submission Procedure: ###
> Due date: 11:59pm on Wednesday , 12/04/13
> This is a group project
> Write the names of each member of the group in the report.
Submit the project, using pdf format for the report, and a zip file for your
code, via e-mail to: nikbar1@umbc.edu