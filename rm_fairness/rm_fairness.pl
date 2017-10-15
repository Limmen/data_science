%%
%% swi-prolog
%% compile: ['rm_fairness.pl'].
%%
%% Basic fairness means to divide resources equally among all users.
%% Basic fairness is fair but not optimal utilization.
%% Max-min fairness means to divide resources to equally first and then if some
%% user have got superflous resources the resources are divided equally among the rest
%% until no user have superflous resources. Problem with Max-min is that with many resources
%% it typically cannot distribute resources fairly and some resources go unused and there
%% might be some users even better of using their own cluster instead of sharing, i.e
%% share-incentive is lost.
%% DRF is the fairness scheme used in Mesos, it is optimized for fairness when more than 1
%% resource is involved. Basically it applied max-min to the dominant resource of each user and
%% whenever new resources come available they are given to the user with the currently lowest
%% dominant share. I.e in DRF just as in Max-min there might be that there are a set of un-used
%% resources because the optimization problem was not solved perfectly, when new resource become
%% available Mesos first check if the demands of the user with the lowest dominant resourcers could
%% be met with the new resources, then with the next lowest.. and so on.
%% I.e DRF equalizes dominant shares among users, it does not equalize total resources,
%% this ensures better utilization and share incentive, a user cannot get a higher share of
%% the dominant resource in a static partitioning of the cluster.
%% Extension of DRF is weighted DRF where each user has a associated weight.
%% both Basic, Max-min and DRF are strategy-proof since a user cannot get its share faster
%% by lying, it can only get superflous resources by lying.
%%
%% Commands:
%%
%% Compute DRF for the example in the paper by Ghodsi et al:
%% user1(X1), user2(X2), total(Tot), drf(X1, X2, Tot, (User1Tasks, User2Tasks)).
%%
%% Compute Basic fairness for the example in the paper by Ghodsi et al:
%% user1(X1), user2(X2), total(Tot), basic_fairness(X1, X2, Tot, (User1Tasks, User2Tasks)).
%%
%% Compute max-min fairness for the example in the paper by Ghodsi et al:
%% user1(X1), user2(X2), total(Tot), max_min_fairness(X1, X2, Tot, (User1Tasks, User2Tasks)).
%%
%% Author Kim Hammar limmen@github.com <kimham@kth.se>

%%%===================================================================
%%% Imports
%%%===================================================================

:- use_module(library(clpfd)).
:- use_module(library(clpq)).

%%%===================================================================
%%% Facts
%%%===================================================================

user1(demand(cpu(1), mem(4))).

user2(demand(cpu(3), mem(1))).

total(total(cpu(9), mem(18))).

%%%===================================================================
%%% Predicates
%%%===================================================================

%% Computes the basic fairness allocation for User1 and User2 given set of TotalResources
%% max_min_fairness(+,+,+,-).
%% max_min_fairness(User1, User2, TotalResources, TaskAllocation).
max_min_fairness(demand(cpu(C1),mem(M1)),demand(cpu(C2),mem(M2)), total(cpu(CT), mem(MT)), allocation(U1Tasks, U2Tasks)):-
	U1Tasks in 0..50,
	U2Tasks in 0..50,
	U1Tasks * C1 + U2Tasks * C2 #=< CT,
	U1Tasks * M1 + U2Tasks * M2 #=< MT,
	Diff #= abs((U1Tasks*C1 + U1Tasks*M1) - (U2Tasks*C2 + U2Tasks*M2)),
	labeling([max(U1Tasks+U2Tasks), min(Diff)], [U1Tasks, U2Tasks, Diff]).

%% Computes the basic fairness allocation for User1 and User2 given set of TotalResources
%% basic_fairness(+,+,+,-).
%% basic_fairness(User1, User2, TotalResources, TaskAllocation).
basic_fairness(demand(cpu(C1),mem(M1)),demand(cpu(C2),mem(M2)), total(cpu(CT), mem(MT)), allocation(U1Tasks, U2Tasks)):-
	U1Cpu is CT div 2,
	U2Cpu is CT div 2,
	U1Mem is MT div 2,
	U2Mem is MT div 2,
	maximize_tasks(U1Cpu, U1Mem, C1, M1, U1Tasks),
	maximize_tasks(U2Cpu, U2Mem, C2, M2, U2Tasks),
	labeling([max(U1Tasks), max(U2Tasks)], [U1Tasks, U2Tasks]).

%% Adds contraints to get maximum number of tasks given the set of resources
%% maximize_tasks(+,+,+,+,-).
%% maximize_tasks(NumCpu, NumMem, CpuPerTask, MemPerTask, MaxNumTasks).
maximize_tasks(Cpu, Mem, CpuPerTask, MemPerTask, Tasks):-
	Tasks in 0..50,
	Tasks * CpuPerTask #=< Cpu,
	Tasks * MemPerTask #=< Mem.

%% Computes the DRF allocation for User1 and User2 given set of TotalResources.
%% drf(+,+,+,-).
%% drf(User1, User2, TotalResources, TaskAllocation).
drf(demand(cpu(C1),mem(M1)),demand(cpu(C2),mem(M2)), total(cpu(CT), mem(MT)), allocation(U1Tasks, U2Tasks)):-
	dominant_resource(demand(cpu(C1),mem(M1)), total(cpu(CT), mem(MT)), D1),
	dominant_resource(demand(cpu(C2),mem(M2)), total(cpu(CT), mem(MT)), D2),
	{
	 C1*U1Tasks + C2*U2Tasks =< CT,
	 M1*U1Tasks + M2*U2Tasks =< MT,
	 D1*U1Tasks =:= D2*U2Tasks
	},
	maximize(U1Tasks + U2Tasks).

%% Computes the dominant resource for a user given the total set of resources and the user demands.
%% dominant_resource(+,+,-).
%% dominant_resource(User, TotalResources, DominantResourceShare).
dominant_resource(demand(cpu(X1), mem(Y1)), total(cpu(X2), mem(Y2)), D):-
	CpuShare = X1 / X2,
	MemShare = Y1 / Y2,
	dominant(CpuShare, MemShare, D).

%% Check which one of two resource demands is dominant
%% dominant(+,+,-)
%% dominant(FractionOfR1Required, FractionOfR2Required, DominantResource)
dominant(X1, X2, X1):-
	X1 > X2.
dominant(X1, X2, X2):-
	X2 >= X1.
