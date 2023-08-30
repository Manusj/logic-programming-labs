:- use_module(library(clpfd)).

% definition of facts of problem

container(aa, 2, 2). container(ab, 3, 2).
container(ac, 5, 5). container(ba, 6, 2).
container(bb, 1, 2). container(bc, 3, 3).
container(ca, 3, 2). container(cb, 5, 2).
container(cc, 2, 3).
on(aa, ba). on(ab, ba). on(ab, bb). on(ac, ba).
on(ac, bb). on(ac, bc). on(ba, ca). on(ba, cb).
on(bb, cb). on(bc, cb). on(bc, cc).
% createTasks(TaskList, StartTime, TaskDuration, TaskEndTime, TaskResourceLimit, Tasklabel):
% is a rule used to create a list of Task called TaskList from the rest of the data given

% base case createTasks - stop recursion when the lists are empty
createTask([],[],[],[],[],[]).

% recursive case createTasks/6 - recursively create new tasks based on the list of data
% [Task|Rest] - is the list of task
% [ST|STx] - list of task start times
% [TD|TDx] - list of task durations
% [TE|TEx] - list of task end time
% [TLim|TLimx] - list of resources (workers) required for the tasks
% [TLab|Tbalx] - list of task labels
createTask([Task|Rest],[ST|STx], [TD|TDx], [TE|TEx], [TLim|TLimx],[TLab|Tbalx]):-
	Task = task(ST,TD,TE,TLim,TLab),
	createTask(Rest,STx,TDx,TEx,TLimx,Tbalx).

% base case createTask/4 - rule used to create the lists for task labels, limits and durations and then call createTask/6 for creating tasks
% Tasks - Lists of tasks created
% StartTimes - start times of tasks created as variables with domain 0 - 10
% TaskEndTimes - end times of tasks created as variables with domain 0 - 10
createTask(Tasks,StartTimes,TaskEndTimes,Tasklabels):-
	findall(B,container(B,_,_), Tasklabels),
	findall(M,container(_,M,_), TaskLimits),
	findall(D,container(_,_,D), TaskDuration),
	createTask(Tasks,StartTimes,TaskDuration,TaskEndTimes,TaskLimits,Tasklabels),
	domain(StartTimes,0,100),
	domain(TaskEndTimes,0,100).

% checkandConstrain(TL,[L|TLx],ST,[S|STx],TE,[T|TEx]) - rules used to define constrains on task start time and task end time based on the facts on
% if on(A,B) the start time of B >= End time of A
% TL - is the current task label which is to be constrained
% [L|TLx] - is the list of task labels.
% ST - is the start time of the current task to be constrained
% [S|STx] - is the list of start times of tasks
% TE - is the end time of current task to be constrained
% [T|TEx] - is the list of end times of task.

% base case checkandConstrain(_,[],_,[],_,[]) - if lists empty then stop recursion
checkandConstrain(_,[],_,[],_,[]).

% case checkandConstrain - if current label to be constrained and the current first element of list has no on relations then add no constrains.
checkandConstrain(TL,[L|TLx],ST,[_|STx],TE,[_|TEx]):-
	\+on(TL,L),
	\+on(L,TL).
% case checkandConstrain - if current label to be constrained and the current first element of list has "on" relations then add no constrains.
checkandConstrain(TL,[L|TLx],ST,[S|STx],TE,[_|TEx]):-
	on(TL,L),
	S#>TE.
checkandConstrain(TL,[L|TLx],ST,[_|STx],TE,[T|TEx]):-
	on(L,TL),
	ST#>T.

% taskStartTimeConstrain[TaskList, TaskStartTimeList, TaskEndTimeList]
% rule used to recursivley iterate throught the Task list and check each task and add constrains using checkandConstrain
taskStartTimeConstrain([],[],[]).
taskStartTimeConstrain([TL|TLx],[ST|STx],[TE|TEx]):-
	checkandConstrain(TL,TLx,ST,STx,TE,TEx),
	taskStartTimeConstrain(TLx,STx,TEx).

% endTimeConstrain(EndTime, TaskEndTimesList) - add constrain on the variable endTime reucrsivly with each TaskEndTime.
% EndTime should always be larger than or equal to the the largest end time in tasks.
% To do this we can add contrain for the variable EndTime with the taskEndTime of each Task.
% EndTime - variable than denotes the Time of completion of all tasks.
% TaskEndTimesList - list of variables of end time of each task.

% base case endTimeConstrain(_,[]) - if the list of Task End Times is empty then stop the recursion
endTimeConstrain(_,[]).
% recursive case endTimeConstrain(EndTime,[TE|Tex]) - add constriains for EndTime with TE(Task End time of some task in list) and recusively goto next list element
endTimeConstrain(EndTime,[TE|Tex]):-
	EndTime#>TE,
	endTimeConstrain(EndTime,Tex).

% addConstrains(EndTime,StartTimes,TaskEndTimes,Tasklabels) - rule to add constrains using the rules taskStartTimeConstrain & endTimeConstrain
addConstrains(EndTime,StartTimes,TaskEndTimes,Tasklabels):-
	taskStartTimeConstrain(Tasklabels,StartTimes,TaskEndTimes),
	endTimeConstrain(EndTime,TaskEndTimes).

% schelue(Tasks, Workers, EndTime, Cost) - rule that schedule tasks so as to minimize the cost
% Task - List of tasks, Workers - number of workers, EndTime - time when all tasks end, Cost - cost of doing task
% scheduling for tasks is found using the predicates cumulative and labeling of clpfd for this
% initially a list of tasks created, the domain added to unknown variables, constrains added on requruied variable and then 
% labeling used to give value to required variables.
schedule(Tasks, Workers, EndTime, Cost) :-
	createTask(Tasks,StartTimes,TaskEndTimes,Tasklabels),
	EndTime in 0..100,
	Workers in 0..7,
	addConstrains(EndTime,StartTimes,TaskEndTimes,Tasklabels),
	cumulative(Tasks, [limit(Workers)]),
	Cost #= Workers*EndTime,
	labeling([minimize(Cost)], [Cost|StartTimes]).
