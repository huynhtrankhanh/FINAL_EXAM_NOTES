# Prolog Survival Guide

## Primer: Essential Concepts

### 1. **Introduction to Prolog**
   Prolog is a declarative programming language designed for symbolic reasoning and manipulation. It excels in rule-based systems, artificial intelligence, and knowledge representation.

### 2. **Logic Programming Basics**
   Prolog operates on the principles of formal logic, specifically first-order logic. Programs in Prolog consist of facts and rules that define relationships and constraints.

### 3. **Terms and Clauses**
   - **Terms:** Basic elements like atoms, numbers, and variables.
   - **Clauses:** Fundamental building blocks, including facts and rules.

### 4. **Predicates and Goals**
   - **Predicate:** A logical relationship or property.
   - **Goal:** A query or objective to be satisfied by the program.

### 5. **Unification**
   Unification is the process of making different terms identical, aligning variables in a way that satisfies logical constraints.

### 6. **Backtracking**
   Prolog uses backtracking to explore alternative solutions. If a certain path fails, it goes back to the last choice point and explores other possibilities.

## Logical Inference

### 1. **Horn Clauses**
   - **Facts:** Statements considered true.
   - **Rules:** Conditions that imply certain conclusions.

### 2. **Resolution**
   The process of combining clauses to derive new information, central to logical inference in Prolog.

### 3. **Cut Operator**
   The `!` (cut) operator is used to commit to a choice, preventing backtracking to the last choice point.

## Common Algorithms

### 1. **Sum of a List**
   ```prolog
   sum([], 0).
   sum([X|Xs], Result) :- sum(Xs, Subtotal), Result is X + Subtotal.
   ```

### 2. **Merge Sort**
   ```prolog
   merge_sort([], []).
   merge_sort([X], [X]).
   merge_sort(List, Sorted) :- 
       split(List, Left, Right),
       merge_sort(Left, SortedLeft),
       merge_sort(Right, SortedRight),
       merge(SortedLeft, SortedRight, Sorted).
   ```

### 3. **Prefix Sum**
   ```prolog
   prefix_sum([], []).
   prefix_sum([X|Xs], [S|Sums]) :-
       prefix_sum(Xs, Sums),
       S is X + Sums.
   ```

### 4. **Binary Tree**
   ```prolog
   % Define a binary tree
   tree(nil).
   tree(node(Value, Left, Right)) :- tree(Left), tree(Right).
   ```

### 5. **Querying Dataset**
   - **Example Dataset:**
     ```prolog
     student(john, math).
     student(jane, history).
     ```

   - **Querying:**
     ```prolog
     ?- student(john, Subject).
     ```

     This query retrieves the subject John is studying.

## Conclusion
This Prolog survival guide introduces fundamental concepts, logical inference, and common algorithms. Explore the power of Prolog through its declarative nature and leverage its capabilities in various domains.

# Extended Prolog Survival Guide

## Building an ECHO Application

### **ECHO Program Implementation:**
```prolog
echo :-
    write('Type something: '),
    read(Input),
    writeln('You said:'),
    writeln(Input),
    echo.  % Recursive call for continuous interaction
```

This simple Prolog program prompts the user to input something and then echoes the input. The `echo` predicate calls itself recursively, allowing continuous interaction.

## Ancestral Tree and Marriage Eligibility

### **Ancestral Tree Representation:**
```prolog
% Defining ancestral relationships
parent(john, mary).
parent(john, bob).
parent(mary, alice).
% ... more relationships ...

% Ancestor relationship
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

### **Binary Tag and Maturity:**
```prolog
% Binary tag representation
binaryTag(john, 0).
binaryTag(mary, 1).
% ... more binary tags ...

% Maturity level
maturity(john, 25).
maturity(mary, 27).
% ... more maturity levels ...
```

### **Marriage Eligibility Rule:**
```prolog
eligible_for_marriage(Partner1, Partner2) :-
    ancestor(LCA, Partner1),
    ancestor(LCA, Partner2),
    dist_to_lca(Partner1, LCA, Dist1),
    dist_to_lca(Partner2, LCA, Dist2),
    binaryTag(Partner1, Tag1),
    binaryTag(Partner2, Tag2),
    maturity(Partner1, Maturity1),
    maturity(Partner2, Maturity2),
    Dist1 =< 3,
    Dist2 =< 3,
    Tag1 \= Tag2,
    abs(Maturity1 - Maturity2) =< 5.

% Distance to Lowest Common Ancestor (LCA)
dist_to_lca(Node, Node, 0).
dist_to_lca(Node, LCA, Dist) :- parent(Node, Parent), dist_to_lca(Parent, LCA, DistParent), Dist is DistParent + 1.
```

This rule `eligible_for_marriage/2` checks if two individuals are eligible for marriage based on the specified conditions. It calculates the distance to the Lowest Common Ancestor (LCA) and compares binary tags and maturity levels.

## Conclusion
Extend your Prolog skills by implementing practical applications like an ECHO program and incorporating complex rules for real-world scenarios, such as assessing marriage eligibility based on ancestral trees, binary tags, and maturity levels. Prolog's logical inference and rule-based nature make it a powerful tool for such applications.
