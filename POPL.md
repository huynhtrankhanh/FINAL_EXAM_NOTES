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

# Scheme Survival Guide

## Introduction to Scheme

### 1. **Overview**
   Scheme is a minimalist, functional programming language known for its simple syntax and powerful features. It follows the Lisp family of languages and emphasizes the principles of functional programming and lexical scoping.

### 2. **Basics**
   - **Data Types:** Scheme has a few basic data types, including numbers, symbols, strings, booleans, and lists.
   - **Functions:** Functions are first-class citizens, allowing them to be passed as arguments and returned as values.

### 3. **Syntax**
   - **Prefix Notation:** Scheme uses prefix notation for expressions. For example, `(add 2 3)` instead of `2 + 3`.
   - **Parentheses:** Parentheses are crucial for defining expressions and creating lists.

## Functional Programming in Scheme

### 1. **Pure Functions**
   Scheme encourages the use of pure functions that produce the same output for the same input, avoiding side effects.

### 2. **Higher-Order Functions**
   Functions can take functions as arguments or return them. This enables powerful functional programming constructs.

### 3. **Recursion**
   Scheme relies heavily on recursion for looping and iteration. Tail call optimization is often employed for efficient recursion.

## Key Concepts

### 1. **Lexical Scoping**
   Scheme uses lexical scoping, meaning variables are resolved based on their lexical context or surrounding code.

### 2. **Closures**
   Closures allow functions to capture and remember the lexical scope in which they were created, leading to powerful and flexible programming constructs.

### 3. **Dynamic Typing**
   Scheme is dynamically typed, meaning variable types are determined at runtime.

## Common Operations and Examples

### 1. **List Manipulation**
   - **Creating Lists:** `(define my-list '(1 2 3))`
   - **Accessing Elements:** `(car my-list)` returns the first element.

### 2. **Function Definition**
   - **Defining Functions:** `(define (add x y) (+ x y))`

### 3. **Conditionals**
   - **If Statement:** `(if (< x 0) "Negative" "Non-negative")`

### 4. **Recursion Example**
   ```scheme
   (define (factorial n)
     (if (= n 0)
         1
         (* n (factorial (- n 1)))))
   ```

## Advanced Topics

### 1. **Macros**
   Scheme macros allow for code transformations, enabling the creation of domain-specific languages within Scheme itself.

### 2. **Continuations**
   Continuations provide a way to capture and control the flow of a program, offering advanced control structures.

## Resources

- **Books:** "Structure and Interpretation of Computer Programs" by Harold Abelson and Gerald Jay Sussman.
- **Online Documentation:** [Racket Documentation](https://docs.racket-lang.org/)

## Conclusion

Scheme's simplicity, functional paradigm, and expressive power make it a valuable language for both beginners and seasoned programmers. Embrace its functional principles and explore the rich ecosystem of libraries and tools available for Scheme development.

# Advanced Scheme Programming

## Prefix Sum

### **Implementation:**
```scheme
(define (prefix-sum lst)
  (let loop ((acc 0) (result '()) (lst lst))
    (if (null? lst)
        (reverse result)
        (loop (+ acc (car lst)) (cons acc result) (cdr lst)))))
```

### **Explanation:**
The `prefix-sum` function calculates the prefix sum of a list. It uses tail recursion to accumulate the sum while building a result list. The result is reversed to maintain the original order.

## Binary Search

### **Implementation:**
```scheme
(define (binary-search target lst)
  (let loop ((low 0) (high (- (length lst) 1)))
    (if (> low high)
        #f
        (let* ((mid (quotient (+ low high) 2))
               (mid-val (list-ref lst mid)))
          (cond ((= mid-val target) mid)
                ((< mid-val target) (loop (+ mid 1) high))
                (else (loop low (- mid 1))))))))
```

### **Explanation:**
The `binary-search` function performs a binary search on a sorted list. It uses recursion to narrow down the search range until the target is found or the search range becomes empty.

## Merge Sort

### **Implementation:**
```scheme
(define (merge-sort lst)
  (if (<= (length lst) 1)
      lst
      (let* ((mid (quotient (length lst) 2))
             (left (take lst mid))
             (right (drop lst mid)))
        (merge (merge-sort left) (merge-sort right)))))
```

### **Explanation:**
The `merge-sort` function sorts a list using the merge sort algorithm. It recursively divides the list into halves, sorts each half, and then merges them back together.

## Subarray Max Sum

### **Implementation:**
```scheme
(define (subarray-max-sum lst)
  (let loop ((current-sum 0) (max-sum 0) (lst lst))
    (if (null? lst)
        max-sum
        (let ((next-val (car lst)))
          (loop (max 0 (+ current-sum next-val))
                (max max-sum (+ current-sum next-val))
                (cdr lst))))))
```

### **Explanation:**
The `subarray-max-sum` function finds the maximum sum of a contiguous subarray using Kadane's algorithm. It iterates through the list, maintaining the current sum and updating the maximum sum as it goes.

## Depth-First Search (DFS)

### **Implementation:**
```scheme
(define (dfs graph node visited)
  (unless (memv node visited)
    (display node)
    (for-each (lambda (neighbor)
                (dfs graph neighbor (cons node visited)))
              (cdr (assoc node graph)))))
```

### **Explanation:**
The `dfs` function performs a Depth-First Search on a graph represented as an adjacency list. It recursively explores each vertex's neighbors, marking visited nodes to avoid cycles.

## Breadth-First Search (BFS)

### **Implementation:**
```scheme
(define (bfs graph start-node)
  (define (bfs-helper queue visited)
    (cond ((null? queue) '())
          (else
           (let ((current-node (car queue))
                 (rest (cdr queue)))
             (unless (memv current-node visited)
               (display current-node)
               (let ((neighbors (cdr (assoc current-node graph))))
                 (let ((new-nodes (filter (lambda (neighbor)
                                            (not (memv neighbor visited)))
                                          neighbors)))
                   (bfs-helper (append rest new-nodes) (cons current-node visited))))))))
  (bfs-helper (list start-node) '()))
```

### **Explanation:**
The `bfs` function performs a Breadth-First Search on a graph represented as an adjacency list. It uses a queue to explore neighbors level by level, ensuring all vertices at the current level are visited before moving to the next level.

## Dijkstra's Algorithm

### **Implementation:**
```scheme
(define (dijkstra graph start-node)
  (define (find-min-distance unvisited distances)
    (let ((min-pair (apply min distances)))
      (list (car min-pair) (cdr min-pair))))
  
  (let loop ((current-node start-node)
             (unvisited (map car graph))
             (distances (if (= current-node start-node)
                            (map (lambda (node) (cons node 0)) (map car graph))
                            (map (lambda (node) (cons node +inf.0)) (map car graph)))))
    (if (null? unvisited)
        distances
        (let* ((neighbors (cdr (assoc current-node graph)))
               (current-distance (cdr (assoc current-node distances)))
               (unvisited-neighbors (filter (lambda (neighbor)
                                              (memv neighbor unvisited))
                                            neighbors)))
          (for-each (lambda (neighbor)
                      (let* ((edge-distance (cdr (assoc (car neighbor) neighbors)))
                             (new-distance (+ current-distance edge-distance)))
                        (when (< new-distance (cdr (assoc (car neighbor) distances)))
                          (set! distances (assoc-set (car neighbor) distances new-distance))))))
                    unvisited-neighbors)
          (let* ((next-node-pair (find-min-distance unvisited distances))
                 (next-node (car next-node-pair)))
            (loop next-node (remove next-node unvisited) distances))))))

```

### **Explanation:**
The `dijkstra` function implements Dijkstra's algorithm to find the shortest path from a start node to all other nodes in a weighted graph represented as an adjacency list. It iteratively selects the node with the minimum distance and updates the distances to its neighbors.

These Scheme implementations cover a range of algorithms and techniques, providing a foundation for solving diverse computational problems. Understanding these examples will enhance your ability to express complex ideas and algorithms using Scheme.

### Concurrency Primitive Survival Guide in C++

Concurrency is crucial for efficient and responsive software. Here's a survival guide featuring classic concurrency algorithms in C++:

#### 1. **Mutexes:**
   - Use `std::mutex` to protect critical sections.
   - Always employ RAII with `std::lock_guard` to ensure automatic unlocking.

```cpp
#include <mutex>

std::mutex myMutex;

void criticalSection() {
    std::lock_guard<std::mutex> lock(myMutex);
    // Critical section code
}
```

#### 2. **Atomic Operations:**
   - Leverage `std::atomic` for lock-free operations.
   - It's essential for shared data in multithreaded environments.

```cpp
#include <atomic>

std::atomic<int> sharedValue;

void atomicOperation() {
    sharedValue.fetch_add(1, std::memory_order_relaxed);
}
```

#### 3. **Condition Variables:**
   - Use `std::condition_variable` for synchronization between threads.
   - Avoid busy-waiting; instead, wait on a condition.

```cpp
#include <condition_variable>

std::mutex myMutex;
std::condition_variable myCondition;

void waitForSignal() {
    std::unique_lock<std::mutex> lock(myMutex);
    myCondition.wait(lock);
    // Continue after receiving signal
}

void sendSignal() {
    myCondition.notify_one();
}
```

#### 4. **Read-Write Locks:**
   - Implement a read-write lock for scenarios with frequent reads.
   - Balances between read and write access efficiently.

```cpp
#include <shared_mutex>

std::shared_mutex rwLock;

void readOperation() {
    std::shared_lock<std::shared_mutex> lock(rwLock);
    // Read operation
}

void writeOperation() {
    std::unique_lock<std::shared_mutex> lock(rwLock);
    // Write operation
}
```

#### 5. **Semaphore:**
   - Create a semaphore using `std::counting_semaphore`.
   - Useful for managing a limited resource pool.

```cpp
#include <semaphore>

std::counting_semaphore<int> mySemaphore(5); // Initialize with maximum resource count

void acquireResource() {
    mySemaphore.acquire();
    // Critical section with resource
}

void releaseResource() {
    mySemaphore.release();
}
```

#### 6. **Barrier:**
   - Implement a barrier to synchronize a group of threads.
   - Threads wait at the barrier until all have arrived.

```cpp
#include <barrier>

std::barrier myBarrier(3); // Initialize with the number of participating threads

void threadFunction() {
    // Code before synchronization point
    myBarrier.arrive_and_wait(); // Synchronization point
    // Code after synchronization point
}
```

Mastering these concurrency primitives will empower you to build robust and efficient multithreaded applications in C++.

Certainly! Let's create a simple proof-of-work algorithm in C++ using the mentioned concurrency primitives and incorporate user-triggered cancellation:

```cpp
#include <iostream>
#include <thread>
#include <mutex>
#include <atomic>
#include <condition_variable>

std::mutex cancelMutex;
std::condition_variable cancelCondition;
std::atomic<bool> cancelFlag(false);

std::atomic<bool> solutionFound(false);
std::string solution;

void workerThread(int threadId) {
    std::string partialSolution;
    std::hash<std::string> hashFunction;

    while (!solutionFound) {
        // Generate a random candidate solution
        partialSolution = "Candidate" + std::to_string(threadId);

        // Check if the candidate solution meets the proof-of-work criteria (6 leading zero bits)
        if (hashFunction(partialSolution) % 64 < 6) {
            std::unique_lock<std::mutex> cancelLock(cancelMutex);
            if (cancelFlag) {
                // User-triggered cancellation
                cancelCondition.notify_all();
                return;
            }

            // Update the solution if it meets the criteria
            solution = partialSolution;
            solutionFound = true;
            cancelCondition.notify_all();
            std::cout << "Solution found by thread " << threadId << ": " << solution << std::endl;
        }
    }
}

void cancelAlgorithm() {
    std::unique_lock<std::mutex> cancelLock(cancelMutex);
    cancelFlag = true;
    cancelCondition.notify_all();
}

int main() {
    const int numThreads = 4;
    std::thread threads[numThreads];

    // Start worker threads
    for (int i = 0; i < numThreads; ++i) {
        threads[i] = std::thread(workerThread, i);
    }

    // Wait for user input to cancel the algorithm
    std::cout << "Press enter to cancel the proof-of-work algorithm." << std::endl;
    std::cin.ignore();  // Wait for user input
    cancelAlgorithm();

    // Wait for worker threads to finish
    for (int i = 0; i < numThreads; ++i) {
        threads[i].join();
    }

    return 0;
}
```

This algorithm uses a basic proof-of-work approach where threads generate candidate solutions until one is found with 6 leading zero bits when hashed. The user can trigger cancellation by pressing enter, and the threads will gracefully terminate, providing the current solution if found.
