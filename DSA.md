Sure, let's start with a brief survival guide for each data structure and algorithm, and then we'll delve into the implementations in Java.

### Data Structures and Algorithms Survival Guide:

#### Array:
- **Definition:** A collection of elements, each identified by an index or a key.
- **Use Cases:** Ideal for random access, simple storage, and retrieval of elements.
- **Common Operations:** Insertion, deletion, access, and search.

#### Stack:
- **Definition:** A Last-In-First-Out (LIFO) data structure.
- **Use Cases:** Function call management, expression evaluation, undo mechanisms.
- **Implementation:** Can be implemented using arrays or linked lists.

#### Queue:
- **Definition:** A First-In-First-Out (FIFO) data structure.
- **Use Cases:** Process scheduling, breadth-first search, print queue.
- **Implementation:** Can be implemented using arrays or linked lists.

#### Priority Queue:
- **Definition:** A data structure that stores elements based on their priority.
- **Use Cases:** Task scheduling, Huffman coding, Dijkstra's algorithm.
- **Implementations:** Can be implemented using arrays (brute force), linked lists (brute force), or arrays (heap).

#### Linked List:
- **Definition:** A linear collection of elements where each element points to the next one.
- **Use Cases:** Dynamic memory allocation, frequent insertions/deletions.
- **Common Operations:** Insertion, deletion, traversal.

Now, let's dive into the Java implementations:

### Stack Implementation:

#### Array-based Stack:
```java
public class ArrayStack {
    private int maxSize;
    private int top;
    private int[] stackArray;

    public ArrayStack(int size) {
        maxSize = size;
        stackArray = new int[maxSize];
        top = -1;
    }

    public void push(int value) {
        if (top < maxSize - 1) {
            stackArray[++top] = value;
        }
    }

    public int pop() {
        if (top >= 0) {
            return stackArray[top--];
        }
        return -1; // Stack underflow
    }

    public int peek() {
        if (top >= 0) {
            return stackArray[top];
        }
        return -1; // Stack is empty
    }

    public boolean isEmpty() {
        return top == -1;
    }
}
```

#### Linked List-based Stack:
```java
public class LinkedListStack {
    private Node top;

    private static class Node {
        int data;
        Node next;

        public Node(int data) {
            this.data = data;
            this.next = null;
        }
    }

    public void push(int value) {
        Node newNode = new Node(value);
        newNode.next = top;
        top = newNode;
    }

    public int pop() {
        if (top != null) {
            int value = top.data;
            top = top.next;
            return value;
        }
        return -1; // Stack underflow
    }

    public int peek() {
        if (top != null) {
            return top.data;
        }
        return -1; // Stack is empty
    }

    public boolean isEmpty() {
        return top == null;
    }
}
```

### Queue Implementation:

#### Array-based Queue:
```java
public class ArrayQueue {
    private int maxSize;
    private int front;
    private int rear;
    private int[] queueArray;

    public ArrayQueue(int size) {
        maxSize = size;
        queueArray = new int[maxSize];
        front = 0;
        rear = -1;
    }

    public void enqueue(int value) {
        if (rear < maxSize - 1) {
            queueArray[++rear] = value;
        }
    }

    public int dequeue() {
        if (!isEmpty()) {
            int value = queueArray[front++];
            return value;
        }
        return -1; // Queue is empty
    }

    public int peek() {
        if (!isEmpty()) {
            return queueArray[front];
        }
        return -1; // Queue is empty
    }

    public boolean isEmpty() {
        return front > rear;
    }
}
```

#### Linked List-based Queue:
```java
public class LinkedListQueue {
    private Node front;
    private Node rear;

    private static class Node {
        int data;
        Node next;

        public Node(int data) {
            this.data = data;
            this.next = null;
        }
    }

    public void enqueue(int value) {
        Node newNode = new Node(value);
        if (rear == null) {
            front = rear = newNode;
        } else {
            rear.next = newNode;
            rear = newNode;
        }
    }

    public int dequeue() {
        if (!isEmpty()) {
            int value = front.data;
            front = front.next;
            if (front == null) {
                rear = null; // If the last element is dequeued
            }
            return value;
        }
        return -1; // Queue is empty
    }

    public int peek() {
        if (!isEmpty()) {
            return front.data;
        }
        return -1; // Queue is empty
    }

    public boolean isEmpty() {
        return front == null;
    }
}
```

### Priority Queue Implementation:

#### Linked List-based Priority Queue (Brute Force):
```java
public class LinkedListPriorityQueue {
    private Node head;

    private static class Node {
        int data;
        int priority;
        Node next;

        public Node(int data, int priority) {
            this.data = data;
            this.priority = priority;
            this.next = null;
        }
    }

    public void enqueue(int value, int priority) {
        Node newNode = new Node(value, priority);
        if (head == null || priority < head.priority) {
            newNode.next = head;
            head = newNode;
        } else {
            Node temp = head;
            while (temp.next != null && temp.next.priority <= priority) {
                temp = temp.next;
            }
            newNode.next = temp.next;
            temp.next = newNode;
        }
    }

    public int dequeue() {
        if (!isEmpty()) {
            int value = head.data;
            head = head.next;
            return value;
        }
        return -1; // Priority Queue is empty
    }

    public int peek() {
        if (!isEmpty()) {
            return head.data;
        }
        return -1; // Priority Queue is empty
    }

    public boolean isEmpty() {
        return head == null;
    }
}
```

#### Array-based Priority Queue (Brute Force):
```java
public class ArrayPriorityQueue {
    private int maxSize;
    private int size;
    private int[] values;
    private int[] priorities;

    public ArrayPriorityQueue(int size) {
        maxSize = size;
        values = new int[maxSize];
        priorities = new int[maxSize];
        size = 0;
    }

    public void enqueue(int value, int priority) {
        if (size < maxSize) {
            int i;
            for (i = size - 1; i >= 0; i--) {
                if (priority > priorities[i]) {
                    values[i + 1] = values[i];
                    priorities[i + 1] = priorities[i];
                } else {
                    break;
                }
            }
            values[i + 1] = value;
           

 priorities[i + 1] = priority;
            size++;
        }
    }

    public int dequeue() {
        if (!isEmpty()) {
            int value = values[--size];
            return value;
        }
        return -1; // Priority Queue is empty
    }

    public int peek() {
        if (!isEmpty()) {
            return values[size - 1];
        }
        return -1; // Priority Queue is empty
    }

    public boolean isEmpty() {
        return size == 0;
    }
}
```

#### Array-based Priority Queue (Heap):
```java
public class HeapPriorityQueue {
    private int maxSize;
    private int size;
    private int[] heap;

    public HeapPriorityQueue(int size) {
        maxSize = size;
        heap = new int[maxSize];
        size = 0;
    }

    public void enqueue(int value) {
        if (size < maxSize) {
            int i = size++;
            while (i > 0) {
                int parent = (i - 1) / 2;
                if (value <= heap[parent]) {
                    break;
                }
                heap[i] = heap[parent];
                i = parent;
            }
            heap[i] = value;
        }
    }

    public int dequeue() {
        if (!isEmpty()) {
            int root = heap[0];
            heap[0] = heap[--size];
            heapify(0);
            return root;
        }
        return -1; // Priority Queue is empty
    }

    public int peek() {
        if (!isEmpty()) {
            return heap[0];
        }
        return -1; // Priority Queue is empty
    }

    public boolean isEmpty() {
        return size == 0;
    }

    private void heapify(int i) {
        int largest = i;
        int leftChild = 2 * i + 1;
        int rightChild = 2 * i + 2;

        if (leftChild < size && heap[leftChild] > heap[largest]) {
            largest = leftChild;
        }

        if (rightChild < size && heap[rightChild] > heap[largest]) {
            largest = rightChild;
        }

        if (largest != i) {
            int temp = heap[i];
            heap[i] = heap[largest];
            heap[largest] = temp;

            heapify(largest);
        }
    }
}
```

These implementations cover array-based and linked list-based structures for stacks, queues, and priority queues, including both brute-force and heap-based approaches for priority queues.
