Title: OOP Survival Guide - Java Edition (Extended)

Contents:
1. Introduction to OOP
2. Java I/O
3. OOP Concepts in Java
4. Design Patterns
5. Java Collections
6. Java Generics
7. Related Concepts and Tips

Chapter 1: Introduction to OOP

Object-oriented programming (OOP) is a programming paradigm that uses objects and their interactions to design and program applications. OOP focuses on crucial concepts such as objects, classes, inheritance, and polymorphism, that come together to enable modular code, better maintainability, and code reusability.

Chapter 2: Java I/O

Java I/O refers to the functionality provided by Java to read and write data.

1. File I/O: Use classes such as FileReader, FileWriter, BufferedReader, and BufferedWriter to read from and write to files.

```java
// Reading from a file
try (BufferedReader reader = new BufferedReader(new FileReader("input.txt"))) {
    String line;
    while ((line = reader.readLine()) != null) {
        System.out.println(line);
    }
} catch (IOException e) {
    e.printStackTrace();
}

// Writing to a file
try (BufferedWriter writer = new BufferedWriter(new FileWriter("output.txt"))) {
    writer.write("Hello, World!");
} catch (IOException e) {
    e.printStackTrace();
}
```

2. Byte Streams: Use InputStream and OutputStream classes to read and write data in the form of bytes.

3. Character Streams: Use Reader and Writer classes for character-based I/O.

Chapter 3: OOP Concepts in Java

1. Objects: Objects are instances of a class. They have attributes and behavior.

```java
public class Person {
    String name;
    int age;

    void introduce() {
        System.out.println("Hello, my name is " + name + " and I am " + age + " years old.");
    }
}

Person person = new Person();
person.name = "John";
person.age = 30;
person.introduce();
```

2. Classes: Blueprint for creating objects. Define the attributes and behavior.

3. Abstraction: Simplifying complex systems by hiding unnecessary details and showing essential features only.

4. Encapsulation: Binds together the attributes and methods that manipulate the attributes within a class and restricts access to them from outside the class.

```java
public class Person {
    private String name;
    private int age;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getAge() {
        return age;
    }

    public void setAge(int age) {
        this.age = age;
    }
}
```

5. Inheritance: Deriving a new class from an existing one, reusing its properties and methods.

```java
public class Employee extends Person {
    double salary;

    void displaySalary() {
        System.out.println("Salary: $" + salary);
    }
}
```

6. Polymorphism: One interface but multiple implementations. Useful for code reusability.

```java
public abstract class Animal {
    public abstract void makeSound();
}

public class Cat extends Animal {
    public void makeSound() {
        System.out.println("Meow!");
    }
}

public class Dog extends Animal {
    public void makeSound() {
        System.out.println("Woof!");
    }
}
```

Chapter 4: Design Patterns

Design patterns are reusable solutions to common programming problems that occur in software design. Some popular design patterns in Java include:

1. Singleton Pattern

```java
public class Singleton {
    private static Singleton instance;

    private Singleton() {}

    public static Singleton getInstance() {
        if (instance == null) {
            instance = new Singleton();
        }
        return instance;
    }
}
```

Familiarize yourself with the rest of the patterns and understand when to apply each one.

Chapter 5: Java Collections

1. Array vs List: Both are used to store elements, but arrays have a fixed size, while lists can grow or shrink. Arrays can store primitive data types, while lists can store only objects.

```java
int[] arr = new int[5];                            // Array
ArrayList<Integer> list = new ArrayList<Integer>(); // List
```

2. HashMap: A collection that stores key-value pairs, where each key is unique. It uses hashing to determine the index for a particular key-value pair.

```java
HashMap<String, Integer> map = new HashMap<>();
map.put("apple", 5);
map.put("banana", 10);
System.out.println(map.get("apple")); // Output: 5
```

3. HashSet: A collection of unique elements, stored in no particular order. Uses hashing to store and retrieve elements efficiently.

```java
HashSet<Integer> set = new HashSet<>();
set.add(5);
set.add(10);
set.add(5);
System.out.println(set); // Output: [5, 10]
```

4. TreeMap: A collection that stores key-value pairs in a sorted order. It's implemented using a Red-Black Tree, which ensures a balanced tree and, therefore, efficient operations.

```java
TreeMap<String, Integer> treeMap = new TreeMap<>();
treeMap.put("apple", 5);
treeMap.put("banana", 10);
System.out.println(treeMap); // Output: {apple=5, banana=10}
```

5. TreeSet: A collection of unique elements sorted in their natural order. It's implemented using a Red-Black Tree, similar to TreeMap.

```java
TreeSet<Integer> treeSet = new TreeSet<>();
treeSet.add(5);
treeSet.add(10);
treeSet.add(5);
System.out.println(treeSet); // Output: [5, 10]
```

Chapter 6: Java Generics

Generics allow you to create classes and methods that can operate on various types of objects while providing type safety. Generics tasks:

1. Create generic classes, interfaces, and methods.

```java
public class GenericBox<T> {
    private T item;

    public void setItem(T item) {
        this.item = item;
    }

    public T getItem() {
        return item;
    }
}

GenericBox<Integer> intBox = new GenericBox<>();
intBox.setItem(5);
Integer item = intBox.getItem();
```

2. Be familiar with type parameters (like E, T, K, and V).

3. Bounded types (type parameters with restrictions like "extends" or "super").

```java
public class GenericBox<T extends Number> { // T must be a subclass of Number
    //...
}
```

4. Understand type inference and type erasure.

Chapter 7: Related Concepts and Tips

1. Exception Handling: Understand the concepts of checked and unchecked exceptions and how to use try-catch blocks to handle exceptions properly.

2. Multithreading: Learn how to create and manage threads in Java and synchronize shared resources access.

3. Reflection: Understand how to use reflection to inspect and manipulate classes, fields, methods, and constructors at runtime.

4. Debugging: Learn how to use debugging tools in your IDE to find and fix issues in your code.

Keep practicing and attempting coding problems related to OOP, Java I/O, design patterns, and generics. Regularly review these fundamental concepts, and you'll be sure to ace your scholarship requirements. Good luck!

# SOLID Principles in Java

SOLID is a set of five design principles that help in writing clean, maintainable, and reusable code. These principles were introduced by Robert C. Martin and are widely used in Object-Oriented Programming (OOP). The SOLID acronym represents the following principles:

1. **S**ingle Responsibility Principle (SRP)
2. **O**pen/Closed Principle (OCP)
3. **L**iskov Substitution Principle (LSP)
4. **I**nterface Segregation Principle (ISP)
5. **D**ependency Inversion Principle (DIP)

## 1. Single Responsibility Principle (SRP)

This principle states that a class should have only one reason to change, meaning that it should have only one job. It increases the cohesion of the class and makes it easier to maintain.

### Example

Before applying SRP:

```java
public class Employee {
    private String name;
    private String email;

    // Employee properties and methods
    // ...

    public void saveEmployee() {
        // code to save employee in a database or file
    }
}
```

After applying SRP:

```java
public class Employee {
    private String name;
    private String email;

    // Employee properties and methods
    // ...
}

class EmployeeStorage {
    public void saveEmployee(Employee employee) {
        // code to save employee in database or file
    }
}
```

## 2. Open/Closed Principle (OCP)

According to this principle, software entities (classes, modules, functions, etc.) should be open for extension but closed for modification. In other words, the existing code should not be altered while adding new functionality to the application.

### Example

Before applying OCP:

```java
public class AreaCalculator {
    public double calculateArea(Object[] shapes) {
        double area = 0;
        for (Object shape : shapes) {
            if (shape instanceof Circle) {
                Circle circle = (Circle) shape;
                area += Math.PI * Math.pow(circle.radius, 2);
            } else if (shape instanceof Square) {
                Square square = (Square) shape;
                area += square.side * square.side;
            }
        }
        return area;
    }
}
```

After applying OCP:

```java
public interface Shape {
    double calculateArea();
}

public class Circle implements Shape {
    private double radius;

    // ...

    public double calculateArea() {
        return Math.PI * Math.pow(radius, 2);
    }
}

public class Square implements Shape {
    private double side;

    // ...

    public double calculateArea() {
        return side * side;
    }
}

public class AreaCalculator {
    public double calculateArea(Shape[] shapes) {
        double area = 0;
        for (Shape shape : shapes) {
            area += shape.calculateArea();
        }
        return area;
    }
}
```

## 3. Liskov Substitution Principle (LSP)

This principle states that objects of a derived class should be able to replace objects of the base class without affecting the correctness of the program.

### Example

Before applying LSP:

```java
public class Rectangle {
    protected int width;
    protected int height;

   // Constructors, Getters, and Setters...
}

public class Square extends Rectangle {
    public void setWidth(int width) {
        this.width = width;
        this.height = width;
    }

    public void setHeight(int height) {
        this.width = height;
        this.height = height;
    }
}
```

After applying LSP:

```java
public interface Shape {
    int calculateArea();
}

public class Rectangle implements Shape {
    private int width;
    private int height;

   // Constructors, Getters, and Setters...

    public int calculateArea() {
        return width * height;
    }
}

public class Square implements Shape {
    private int side;

   // Constructors, Getters, and Setters...

    public int calculateArea() {
        return side * side;
    }
}
```

## 4. Interface Segregation Principle (ISP)

This principle states that a class should not be forced to implement interfaces it does not use. Instead of defining large interfaces, it's better to split them into smaller and more specific ones.

### Example

Before applying ISP:

```java
public interface Printer {
    void print();
    void scan();
    void fax();
}

public class BasicPrinter implements Printer {
    public void print() {
        // Print functionality
    }

    public void scan() {
        // Not needed for a basic printer
        throw new UnsupportedOperationException("Basic printer does not support scanning");
    }

    public void fax() {
        // Not needed for a basic printer
        throw new UnsupportedOperationException("Basic printer does not support faxing");
    }
}
```

After applying ISP:

```java
public interface Printer {
    void print();
}

public interface Scanner {
    void scan();
}

public interface Fax {
    void fax();
}

public class BasicPrinter implements Printer {
    public void print() {
        // Print functionality
    }
}

public class AllInOnePrinter implements Printer, Scanner, Fax {
    public void print() {
        // Print functionality
    }

    public void scan() {
        // Scan functionality
    }

    public void fax() {
        // Fax functionality
    }
}
```

## 5. Dependency Inversion Principle (DIP)

This principle states that high-level modules should not depend on low-level modules; both should depend on abstractions. Also, abstractions should not depend on details; details should depend on abstractions.

### Example

Before applying DIP:

```java
public class TextNotifier {
    private EmailService emailService;

    public TextNotifier() {
        this.emailService = new EmailService();
    }

    public void notify(String message) {
        emailService.sendEmail(message);
    }
}

public class EmailService {
    public void sendEmail(String message) {
        // Code to send email
    }
}
```

After applying DIP:

```java
public interface Notifier {
    void notify(String message);
}

public class TextNotifier {
    private Notifier notifier;

    public TextNotifier(Notifier notifier) {
        this.notifier = notifier;
    }

    public void notify(String message) {
        notifier.notify(message);
    }
}

public class EmailService implements Notifier {
    public void notify(String message) {
        // Code to send email
    }
}

public class Main {
    public static void main(String[] args) {
        Notifier emailNotifier = new EmailService();
        TextNotifier textNotifier = new TextNotifier(emailNotifier);
        textNotifier.notify("Sample message");
    }
}
```

Following the SOLID principles can significantly improve the design, maintainability, and reusability of your code, making it more robust and flexible.
