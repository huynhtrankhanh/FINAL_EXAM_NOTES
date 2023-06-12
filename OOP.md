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

# Design Patterns in Java

Design patterns represent reusable solutions to common software design challenges. They do not pertain to specific algorithms or programming languages but rather to the design and organization of classes and interfaces. As a result, these patterns can be used in any object-oriented programming language, including Java.

This document introduces some of the most commonly used design patterns in Java, along with their respective code snippets.

## Table of Contents

1. [Singleton Pattern](#singleton-pattern)
2. [Factory Pattern](#factory-pattern)
3. [Abstract Factory Pattern](#abstract-factory-pattern)
4. [Builder Pattern](#builder-pattern)
5. [Prototype Pattern](#prototype-pattern)
6. [Adapter Pattern](#adapter-pattern)
7. [Bridge Pattern](#bridge-pattern)
8. [Composite Pattern](#composite-pattern)
9. [Decorator Pattern](#decorator-pattern)
10. [Facade Pattern](#facade-pattern)
11. [Flyweight Pattern](#flyweight-pattern)
12. [Proxy Pattern](#proxy-pattern)
13. [Chain of Responsibility Pattern](#chain-of-responsibility-pattern)
14. [Command Pattern](#command-pattern)
15. [Iterator Pattern](#iterator-pattern)
16. [Mediator Pattern](#mediator-pattern)
17. [Memento Pattern](#memento-pattern)
18. [Observer Pattern](#observer-pattern)
19. [State Pattern](#state-pattern)
20. [Strategy Pattern](#strategy-pattern)
21. [Template Method Pattern](#template-method-pattern)
22. [Visitor Pattern](#visitor-pattern)

## Singleton Pattern

The Singleton pattern ensures that a class has only one instance and provides a global point of access to that instance. It is useful when you want a single object to coordinate actions across the system.

Here's a basic implementation of the Singleton pattern in Java:

```java
public class Singleton {

    private static Singleton instance;

    // Prevent direct instantiation
    private Singleton() {}

    public static Singleton getInstance() {
        if (instance == null) {
            instance = new Singleton();
        }
        return instance;
    }
}
```

## Factory Pattern

The Factory pattern defines an interface for creating objects in a superclass, but allows subclasses to alter the type of objects that will be created. It is useful when a class cannot anticipate the class of objects it must create or when a class wants its subclasses to specify the objects it creates.

```java
abstract class Car {
    // Common properties and methods for all cars
}

class Ferrari extends Car {
    // Ferrari-specific properties
}

class Lamborghini extends Car {
    // Lamborghini-specific properties
}

class CarFactory {
    public static Car createCar(String carType) {
        if (carType.equalsIgnoreCase("Ferrari")) {
            return new Ferrari();
        } else if (carType.equalsIgnoreCase("Lamborghini")) {
            return new Lamborghini();
        }
        return null;
    }
}

// Usage:
Car car = CarFactory.createCar("Ferrari");
```

## Abstract Factory Pattern

The Abstract Factory pattern provides an interface for creating families of related or dependent objects without specifying their concrete classes.

```java
interface AbstractCarFactory {
    Car createCar();
}

class FerrariFactory implements AbstractCarFactory {
    @Override
    public Car createCar() {
        return new Ferrari();
    }
}

class LamborghiniFactory implements AbstractCarFactory {
    @Override
    public Car createCar() {
        return new Lamborghini();
    }
}

// Usage:
AbstractCarFactory factory = new FerrariFactory();
Car car = factory.createCar();
```

## Builder Pattern

The Builder pattern separates the construction of a complex object from its representation so that the same construction process can create different representations. It is useful when you need to assemble a complex object with various parts.

```java
class Pizza {
    private String dough;
    private String sauce;
    private String topping;

    public void setDough(String dough) {
        this.dough = dough;
    }

    public void setSauce(String sauce) {
        this.sauce = sauce;
    }

    public void setTopping(String topping) {
        this.topping = topping;
    }
}

abstract class PizzaBuilder {
    protected Pizza pizza;

    public void createNewPizzaProduct() {
        pizza = new Pizza();
    }

    public Pizza getPizza() {
        return pizza;
    }

    public abstract void buildDough();

    public abstract void buildSauce();

    public abstract void buildTopping();
}

class HawaiianPizzaBuilder extends PizzaBuilder {
    @Override
    public void buildDough() {
        pizza.setDough("cross");
    }

    @Override
    public void buildSauce() {
        pizza.setSauce("mild");
    }

    @Override
    public void buildTopping() {
        pizza.setTopping("ham and pineapple");
    }
}

class Waiter {
    private PizzaBuilder pizzaBuilder;

    public void setPizzaBuilder(PizzaBuilder pizzaBuilder) {
        this.pizzaBuilder = pizzaBuilder;
    }

    public Pizza getPizza() {
        return pizzaBuilder.getPizza();
    }

    public void constructPizza() {
        pizzaBuilder.createNewPizzaProduct();
        pizzaBuilder.buildDough();
        pizzaBuilder.buildSauce();
        pizzaBuilder.buildTopping();
    }
}

// Usage:
Waiter waiter = new Waiter();
PizzaBuilder hawaiianPizzaBuilder = new HawaiianPizzaBuilder();

waiter.setPizzaBuilder(hawaiianPizzaBuilder);
waiter.constructPizza();

Pizza pizza = waiter.getPizza();
```

## Prototype Pattern

The Prototype pattern allows you to create a new instance of a class by copying the state of an existing instance. This is useful when object construction costs are high or when creating an object requires a complex process.

```java
abstract class Prototype implements Cloneable {
    public Prototype clone() {
        try {
            return (Prototype) super.clone();
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
        }
        return null;
    }
}

class ConcretePrototype extends Prototype {
    // ConcretePrototype-specific properties
}

// Usage:
ConcretePrototype original = new ConcretePrototype();
ConcretePrototype clone = original.clone();
```

## Adapter Pattern

The Adapter pattern allows classes with incompatible interfaces to work together by wrapping the existing class with a new interface. It is useful when you want to reuse existing classes without modifying them.

```java
// Existing interface
interface OldInterface {
    void oldMethod();
}

// New interface
interface NewInterface {
    void newMethod();
}

class OldClass implements OldInterface {
    @Override
    public void oldMethod() {
        // Implementation
    }
}

class Adapter implements NewInterface {
    private OldInterface oldObject;

    public Adapter(OldInterface oldObject) {
        this.oldObject = oldObject;
    }

    @Override
    public void newMethod() {
        oldObject.oldMethod();
    }
}

// Usage:
OldInterface oldObject = new OldClass();
NewInterface newObject = new Adapter(oldObject);
newObject.newMethod();
```

## Bridge Pattern

The Bridge pattern decouples an abstraction from its implementation, allowing them to vary independently. This is useful when you want to share an implementation among multiple objects while keeping the implementation hidden.

```java
interface Renderer {
    void render();
}

class OpenGLRenderer implements Renderer {
    @Override
public void render() {
        // OpenGL-specific rendering implementation
    }
}

class DirectXRenderer implements Renderer {
    @Override
    public void render() {
        // DirectX-specific rendering implementation
    }
}

abstract class Shape {
    protected Renderer renderer;

    public Shape(Renderer renderer) {
        this.renderer = renderer;
    }

    public abstract void draw();
}

class Square extends Shape {
    public Square(Renderer renderer) {
        super(renderer);
    }

    @Override
    public void draw() {
        renderer.render();
    }
}

// Usage:
Renderer openGLRenderer = new OpenGLRenderer();
Shape square = new Square(openGLRenderer);
square.draw();
```

## Composite Pattern

The Composite pattern allows you to compose objects into tree structures to represent part-whole hierarchies. This is useful when you want clients to treat individual objects and compositions of objects uniformly.

```java
interface Component {
    void operation();
}

class Leaf implements Component {
    @Override
    public void operation() {
        // Leaf-specific implementation
    }
}

class Composite implements Component {
    private List<Component> components = new ArrayList<>();

    public void add(Component component) {
        components.add(component);
    }

    public void remove(Component component) {
        components.remove(component);
    }

    @Override
    public void operation() {
        for (Component component : components) {
            component.operation();
        }
    }
}

// Usage:
Composite composite = new Composite();
composite.add(new Leaf());
composite.add(new Leaf());
composite.operation();
```

## Decorator Pattern

The Decorator pattern attaches additional responsibilities to an object dynamically without modifying its structure. This is useful when you need to add new functionality to objects without affecting other objects of the same class.

```java
interface Component {
    void operation();
}

class ConcreteComponent implements Component {
    @Override
    public void operation() {
        // Implementation
    }
}

abstract class Decorator implements Component {
    protected Component component;

    public Decorator(Component component) {
        this.component = component;
    }

    public abstract void operation();
}

class ConcreteDecorator extends Decorator {
    public ConcreteDecorator(Component component) {
        super(component);
    }

    @Override
    public void operation() {
        component.operation();
        // New functionality
    }
}

// Usage:
Component component = new ConcreteComponent();
Component decorator = new ConcreteDecorator(component);
decorator.operation();
```

## Facade Pattern

The Facade pattern provides a unified interface to a set of interfaces in a subsystem. This is useful when you want to simplify the usage of a complex system by providing a single entry point.

```java
class SubsystemA {
    public void operationA() {
        // Implementation
    }
}

class SubsystemB {
    public void operationB() {
        // Implementation
    }
}

class Facade {
    private SubsystemA subsystemA;
    private SubsystemB subsystemB;

    public Facade() {
        subsystemA = new SubsystemA();
        subsystemB = new SubsystemB();
    }

    public void operation() {
        subsystemA.operationA();
        subsystemB.operationB();
    }
}

// Usage:
Facade facade = new Facade();
facade.operation();
```

## Flyweight Pattern

The Flyweight pattern minimizes memory usage by sharing as much data as possible with other similar objects. This is useful when you have a large number of objects that have some shared state.

```java
class Flyweight {
    private String sharedState;

    public Flyweight(String sharedState) {
        this.sharedState = sharedState;
    }

    public void operation(String uniqueState) {
        // Implementation using sharedState and uniqueState
    }
}

class FlyweightFactory {
    private Map<String, Flyweight> flyweights = new HashMap<>();

    public Flyweight getFlyweight(String key) {
        Flyweight flyweight = flyweights.get(key);
        if (flyweight == null) {
            flyweight = new Flyweight(key);
            flyweights.put(key, flyweight);
        }
        return flyweight;
    }
}

// Usage:
FlyweightFactory factory = new FlyweightFactory();
Flyweight flyweight = factory.getFlyweight("sharedState");
flyweight.operation("uniqueState");
```

## Proxy Pattern

The Proxy pattern provides a surrogate or placeholder for another object to control access to it. This is useful when you want to defer object creation, add access control, or add extra functionality during access to an object.

```java
interface Subject {
    void request();
}

class RealSubject implements Subject {
    @Override
    public void request() {
        // RealSubject-specific implementation
    }
}

class Proxy implements Subject {
    private RealSubject realSubject;

    @Override
    public void request() {
        if (realSubject == null) {
            realSubject = new RealSubject();
        }
        realSubject.request();
    }
}

// Usage:
Subject proxy = new Proxy();
proxy.request();
```

## Chain of Responsibility Pattern

The Chain of Responsibility pattern gives more than one object a chance to handle a request. Objects in the chain are linked so that the request is passed along the chain until an object handles it.

```java
abstract class Handler {
    protected Handler successor;

    public void setSuccessor(Handler successor) {
        this.successor = successor;
    }

    public abstract void handleRequest(int request);
}

class ConcreteHandlerA extends Handler {
    @Override
    public void handleRequest(int request) {
        if (request < 10) {
            // Handle request
        } else if (successor != null) {
            successor.handleRequest(request);
        }
    }
}

class ConcreteHandlerB extends Handler {
    @Override
    public void handleRequest(int request) {
        if (request >= 10 && request < 20) {
            // Handle request
        } else if (successor != null) {
            successor.handleRequest(request);
        }
    }
}

// Usage:
Handler handlerA = new ConcreteHandlerA();
Handler handlerB = new ConcreteHandlerB();
handlerA.setSuccessor(handlerB);
handlerA.handleRequest(15);
```

## Command Pattern

The Command pattern turns a request into a standalone object that contains all information about the request. This is useful when you want to parameterize objects with different requests, queue requests, or log requests.

```java
interface Command {
    void execute();
}

class ConcreteCommand implements Command {
    private Receiver receiver;

    public ConcreteCommand(Receiver receiver) {
        this.receiver = receiver;
    }

    @Override
    public void execute() {
        receiver.action();
    }
}

class Receiver {
    public void action() {
        // Implementation
    }
}

class Invoker {
    private Command command;

    public void setCommand(Command command) {
        this.command = command;
    }

    public void executeCommand() {
        command.execute();
    }
}

// Usage:
Receiver receiver = new Receiver();
Command command = new ConcreteCommand(receiver);
Invoker invoker = new Invoker();
invoker.setCommand(command);
invoker.executeCommand();
```

## Iterator Pattern

The Iterator pattern provides a way to access the elements of an aggregate object without exposing its underlying representation. This is useful when you want to traverse through different data structures in a uniform way.

```java
interface Iterator<T> {
    boolean hasNext();
    T next();
}

interface Aggregate<T> {
    Iterator<T> createIterator();
}

class ConcreteAggregate implements Aggregate<String> {
    private List<String> data = new ArrayList<>();

    public void add(String value) {
        data.add(value);
    }

    @Override
    public Iterator<String> createIterator() {
        return new ConcreteIterator();
    }

    private class ConcreteIterator implements Iterator<String> {
        private int currentIndex = 0;

        @Override
        public boolean hasNext() {
            return currentIndex < data.size();
        }

        @Override
        public String next() {
            return hasNext() ? data.get(currentIndex++) : null;
        }
    }
}

// Usage:
ConcreteAggregate aggregate = new ConcreteAggregate();
aggregate.add("A");
aggregate.add("B");
aggregate.add("C");

Iterator<String> iterator = aggregate.createIterator();
while (iterator.hasNext()) {
    System.out.println(iterator.next());
}
```

## Mediator Pattern

The Mediator pattern defines an object that encapsulates how a set of objects interact. This is useful when you want to manage complex interactions between different objects while minimizing object dependencies.

```java
interface Mediator {
    void send(String message, Colleague colleague);
}

abstract class Colleague {
    protected Mediator mediator;

    public Colleague(Mediator mediator) {
        this.mediator = mediator;
    }
}

class ConcreteColleagueA extends Colleague {
    public ConcreteColleagueA(Mediator mediator) {
        super(mediator);
    }

    public void send(String message) {
        mediator.send(message, this);
    }

    public void receive(String message) {
        // Handle message
    }
}

class ConcreteColleagueB extends Colleague {
    public ConcreteColleagueB(Mediator mediator) {
        super(mediator);
    }

    public void send(String message) {
        mediator.send(message, this);
    }

    public void receive(String message) {
        // Handle message
    }
}

class ConcreteMediator implements Mediator {
    private ConcreteColleagueA colleagueA;
    private ConcreteColleagueB colleagueB;

    public void setColleagueA(ConcreteColleagueA colleagueA) {
        this.colleagueA = colleagueA;
    }

    public void setColleagueB(ConcreteColleagueB colleagueB) {
        this.colleagueB = colleagueB;
    }

    @Override
    public void send(String message, Colleague colleague) {
        if (colleague == colleagueA) {
            colleagueB.receive(message);
        } else {
            colleagueA.receive(message);
        }
    }
}

// Usage:
ConcreteMediator mediator = new ConcreteMediator();
ConcreteColleagueA colleagueA = new ConcreteColleagueA(mediator);
ConcreteColleagueB colleagueB = new ConcreteColleagueB(mediator);
mediator.setColleagueA(colleagueA);
mediator.setColleagueB(colleagueB);

colleagueA.send("Hello from A");
colleagueB.send("Hello from B");
```

## Memento Pattern

The Memento pattern captures and externalizes an object's internal state so that the object can be restored to this state later without violating encapsulation. This is useful when you want to save an object's state for later use or when you want to implement undo/redo functionality in an application.

```java
class Originator {
    private String state;

    public void setState(String state) {
        this.state = state;
    }

    public Memento saveState() {
        return new Memento(state);
    }

    public void restoreState(Memento memento) {
        state = memento.getState();
    }
}

class Memento {
    private String state;

    public Memento(String state) {
        this.state = state;
    }

    public String getState() {
        return state;
    }
}

class Caretaker {
    private List<Memento> mementoList = new ArrayList<>();

    public void add(Memento memento) {
        mementoList.add(memento);
    }

    public Memento get(int index) {
        return mementoList.get(index);
    }
}

// Usage:
Originator originator = new Originator();
Caretaker caretaker = new Caretaker();

originator.setState("State1");
caretaker.add(originator.saveState());

originator.setState("State2");
caretaker.add(originator.saveState());

// Restore state
originator.restoreState(caretaker.get(0));
```

## Observer Pattern

The Observer pattern defines a one-to-many dependency between objects so that when one object changes state, all its dependents are notified and updated automatically. This is useful when you want to establish a relationship between objects where a change in one object leads to changes in other objects.

```java
interface Observer {
    void update(String message);
}

interface Subject {
    void attach(Observer observer);
    void detach(Observer observer);
    void notifyObservers();
}

class ConcreteSubject implements Subject {
    private List<Observer> observers = new ArrayList<>();
    private String message;

    public void setMessage(String message) {
        this.message = message;
        notifyObservers();
    }

    @Override
    public void attach(Observer observer) {
        observers.add(observer);
    }

    @Override
    public void detach(Observer observer) {
        observers.remove(observer);
    }

    @Override
    public void notifyObservers() {
        for (Observer observer : observers) {
            observer.update(message);
        }
    }
}

class ConcreteObserver implements Observer {
    private String name;

    public ConcreteObserver(String name) {
        this.name = name;
    }

    @Override
    public void update(String message) {
        System.out.println(name + ": " + message);
    }
}

// Usage:
ConcreteSubject subject = new ConcreteSubject();

Observer observer1 = new ConcreteObserver("Observer1");
Observer observer2 = new ConcreteObserver("Observer2");

subject.attach(observer1);
subject.attach(observer2);

subject.setMessage("Hello, observers!");
```

## State Pattern

The State pattern allows an object to alter its behavior when its internal state changes. The object will appear to change its class. This is useful when you want to manage the behavior of an object depending on its current state.

```java
interface State {
    void handle();
}

class ConcreteStateA implements State {
    @Override
    public void handle() {
        // Implementation for state A
    }
}

class ConcreteStateB implements State {
    @Override
    public void handle() {
        // Implementation for state B
    }
}

class Context {
    private State state;

    public void setState(State state) {
        this.state = state;
    }

    public void handle() {
        state.handle();
    }
}

// Usage:
Context context = new Context();
State stateA = new ConcreteStateA();
State stateB = new ConcreteStateB();

context.setState(stateA);
context.handle();

context.setState(stateB);
context.handle();
```

## Strategy Pattern

The Strategy pattern defines a family of algorithms, encapsulates each one, and makes them interchangeable. This is useful when you want to perform different tasks that can be done in different ways, and you want to be able to switch between the algorithms easily.

```java
interface Strategy {
    void execute();
}

class ConcreteStrategyA implements Strategy {
    @Override
    public void execute() {
        // Implementation for strategy A
    }
}

class ConcreteStrategyB implements Strategy {
    @Override
    public void execute() {
        // Implementation for strategy B
    }
}

class Context {
    private Strategy strategy;

    public void setStrategy(Strategy strategy) {
        this.strategy = strategy;
    }

    public void executeStrategy() {
        strategy.execute();
    }
}

// Usage:
Context context = new Context();
Strategy strategyA = new ConcreteStrategyA();
Strategy strategyB = new ConcreteStrategyB();

context.setStrategy(strategyA);
context.executeStrategy();

context.setStrategy(strategyB);
context.executeStrategy();
```

## Template Method Pattern

The Template Method pattern defines the skeleton of an algorithm in a method, deferring some steps to subclasses. This is useful when you want to define the structure of an algorithm but allow subclasses to redefine certain steps of the algorithm without changing the algorithm's structure.

```java
abstract class AbstractClass {
    public final void templateMethod() {
        primitiveOperation1();
        primitiveOperation2();
        // Other steps
    }

    public abstract void primitiveOperation1();

    public abstract void primitiveOperation2();
}

class ConcreteClass extends AbstractClass {
    @Override
    public void primitiveOperation1() {
        // Implementation for step1
    }

    @Override
    public void primitiveOperation2() {
        // Implementation for step2
    }
}

// Usage:
AbstractClass concreteClass = new ConcreteClass();
concreteClass.templateMethod();
```

## Visitor Pattern

The Visitor pattern allows you to add new operations to a set of classes without modifying the classes. Instead, you create a new class that implements the new operation and visits the existing classes. This is useful when you want to extend the functionality of a set of classes without modifying their implementation.

```java
interface Visitor {
    void visitConcreteElementA(ConcreteElementA elementA);
    void visitConcreteElementB(ConcreteElementB elementB);
}

class ConcreteVisitor1 implements Visitor {
    @Override
    public void visitConcreteElementA(ConcreteElementA elementA) {
        // Implementation for visiting ConcreteElementA in ConcreteVisitor1
    }

    @Override
    public void visitConcreteElementB(ConcreteElementB elementB) {
        // Implementation for visiting ConcreteElementB in ConcreteVisitor1
    }
}

interface Element {
    void accept(Visitor visitor);
}

class ConcreteElementA implements Element {
    @Override
    public void accept(Visitor visitor) {
        visitor.visitConcreteElementA(this);
    }
}

class ConcreteElementB implements Element {
    @Override
    public void accept(Visitor visitor) {
        visitor.visitConcreteElementB(this);
    }
}

// Usage:
Element elementA = new ConcreteElementA();
Element elementB = new ConcreteElementB();
Visitor visitor = new ConcreteVisitor1();

elementA.accept(visitor);
elementB.accept(visitor);
```

These design patterns and code snippets provide a foundation for creating maintainable and extensible Java applications. By understanding these patterns and applying them appropriately, you can create higher-quality software that is easier to understand and modify.

In Java, access modifiers are keywords that determine the visibility and accessibility of class members (i.e., variables, methods, and inner classes). There are three main access modifiers: private, protected, and public.

1. private:

The private access modifier is the most restrictive one. It limits the visibility of a class member only to the class where it's defined. No other class (including subclasses) can access private members.

Example:

```java
class Animal {
    private String species;

    private void setSpecies(String species) {
        this.species = species;
    }
}

class Dog extends Animal {
    void setDogSpecies(String species) {
        // This will result in a compile-time error since setSpecies is private in the Animal class
        setSpecies(species);
    }
}
```

2. protected:

The protected access modifier allows the class member to be accessed within the class it's defined in, its subclasses, and any other classes within the same package.

Example:

```java
package animal;

public class Animal {
    protected String species;

    protected void setSpecies(String species) {
        this.species = species;
    }
}

package animal;

public class Dog extends Animal {
    void setDogSpecies(String species) {
        // This is valid since setSpecies is protected and Dog is a subclass of Animal
        setSpecies(species);
    }
}

package client;

import animal.Animal;

public class AnimalClient {
    public static void main(String[] args) {
        Animal animal = new Animal();
        // This will result in a compile-time error since setSpecies is protected
        // and AnimalClient is not in the same package as Animal
        animal.setSpecies("mammal");
    }
}
```

3. public:

The public access modifier allows the class member to be accessed from any class, regardless of package or inheritance.

Example:

```java
package animal;

public class Animal {
    public String species;

    public void setSpecies(String species) {
        this.species = species;
    }
}

package client;

import animal.Animal;

public class AnimalClient {
    public static void main(String[] args) {
        Animal animal = new Animal();
        // This is valid since setSpecies is public
        animal.setSpecies("mammal");
    }
}
```

In summary:

- private: accessible only within the class it's defined in.
- protected: accessible within the class it's defined in, its subclasses, and classes in the same package.
- public: accessible from any class, regardless of package or inheritance.

```java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;

public class SimpleIOExample {

    public static void main(String[] args) {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        PrintWriter writer = new PrintWriter(System.out, true);

        writer.println("Welcome to Simple I/O Example!");

        try {
            writer.print("Enter your name: ");
            String name = reader.readLine();

            writer.print("Enter your age: ");
            int age = Integer.parseInt(reader.readLine());

            writer.print("Enter your height (in meters): ");
            float height = Float.parseFloat(reader.readLine());

            writer.print("Do you love programming? (true/false): ");
            boolean lovesProgramming = Boolean.parseBoolean(reader.readLine());

            writer.println("\nYour details:");
            writer.println("Name: " + name);
            writer.println("Age: " + age);
            writer.println("Height: " + height + " meters");
            writer.println("Loves Programming: " + lovesProgramming);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
```
This Java program demonstrates simple input/output operations using stdin (System.in, BufferedReader) and stdout (System.out, PrintWriter). It reads a user's name, age, height, and asks whether they love programming, and then prints the input back to the console.
