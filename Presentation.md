## 1: Welcome to this presentation, this is my final project for CS421, programming languages and compiler. In this project I will review a paper for "Comparing Libraries for Generic Programming in Haskell.

The paper is fairly old, it was published in 2008 by Alexey Rodriguez

My name is Himangshu, and I am doing is project alone.

## 2: Motivation:

The goal of generic programming is to express algorithms and data structures in a broadly adaptable, interoperable form that allows their direct use in software construction.
The paper highlights the proliferation of generic programming libraries in Haskell and the need for a systematic comparison. Then it develops a set of benchmarks to evaluate how well these libraries support typical generic programming scenarios, such as equality, serialization, and traversals.

## 4: Why Compare 
1. First of all, 
My personal motivation to review this paper stems from my experience in using Generics in Java as a programmer myself. I had limited knowledge of the internal implementation details of those libraries at the compiler level. The paper that I am reviewing provides an in-depth analysis of various Haskell libraries for generic programming, this gives me the opportunity to deepen my understanding of these concepts.

2. In general there are a lot of libraries, this comparison help you choose the right library. Also, by comparing them, we might be able to produce a hierarchy of libraries 
3. Then the third reason is that Haskell itself is powerful enough to define generic programs. Although we incur into some syntactic overhead, but at least your tool chain becomes a bit simpler because you don't have to use preprocessors. So our contributions are...

## Libraries Evaluated
"The libraries evaluated in the study include 
1. LIGD, 
2. Spine, 
3. EMGM, 
4. SYB1_2, 
5. RepLib, 
6. SmashA, 
7. PolyP, 
8. Uniplate, 
9. syb3, and multirec. 

Each library has its strengths and weaknesses, making them suitable for different scenarios. For example, some libraries excel in supporting a wide range of types, while others offer better performance or ease of use."

## 5: Benchmarking Criteria
Now, how do we define the criteria for such comparison ??

The criterion for defining the benchmark are divided in 3 main categories. Types, Expressiveness and Usability.

The first criteria is - What are the datatypes the generic programming function can be applied, this is called universe size. We can have a very exotic datatype such as nested datatypes, or mutually recursive datatypes, i.e. we have system of datatypes that mutually call each other, so a librabry that can't deal with such datatypes will not be considered very good.

Another criteria is : is it possible to restrict the use of a generic function to a certain set of datatypes. How the compiler handles the error a unsupported datatype is passed.  This is defined as subuniverses.

Second set of criteria specifies what are the generic functions that can be defined. There are lot of subcategories defined under Expressiveness in the paper, in my presentation I am going to discuss only the most important ones. Rest of them can be found in the project report.

1. Can you pass a fucntion to another fucntion, i.e. is this a first class GF. This is tested by gmapQ, the function that applies a generic function argument to all constructor arguments
2. Then the second point is abstraction over type constructors. So the generic map library is abstracting over F, which is of kind star to star. This is important if, for example, you want to write mapping over binary trees that you have defined or other data structures. 
3. Can a generic function contain specific behaviour for a particular datatype, and let the remaining datatypes be handled generically? In this situation, ad- hoc, datatype-specific definitions are used instead of uniformly generic behaviour. This is tested by the selectSalary function, which consists of cases that perform a traversal over a datatype, accumulating the values collected by the Salary ad-hoc case

Rest of them are explained in the summary report.

Next set of criteria is about usability, this defines how convenient a library is to use, efficiency, quality

1. Performance: Compares runtime for each functions
2. Portability: How easier it is to port functions between different Haskell compilers
3. Overhead of library use: This measures the additional programming effort required when using the generic programming library, including automatic generation of structure representations and instantiating generic functions.
4. Practical aspects: Considers whether there is an implementation, its maintenance status, and the quality of its documentation.
5. Ease of learning and use: Some libraries have complex implementation mechanisms, making them more difficult to learn and use.


## 7: Implementation
Alright, so far we have the criteria defined for the evaluation. Now, how do we use the information to acutally test the libraries.
 
"The implementation involves creating benchmarks for various generic programming scenarios, such as equality, serialization, and traversals. These benchmarks help evaluate the libraries' support for generic programming scenarios. I faced several challenges, such as updating dependencies and configuring the environment to accommodate necessary libraries and compiler options.

I will talk about 2 test scenarios in the next 2 slides:

## 8: First class generic function:

Let's see an example for first-class generic functions. We'll try to write the function gmapq.

Basically idea of gmapq is that if we're given a generic function, then this function is applied to the children of its constructor. 

So, Scrap boilerplate has no problem with this criteria because generic functions are just Haskell functions, so that means that higher-order generic functions are just rank-end Haskell functions. So there's no problem. Y ou check it on the good side. 

For EMGM, it is a bit more tricky. But we can still write a higher-order generic functions even though they're more difficult to write than other generic functions such as equality. 

So in our scoring, we have three categories. We have those libraries that...

## 8.2 Adhoc-definations
Now let's look at the ad hoc definitions. We have seen this function already. So you apply salaries to data structure A. The type for EMGM doesn't change much. You just have this additional context because it's a different approach. So in SYB, the definition is very simple. We already saw it. So the support is good. 

In EMGM, it has good support as well because the generic function definition is given in instance declarations. All you have to do is have an instance declaration that corresponds to salaries. 

So the verdict is that the particular libraries that I have shown now, so Scruffy-Boretta and EMGM, they have good support for ad hoc cases. But this is not the case, for example, of LIGD and SPINE. Why? Because if you want to extend to give an ad hoc case for a data type that you have just defined, you have to go back to the source of the library and then modify it, which is really undesirable. Right. So the last thing for this scenario is that you want to...

## 8: Running the Tests
The tests are located under the `/comparison` directory in the root of the project. I have already documented the steps to run in the readme file of my github repo. This is just a screenshot of the steps. 

## 9: Results
"The benchmarking results show that libraries like Spine, EMGM, and Smash generally support more features than others, excelling in areas like universe size, first-class generic functions, and performance. 

However, they might introduce more overhead in terms of structure representations and work required for function instantiation and definition. Libraries like SYB, while more limited in features, are easier to learn and use for a programmer.

This is the final table from the comparison as described in the paper. I have not been able to run all the tests, but adding this slide for reference. The table has 3 sections each for the criteria defined for the benchmark as explained in earlier slides. Libraries with full black dots are generally more useful.

But it turns out that no single library excels across all criteria. Libraries that perform well in one aspect often struggle in others. For instance, extensible libraries like SYB3 require more boilerplate code than non-extensible ones and support a smaller universe size. On the other hand, EMGM, while providing extensible functions, complicates the definition of higher-order generic functions. The choice of the best generic programming library depends on specific use cases.

In conclusion, the selection of a generic programming library should be based on the specific requirements and scenarios of the application at hand. 

## 12: Questions
"Thank you for watching my presentation. If you have any question please feel free to email me or report an issue in github.