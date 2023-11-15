# S4 classes for talented people Code to go along with a blog post about  object oriented programming in R: http://digitheadslabnotebook.blogspot.com/2012/09/oo-in-r.html

# define an S4 class for people
setClass("Person", representation(name = "character", age = "numeric"), prototype(name = NA_character_, age = NA_real_) )
# define subclasses for different types of people
setClass("Programmer", representation(language = "character"), contains="Person")
setClass("Musician", representation(instrument = "character"), contains="Person")

# create a generic method called 'talent' that dispatches
# on the type of object it's applied to
setGeneric("talent", function(object) { standardGeneric("talent") })
setMethod("talent", signature("Programmer"),function(object) {
  paste("Codes in", paste(object@language, collapse=", "))
})
setMethod("talent",signature("Musician"),
          function(object) {
            paste("Plays the", paste(object@instrument, collapse=", "))
          })

# create some talented people
donald <- new("Programmer",name="Donald Knuth",age=74,language=c("MMIX"))
coltrane <- new("Musician",name="John Coltrane",age=40,instrument=c("Tenor Sax", "Alto Sax"))
miles <- new("Musician",name="Miles Dewey Davis",instrument=c("Trumpet"))
monk <- new("Musician",name="Theloneous Sphere Monk",instrument=c("Piano"))

talent(coltrane)
talent(donald)
talent(monk)

getNameAndTitle <- function(person) {
  sprintf("%s, %s", person@name, class(person))
}

getNameAndTitle(miles)

# Let's write a method that changes the state of an object.
# An employee is a person who has a salary and gets a raise
# now and then.
setClass("Employee", representation(boss = "Person", salary="numeric"), contains = "Person")

setGeneric("raise",function(object, percent=0) {
  standardGeneric("raise")
})

setMethod("raise",signature("Employee"),
          function(object, percent=0) {
            object@salary <- object@salary * (1+(percent/100))
            object
          })

smithers <- new("Employee",name="Waylon Smithers",boss=new("Person",name="Mr. Burns"),salary=100)
raise(smithers, percent=15)
smithers@salary

# The raise method returns the new object with the new
# value for salary. Don't forget to capture it.
smithers <- raise(smithers, percent=15)
smithers@salary

# multiple inheritance: The humble code monkey is a programmer and also an employee
setClass("Code Monkey", contains=c("Programmer","Employee"))

setMethod("talent",signature("Code Monkey"),
          function(object) {
            paste("Codes in", paste(object@language, collapse=", "), "for", object@boss@name)
          })

# This is a code monkey
chris <- new("Code Monkey",name="Chris",age=29,boss=new("Person", name="The Man"),language=c("Java", "R", "Python", "Clojure"),salary=80000)

talent(chris)
chris <-raise(chris,percent=-10)
chris@salary

# Multiple dispatch. I'm curious as to what else you can do
# with method dispatch
setGeneric("together",function(person1, person2) {
  standardGeneric("together")
})

setMethod("together",signature("Musician", "Musician"),
          function(person1, person2) {
            sprintf("%s and %s jam together!", person1@name, person2@name)
          })

setMethod("together",signature("Musician", "Programmer"),
          function(person1, person2) {
            sprintf("%s and %s make electronic music!", person1@name, person2@name)
          })

setMethod("together",signature("Programmer", "Programmer"),
          function(person1, person2) {
            sprintf("%s and %s hack some code!", person1@name, person2@name)
          })

together(miles, coltrane)
together(monk, chris)
together(donald, chris)

# For more on formal classes see:
# S4 Classes in 15 pages, more or less
# How S4 Methods Work by John Chambers
# Hadley Wickham's The S4 object system at https://github.com/hadley/devtools/wiki/S4