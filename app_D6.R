#GroupD6


library(shiny)
library(shinydashboard)
library(shinyWidgets)

source("D6_button.R")

ui <- dashboardPage(
  dashboardHeader(title = "Group D6, rotational symmetries of the hexagon",
                  titleWidth = 500),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      column(width=4,
             box(
               width = NULL,
               height = 500,
               h3 ("Elements of the group"),
               h4("The identity"),
               controlRow1("ctrlI"),
               
               h4("Order 6 elements (60 and 300 degree rotations)"),
               controlRow2(
                 c("ctrl123456", "ctrl165432")
               ),
               
               h4("Order 3 elements (120 and 240 degree rotations)"),
               controlRow2(
                 c("ctrl246135", "ctrl264153")
               ),   
               
               h4("Order 2 elements (180 degree rotations)"),
               controlRow1(
                 c("ctrl362514")
               ),   
    
               h4("Order 2 elements (180 degree edge-edge flip)"),
               controlRow3(
                 c("ctrl453612", "ctrl562314", "ctrl342516")
               ),
               
               h4("Order 2 elements (180 degree vertex-vertex flip)"),
               controlRow3(
                 c("ctrl4613", "ctrl3526", "ctrl2415")
               ),

             ),
             box(
               width = NULL,
               height = 150,
               title = "Subgroups",
               buttonRow4(
                 inputIds = c("btnC2", "btnC3", "btnC6", "btnS3"),
                 labels = list("Show C2", "ShowC3", "ShowC6", "ShowS3"),
                 btnStyle = "padding:4px;font-size:120%"
               )   
             ),#box
             box(
               width = NULL,
               height = 100,
               title = "Cosets",
               buttonRow2(
                 inputIds = c("btnLC", "btnRC"),
                 labels = list("Left Cosets", "Right Cosets"),
                 btnStyle = "padding:4px;font-size:120%"
               )  #agb
             ),#box
             box(
               width = NULL,
               height = 120,
               title = "Conjugate Subgroup",
               buttonRow2(
                 inputIds = c("btnmark", "btnconj"),
                 labels = list("Select a", "Generate Subgroup"),
                 btnStyle = "padding:4px;font-size:120%"
               ),  
               h4(uiOutput("conjmsg"))
             ),#box
             box(
               width = NULL,
               height = 120,
               title = "Generate a Subgroup",
               buttonRow4(
                 inputIds = c("btnmarkgena", "btnmarkgenb", "btngen", "btnclear"),
                 labels = list("Generator a", "Generator b","Generate","Clear"),
                 btnStyle = "padding:4px;font-size:120%"
               ),  
               h4(uiOutput("genmsg"))
             )#box
             
             
      ),
      #col
      column(
        width = 8,
        box(
          width = NULL,
          height = 380,
          fluidRow(
            column(
              width = 8,
              h3("Inputs and Products"),
              
              htmlOutput("results"),
              tags$head(tags$style("#results{color:red; font-size:20px; font-style:italic; 
overflow-y:scroll; max-height: 300px; background: ghostwhite;}"))
            ),
            column(
              width = 4,
              actionBttn("reset", "Clear Inputs and Products")
            )
            
          )
        ),
        box(width = NULL,
            height = 600,
            tableOutput("multable")
        )
        
        
      )
    )  #fluid
  )  
)


source("D6calc.R")
source("perm.R")

#Global variables accessible to server()
N <- 12

D6DF <- makeD6data()

#colors for cosets
color.list <- c("pink","aquamarine","beige","hotpink", "violet")

#Output to display in the text box
result.list <- ""
#Result of all multiplications so far
product <- "I"
subgroup <- numeric(0)
conjugating <- FALSE
generating <- 0
a <-"I"
gena <- "I"
genb <- "I"

#Computes a product as specified by "a" and "b" in vector v
evaluate <- function(v,a,b) {
  result <- "I"
  for (i in 1:length(v)){
    result <- Perm.multiply(result,ifelse(v[i]=="a",a,b))
  }
  return (result)
}


server <- function(input, output, session) {
  #Elements in the chosen subgroup
  displayButton <- function(i) {
    renderUI({actionButton(D6DF[i,1],D6DF[i,2],
                           style=paste("padding:4px;
                   font-size:120%;background:",D6DF[i,3]))}) 
  }
  #show all the buttons
  showButtons <- function() {
    output$ctrl123456 <- displayButton(1)
    output$ctrl246135<- displayButton(2)                                     
    output$ctrl362514<- displayButton(3)
    output$ctrl264153<- displayButton(4)
    output$ctrl165432 <- displayButton(5)
    output$ctrl4613<- displayButton(6)                                     
    output$ctrl3526<- displayButton(7)
    output$ctrl2415<- displayButton(8)
    output$ctrl453612 <- displayButton(9)
    output$ctrl562314<- displayButton(10)                                     
    output$ctrl342516<- displayButton(11)
    output$ctrlI<- displayButton(12)
  }
  showButtons()
  #Display the multiplication table
  tbl <- outer(D6DF[,2],D6DF[,2],Vectorize(Perm.multiply,c("a","b")))
  colnames(tbl) <- D6DF[,2]
  rownames(tbl) <- D6DF[,2] 
  output$multable <- renderTable(tbl,rownames = TRUE)
  #Multiplies by a specified permutation and displays all calculations so far
  compute.and.show <- function(perm){
    if (conjugating) {
      a <<- perm
      output$conjmsg <- renderUI(paste0("Conjugating by element ",perm,collapse=""))
      conjugating <<- FALSE
      return()
    }
    if (generating==1) {
      gena <<- perm
      output$genmsg <- renderUI(paste0("Generating with element ",gena,collapse=""))
      return()
    }
    if (generating==2) {
      genb <<- perm
      output$genmsg <- 
        renderUI(paste0("Generating with elements ",gena," and ", genb,collapse=""))
      return()
    }
    product <<- Perm.multiply(perm,product)
    line.out <- paste(perm,product,sep = "&emsp;")
    result.list <<- paste(result.list, line.out, "<br/>")
    output$results<-renderUI(HTML(result.list))
  }
  #Marks all elements in a subgroup with a color
  mark.subgroup <- function() {
    for (i in 1:N){
      D6DF$color[i] <<- ifelse(i  %in% subgroup,"yellow","gray90")
    }
  }
  #Event handlers for all the element buttons 
  observeEvent(input$btnI,{
    compute.and.show("I")
  })
  observeEvent(input$btn123456,{
    compute.and.show("(123456)")
  })
  observeEvent(input$btn246135,{
    compute.and.show("(135)(246)")
  })
  observeEvent(input$btn362514,{
    compute.and.show("(14)(25)(36)")
  })
  observeEvent(input$btn264153,{
    compute.and.show("(153)(264)")
  })
  observeEvent(input$btn165432,{
    compute.and.show("(165432)")
  })
  observeEvent(input$btn4613,{
    compute.and.show("(13)(46)")
  })
  observeEvent(input$btn3526,{
    compute.and.show("(26)(35)")
  })
  observeEvent(input$btn2415,{
    compute.and.show("(15)(24)")
  })
  observeEvent(input$btn453612,{
    compute.and.show("(12)(36)(45)")
  })
  observeEvent(input$btn562314,{
    compute.and.show("(14)(23)(56)")
  })
  observeEvent(input$btn342516,{
    compute.and.show("(16)(25)(34)")
  })
  
  
  #continue from here!
  #The reset button clears the output and reinitializes the product
  observeEvent(input$reset,{
    result.list <<- ""
    product <<- "I"
    output$results<-renderUI(HTML(result.list))
  })
  #Event handlers for the subgroup buttons
  observeEvent(input$btnC2,{
    subgroup <<- c(6,12)
    mark.subgroup()
    showButtons()
  })
  observeEvent(input$btnC3,{
    subgroup <<- c(2,4,12)
    mark.subgroup()
    showButtons()
  })
  observeEvent(input$btnC6,{
    subgroup <<- c(1,2,3,4, 5, 12)
    mark.subgroup()
    showButtons()
  })
  observeEvent(input$btnS3,{
    subgroup <<- c(2, 4, 6, 7, 8 ,12)
    mark.subgroup()
    showButtons()
  })
  #Event handler for left cosets
  observeEvent(input$btnLC,{
    mark.subgroup()
    idx = 1   #index into the color list -- one for each coset
    #Keep creating cosets as long as there are elements that are still gray
    while(length(which(D6DF$color == "gray90") >0)){
      #Find the first unassigned group element
      in.coset <- which(D6DF$color == "gray90")[1]
      #Generate its left coset and put a new color on the buttons
      for (j in 1:N) {
        if(j %in% subgroup) {
          element <- Perm.multiply(D6DF[in.coset,2],D6DF[j,2])
          k <- which(D6DF[,2] == element)
          D6DF[k,3] <<- color.list[idx]
        }
      }
      idx <- idx + 1
    }
    showButtons()
  })
  #Right cosets work the same way
  observeEvent(input$btnRC,{
    mark.subgroup()
    idx = 1   #index into the color list -- one for each coset
    #Keep creating cosets as long as there are elements that are still gray
    while(length(which(D6DF$color == "gray90") >0)){
      #Find the first unassigned group element
      in.coset <- which(D6DF$color == "gray90")[1]
      #Generate its left coset and put a new color on the buttons
      for (j in 1:N) {
        if(j %in% subgroup) {
          element <- Perm.multiply(D6DF[j,2],D6DF[in.coset,2])
          k <- which(D6DF[,2] == element)
          D6DF[k,3] <<- color.list[idx]
        }
      }
      idx <- idx + 1
    }
    showButtons()
  })
  
  
  observeEvent(input$btnmark,{
    conjugating <<- TRUE
    output$conjmsg <- renderUI("Click the button for the desired element a")
  })
  observeEvent(input$btnmarkgena,{
    generating <<- 1
    D6DF[,3] <<- rep("gray90",N)
    showButtons()
    output$genmsg <- renderUI("Click the button for generator a")
  })
  observeEvent(input$btnmarkgenb,{
    generating <<- 2
    D6DF[,3] <<- rep("gray90",N)
    showButtons()
    output$genmsg <- renderUI("Click the button for generator b")
  })
  #Generate random sequences of generators.
  #If we generate more than half the group, it's the entire group
  #This algorithm could turn out to be inefficient,and in principle it can fail
  observeEvent(input$btngen,{
    subgroup <<- numeric(0)
    for (j in 1:(4*N)) {
      v <- sample(c("a","b"),sample(7:10,1),replace = TRUE)
      element <- evaluate(v,gena,genb)
      k <- which(D6DF[,2] == element)[1]
      if(!(k %in% subgroup)){
        subgroup <<- c(subgroup,k)
        D6DF[k,3] <<- "yellow"
      }
      #If subgroup has more than N/2 elements, it's the entire group
      if (length(subgroup) > N/2){
        subgroup <<- 1:N
        break
      } 
    }  
    mark.subgroup()
    showButtons()
    output$genmsg <- 
      renderUI(paste0("The subgroup generated by ",gena," and ", genb," is now yellow"))
  })
  observeEvent(input$btnclear,{
    subgroup <<- rep(FALSE,N)
    generating <<- 0
    gena <<- "I"
    genb <<- "I"
    mark.subgroup()
    showButtons()
    output$genmsg <- renderUI("")
  })
  observeEvent(input$btnconj,{
    aInv <- Perm.inverse(a)
    D6DF[,3] <<- rep("gray90",N)
    for (j in 1:N) {
      if (j %in% subgroup){
        element <- Perm.conjugate(a,D6DF[j,2])
        k <- which(D6DF[,2] == element)[1]
        D6DF[k,3] <<- "pink"
      }
    }  
    showButtons()
    output$conjmsg <- renderUI(paste0("The subgroup ",a,"H",aInv," is now pink"))
  })}

# Run the application 
shinyApp(ui = ui, server = server)



