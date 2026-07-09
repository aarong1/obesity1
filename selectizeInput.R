

library(shiny)
  
  ui <- fluidPage(
      tags$head(
      HTML('<style>
    span.label {
    font-size: 14px;
    
    color: steelblue;
    color:mediumseagreen;
    color:var(--bs-success);

    }
    
     span.caption {
    font-size: 10px;
    
    color: grey;
    }
  
               .selectize-control.contacts .selectize-input > div .email {
    opacity: 0.8;
  }
  .selectize-control.contacts .selectize-input > div .name + .email {
    margin-left: 5px;
  }
  .selectize-control.contacts .selectize-input > div .email:before {
    content: "<";
  }
  .selectize-control.contacts .selectize-input > div .email:after {
    content: ">";
  }
  .selectize-control.contacts .selectize-dropdown .caption {
    font-size: 12px;
    display: block;
    color: #a0a0a0;
  }
  </style>
  ')
    ),
    actionButton('f','f'),
    selectizeInput(
      "select_to", 
      "To:", 
      choices = list(
        age = list("Brian Reavis" = "brian@thirdroute.com" ),
        age = list("Nikola Tesla" = "nikola@tesla.com"),
        age = list("someone@gmail.com")
      ),# Populated via options below
      multiple = TRUE,
     
      options = list(

        persist = T,
        maxItems = NULL,
        valueField = "email",
        labelField = "name",
        # searchField = c("name", "email"),
        # options = list(
        #   list(email = "brian@thirdroute.com", name = "Brian Reavis"),
        #   list(email = "nikola@tesla.com", name = "Nikola Tesla"),
        #   list(email = "someone@gmail.com")
        # ),
        # Render custom HTML for items and options
        render = I("{
    item: function(item, escape) {
      console.log(item);
      var name = item.email ? '<span class=\"name\">' + escape(item.email) + '</span>' : '';
      return '<div>' + '<span class=\"email\">' + name + '</span></div>';
    },
    option: function(item, escape) {
      var label = item.name || item.email;
      var caption = item.name ? item.email : null;
      return '<div>' +
       (caption ? '<span class=\"label\">' + escape(caption) + '</span>' : '') + '<span class=\"caption\">' + escape(label) + '</span>' +
        
      '</div>';
    }
  }") #,
        # Custom email validation logic
        # createFilter = I('function(input) {
        #   var REGEX_EMAIL = "([a-z0-9!#$%&\'*+/=?^_`{|}~-]+(?:.[a-z0-9!#$%&\'*+/=?^_`{|}~-]+)*@" + "(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?)";
        #   return new RegExp("^" + REGEX_EMAIL + "$", "i").test(input);
        # }'),
        # create = I('function(input) {
        #   return { email: input };
        # }')
      )
    )
  )
  
  server <- function(input, output, session) {
    
    observeEvent(input$f,ignoreInit = T,{
      
    updateSelectizeInput(session, inputId='select_to', choices = 
                           pop %>%
                           count(age10) %>%
                           t() %>% 
                           as.data.frame() %>% 
                           setNames(format(
                             big.mark=",",
                             as.numeric(.[2,])*10#model_specification$population$scale_down_factor
                             )) %>%  
                           sapply(FUN = function(x){
                             list( 
                               x[[1]])})
                         
      # list(
      #   age=list(email = "brian@thirdroute.com", name = "Brian Reavis"),
      #   age=list(email = "nikola@tesla.com", name = "Nikola Tesla"),
      #   age=list(email = "someone@gmail.com")
      # )
        )
    })
    
    observe({
      print(input$select_to)
      })
  }
  
  shinyApp(ui, server)
