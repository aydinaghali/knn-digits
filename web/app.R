library(shiny)
library(shinyjs)
library(caret)
library(magick)

model.knn <- readRDS("../knn_model.rda")

ui <- fluidPage(
  tags$style(HTML("
    .centered {
      display: flex;
      /*justify-content: center;*/
      align-items: center;
      height: 100vh;  /* Full viewport height */
      flex-direction: column;
    }
    
    .big-text {
      font-size: 20px;
    }
  ")),
  
  useShinyjs(),
  
  div(class = "centered",
    br(),
    tags$canvas(id = "drawingCanvas", width = 600, height = 600, style = "border:1px solid black;"),
    br(),
    sliderInput("size", "Brush Size", min = 20, max = 100, value = 40),
    actionButton("clearBtn", "Clear Canvas"),
    br(),
    actionButton("predictBtn", "Predict"),
    br(),
    div(class="big-text",
    textOutput("prediction"))
  ),
  
  tags$script(HTML("
    var canvas = document.getElementById('drawingCanvas');
    var ctx = canvas.getContext('2d');
    var drawing = false;
    var size = 40;
    var lastX, lastY;

    canvas.addEventListener('mousedown', function(e) {
      drawing = true;
      lastX = e.offsetX;
      lastY = e.offsetY;
    });

    canvas.addEventListener('mouseup', function(e) {
      drawing = false;
    });

    canvas.addEventListener('mousemove', function(e) {
      if (drawing) {
        ctx.strokeStyle = 'black';
        ctx.lineWidth = size;
        ctx.lineCap = 'round';
        
        ctx.beginPath();
        ctx.moveTo(lastX, lastY);  // Start from the previous position
        ctx.lineTo(e.offsetX, e.offsetY);  // Draw to the current position
        ctx.stroke();

        lastX = e.offsetX;  // Update last position
        lastY = e.offsetY;
      }
    });

    // Handler to update the brush size
    Shiny.addCustomMessageHandler('updateSize', function(newSize) {
      size = newSize;
    });

    // Handler to clear the canvas
    /*Shiny.addCustomMessageHandler('clearCanvas', function() {
      console.log('clearCanvas handler triggered');
      ctx.clearRect(0, 0, canvas.width, canvas.height);
    });*/
    
    function clearCanvas(){
      ctx.clearRect(0, 0, canvas.width, canvas.height);
    }
    
    function getPixelData() {
      var imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
      var pixelArray = Array.from(imageData.data);  // Convert the Uint8ClampedArray to a regular array
      Shiny.setInputValue('canvasPixels', pixelArray);  // Send pixel data to R
    }
  "))
)

server <- function(input, output, session) {
  observe({
    size <- input$size
    session$sendCustomMessage("updateSize", size)
  })
  
  observeEvent(input$clearBtn, {
    runjs("clearCanvas();")
  })
  
  observeEvent(input$predictBtn, {
    runjs("getPixelData();");
  })
  
  observeEvent(input$canvasPixels, {
    # The input$canvasPixels contains the raw pixel data
    raw_data <- input$canvasPixels
    # Example: Print the first few values of the pixel data
    #print(head(raw_data))  # Display the first few values in the R console
    #output$prediction <- renderText({
    #  paste("Pixel data length:", length(raw_data))  # Display the pixel data length in the UI
    #})
    
    raw_data <- c(
      raw_data[seq(1, length(raw_data), 4)], 
      raw_data[seq(1, length(raw_data), 4)+1],
      raw_data[seq(1, length(raw_data), 4)+2],
      raw_data[seq(1, length(raw_data), 4)+3]
    )
    
    img_matrix <- array(raw_data, dim = c(600, 600, 4))
    # crop
    img_matrix <- img_matrix[, apply(img_matrix[,,4], 2, FUN=function(x){sum(x)>0}), ]
    img_matrix <- img_matrix[apply(img_matrix[,,4], 1, FUN=function(x){sum(x)>0}), , ]
    
    img_matrix <- img_matrix / 255
    
    img <- magick::image_read(img_matrix)
    #image_write(img, path="test_cropped.png", format="png")
    
    resized_img <- magick::image_resize(img, "20x20")
    #image_write(resized_img, path="test_resized.png", format="png")
    
    blank_img <- image_blank(28, 28)
    offset_x <- (28 - image_info(resized_img)$width) / 2
    offset_y <- (28 - image_info(resized_img)$height) / 2
    final_img <- image_composite(blank_img, resized_img, operator = "over",
                                 offset = paste0("+", offset_x, "+", offset_y))
    #image_write(final_img, path="test_final.png", format="png")
    
    
    final_array <- as.numeric(magick::image_data(final_img))
    final_array <- array(final_array, dim = c(28, 28, 4))
    
    grayscale_array <- final_array[,,4]
    
    data <- as.vector(grayscale_array)
    #saveRDS(data, file="test")
    
    pixels <- data.frame(data * 255) %>% t() %>% data.frame()
    colnames(pixels) <- paste0("pixel", 0:(28*28-1))
    predicted_digit <- predict(model.knn, newdata = pixels, type="class")
    #print(predicted_digit[1])
    print(predict(model.knn, newdata = pixels, type="prob"))
    
    output$prediction <- renderText({
      paste("Predicted digit:", predicted_digit[1])
    })
  })
}

shinyApp(ui, server)




