library(shiny)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(shinyWidgets)

# User Interface 
ui <- fluidPage(
    titlePanel("Generator Behaviour"),
    sidebarLayout(position = "left",
                  sidebarPanel("sidebar panel", # Where all the filter options go 
                               pickerInput(inputId = "Region", label = "Select a Region(s)", choices=c(unique(bid_merged$Region)), options = list(`actions-box` = TRUE),multiple = TRUE),
                               pickerInput(inputId = "Fuel_Type", label = "Select a Fuel_Type(s)", choices=c(unique(bid_merged$Fuel_Type)), options = list(`actions-box` = TRUE),multiple = TRUE),
                               pickerInput(inputId = "Generator", label = "Select a Generator", choices=c(unique(bid_merged$DUID)), options = list(`actions-box` = TRUE),multiple = TRUE),
                               pickerInput(inputId = "Year", label = "Select a Year(s)", choices=c(unique(bid_merged$Year)), options = list(`actions-box` = TRUE),multiple = TRUE),
                               pickerInput(inputId = "Month_name", label = "Select a Month(s)", choices=c(unique(bid_merged$Month_name)), options = list(`actions-box` = TRUE),multiple = TRUE),
                               pickerInput(inputId = "Day_name", label = "Select a Day(s)", choices=c(unique(bid_merged$Day_name)), options = list(`actions-box` = TRUE),multiple = TRUE),
                               pickerInput(inputId = "Hour", label = "Select an Hour(s)", choices=c(unique(bid_merged$Hour)), options = list(`actions-box` = TRUE),multiple = TRUE),
                               actionButton(inputId = "go", label = "Update")
                  ),
                  mainPanel( # Where all the plots go 
                      tabsetPanel( # Creates tabs 
                          tabPanel("Priceband", #Tab 1 
                                   fluidRow(
                                       splitLayout(cellWidths = c("50%", "50%"), class = "well",
                                                   plotOutput("PCA_pb", width="400px",height="500px",
                                                              brush = brushOpts("plot_brush_pb", resetOnNew = TRUE),
                                                              dblclick = "zoom_pb"), # PCA Plot
                                                   plotOutput("hist_pb", width="400px",height="500px") # Price Plot
                                       ),
                                       column(12,textOutput('unique_DUID_pb'), class = "well"), # Brush Output of unique DUID
                                       column(12,dataTableOutput('info_pb'), class = "well") # Brush Output
                                   )
                          ), 
                          tabPanel("BandAvail", # Tab 2 
                                   fluidRow(
                                       splitLayout(cellWidths = c("50%", "50%"), class = "well",
                                                   plotOutput("PCA_ba", width="400px",height="500px", 
                                                              brush = brushOpts("plot_brush_ba", resetOnNew = TRUE),
                                                              dblclick = "zoom_ba"), # PCA Plot
                                                   plotOutput("hist_ba", width="400px",height="500px") # Quantity Plot
                                       )
                                       , column(12, textOutput('unique_DUID_ba'), class = "well") # Brush Output of unique DUID
                                       , column(12,dataTableOutput('info_ba'), class = "well") # Brush Output
                                   )
                          ),
                          tabPanel("Percentiles", # Tab 3
                                   fluidRow( 
                                       plotOutput("PCA_Percentiles", brush = "brush_Percentiles", width="500px",height="500px"),
                                       column(12, textOutput('unique_DUID_percentiles'), class = "well"), # Brush Output of unique DUID
                                       column(12,dataTableOutput('brush_percentiles_Table'), class = "well"), # Brush Output
                                   )
                                   
                                   )
                      )
                  )
    )
)

# Logic
server <- function(input, output, session) {
    
    # data() Returns Row_index vector which is a vector that returns all the indexes of the selected rows in terms of 'bid_merged'
    data <- eventReactive(input$go, { 
        row_index <- as.numeric(rownames(bid_merged[bid_merged$DUID %in% input$Generator & 
                                                        bid_merged$Year %in% input$Year & 
                                                        bid_merged$Month_name %in% input$Month_name & 
                                                        bid_merged$Day_name %in% input$Day_name &
                                                        bid_merged$Hour %in% input$Hour &
                                                        bid_merged$Region %in% input$Region &
                                                        bid_merged$Fuel_Type %in% input$Fuel_Type,]
        )
        )
        return(row_index)
    }) 
    
    data_percent <- eventReactive(input$go, { 
            row_index <- as.numeric(rownames(bid_merged_percentiles[bid_merged_percentiles$DUID %in% input$Generator & 
                                                        bid_merged_percentiles$Year %in% input$Year & 
                                                        bid_merged_percentiles$Month_name %in% input$Month_name & 
                                                        bid_merged_percentiles$Day_name %in% input$Day_name &
                                                        bid_merged_percentiles$Hour %in% input$Hour &
                                                        bid_merged_percentiles$Region %in% input$Region &
                                                        bid_merged_percentiles$Fuel_Type %in% input$Fuel_Type,]
        )
        )
        return(row_index)
    }) 
    
    # all_points() Returns a table of all the observations from bid_merged that satisfy the dropdown selected conditions.
    # In other words, all the points that are shown on the PCA chart.
    all_points <- eventReactive(input$go, {
        all_pts <- filter(bid_merged[bid_merged$DUID %in% input$Generator & 
                                         bid_merged$Year %in% input$Year & 
                                         bid_merged$Month_name %in% input$Month_name & 
                                         bid_merged$Day_name %in% input$Day_name &
                                         bid_merged$Hour %in% input$Hour &
                                         bid_merged$Region %in% input$Region &
                                         bid_merged$Fuel_Type %in% input$Fuel_Type,])
        return(all_pts)
    })
    
    # given two maximum values, function returns the largest value out of the two 
    limits <- function(x,y) { 
        if(x > y) {return(x)} 
        else {return(y)}
    } 
    
    # Given a vector of numerics, it returns the largest absolute value of the list 
    longest_dist <- function(v) {
        l_dist = max(abs(v))
        return(l_dist)
    }

    ranges <- reactiveValues(x = c(-10,10), y = c(-10,10))
    
    # Percentiles PCA Plot 
    output$PCA_Percentiles <- renderPlot({
        range <- limits(max(abs(percentiles.pca$x[data_percent(),1])), max(abs(percentiles.pca$x[data_percent(),2])))
        
        # PCA Plot
        plot(percentiles.pca$x[data_percent(),1], percentiles.pca$x[data_percent(),2],
             xlab = paste("PC 1 (", round(summary_percentiles$importance[2]*100,1), "%)", sep = ""), 
             ylab = paste("PC 2 (", round(summary_percentiles$importance[5]*100,1), "%)", sep = ""), 
             xlim = c(-range,range), ylim = c(-range,range), 
             main = "PCA Plot Percentiles",
             col="black",
        )
        
        # Add grid lines
        abline(v=0, lty=2, col="grey50")
        abline(h=0, lty=2, col="grey50")
        
        # Get co-ordinates of variables (loadings) and multiply by a scalar for the arrows 
        l.x <- percentiles.pca$rotation[,1]*range
        l.y <- percentiles.pca$rotation[,2]*range
        
        # Draw arrows
        arrows(x0=0, x1=l.x, y0=0, y1=l.y, col="red", length=0.15, lwd=1.5)
        
        # Label position of arrows 
        l.pos <- l.y # Creates a vector of y axis coordinates 
        lo <- which(l.y < 0) # Get the variables on the bottom half of the plot
        hi <- which(l.y > 0) # Get variables on the top half of the plot 
        
        # Replace values in the vector
        l.pos <- replace(l.pos, lo, "1")
        l.pos <- replace(l.pos, hi, "3")
        
        # Variable labels
        text(l.x, l.y, labels= substr(row.names(percentiles.pca$rotation), 7, 12), col="red", pos=l.pos)    
    })
    
    # List of DUIDs in brushed area 
    output$unique_DUID_percentiles <- renderText({
        brushed_pts <- bid_merged_percentiles[data_percent(), ] # selects just DUID only
        paste("Generators in selected area:") 
        unique(brushedPoints(brushed_pts, input$brush_Percentiles, xvar = "PC1", yvar = "PC2")[,1])
    })
    
    # Table Output for Percentiles
    output$brush_percentiles_Table <- renderDataTable({
        brushed_pts <- bid_merged_percentiles[data_percent(), c(1:2, 14:19)]
        brushedPoints(brushed_pts, input$brush_Percentiles, xvar = "PC1", yvar = "PC2")
    })
    
    # PCA Plot for Priceband
    output$PCA_pb <- renderPlot({ 
            range <- limits(max(abs(p$x[data(),1])), max(abs(p$x[data(),2])))
            par(mar=c(5, 4, 4, 8), xpd = TRUE) # Set aside area for the legend
            
            # PCA Plot
            plot(p$x[data(),1], p$x[data(),2],
                 xlab = paste("PC 1 (", round(sp$importance[2]*100,1), "%)", sep = ""), 
                 ylab = paste("PC 2 (", round(sp$importance[5]*100,1), "%)", sep = ""), 
                 #xlim = c(-range,range), ylim = c(-range,range), 
                 xlim = ranges$x, ylim = ranges$y,
                 main = "PCA Plot for chosen Generator(s)",
                 col="black",
                 pch = pch.group[data()],
                 bg = col.group[data()]
            )
            
            # Add grid lines
            abline(v=0, lty=2, col="grey50")
            abline(h=0, lty=2, col="grey50")
            
            # Get co-ordinates of variables (loadings) and multiply by a scalar for the arrows 
            l.x <- p$rotation[,1]*range
            l.y <- p$rotation[,2]*range
            
            # Draw arrows
            arrows(x0=0, x1=l.x, y0=0, y1=l.y, col="red", length=0.15, lwd=1.5)
            
            # Label position of arrows 
            l.pos <- l.y # Creates a vector of y axis coordinates 
            lo <- which(l.y < 0) # Get the variables on the bottom half of the plot
            hi <- which(l.y > 0) # Get variables on the top half of the plot 
            
            # Replace values in the vector
            l.pos <- replace(l.pos, lo, "1")
            l.pos <- replace(l.pos, hi, "3")
            
            # Variable labels
            text(l.x, l.y, labels= substr(row.names(p$rotation), 6, 11), col="red", pos=l.pos)
            
            # Legend for the Years (colours)
            legend("topright", 
                   inset=c(-0.2,0),
                   legend=c(unique(bid_merged$Year)), 
                   col="black", 
                   pt.bg=c(colVec), 
                   pch=c(rep(21,times = 12)), pt.cex=1.5)
        }) 
    
    observeEvent(input$zoom_pb, {
        brush <- input$plot_brush_pb
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- c(-limits(max(abs(p$x[data(),1])), max(abs(p$x[data(),2]))),
                          limits(max(abs(p$x[data(),1])), max(abs(p$x[data(),2]))))
            ranges$y <- c(-limits(max(abs(p$x[data(),1])), max(abs(p$x[data(),2]))),
                          limits(max(abs(p$x[data(),1])), max(abs(p$x[data(),2]))))
        }
    })
    
    # Table output for Priceband
    output$info_pb <- renderDataTable({ 
        brushed_pts <- bid_merged_pb[data(), ] # so the output is only pricebands and doesn't include bandavails (incl DUID, etc...)
        brushedPoints(brushed_pts, input$plot_brush_pb, xvar = "PC1", yvar = "PC2")
    })  
    
    # Price Histogram
    output$hist_pb <- renderPlot({
        brushed_pts <- bid_merged_pb[data(), ]
        brushed_table <- brushedPoints(brushed_pts, input$plot_brush_pb, xvar = "PC1", yvar = "PC2") 
        
        point_all <- all_points() 
        
        bands <-  c(rep('PB1', 3), rep('PB2', 3), rep('PB3', 3), rep('PB4', 3), rep('PB5', 3),
                    rep('PB6', 3), rep('PB7', 3), rep('PB8', 3), rep('PB9', 3), rep('PB10', 3))
        
        legend <- c(rep(c('avg price selected obs', 'avg price plotted obs', 'avg price all obs'), 10))
        
        value <- c(mean(brushed_table$PRICEBAND1), mean(point_all$PRICEBAND1), mean(bid_merged$PRICEBAND1),
                   mean(brushed_table$PRICEBAND2), mean(point_all$PRICEBAND2), mean(bid_merged$PRICEBAND2),
                   mean(brushed_table$PRICEBAND3), mean(point_all$PRICEBAND3), mean(bid_merged$PRICEBAND3),
                   mean(brushed_table$PRICEBAND4), mean(point_all$PRICEBAND4), mean(bid_merged$PRICEBAND4),
                   mean(brushed_table$PRICEBAND5), mean(point_all$PRICEBAND5), mean(bid_merged$PRICEBAND5),
                   mean(brushed_table$PRICEBAND6), mean(point_all$PRICEBAND6), mean(bid_merged$PRICEBAND6),
                   mean(brushed_table$PRICEBAND7), mean(point_all$PRICEBAND7), mean(bid_merged$PRICEBAND7),
                   mean(brushed_table$PRICEBAND8), mean(point_all$PRICEBAND8), mean(bid_merged$PRICEBAND8),
                   mean(brushed_table$PRICEBAND9), mean(point_all$PRICEBAND9), mean(bid_merged$PRICEBAND9),
                   mean(brushed_table$PRICEBAND10), mean(point_all$PRICEBAND10), mean(bid_merged$PRICEBAND10))
        
        hist_data <- data.frame(bands, legend, value)
        
        ggplot(hist_data, aes(fill=legend, y=value, x= reorder(bands, seq.int(nrow(hist_data))))) + 
            geom_bar(position="dodge", stat="identity") +
            labs(y= "Price per Mwh ($)", x = "Price Bands") + 
            ggtitle("Price Plot for chosen observation") + 
            theme(plot.title=element_text(size=14,face="bold")) +
            theme(legend.position="bottom")
    }) 
    
    # List of DUIDs in brushed area 
    output$unique_DUID_pb <- renderText({
        brushed_pts <- bid_merged_pb[data(), ] # selects just DUID only
        paste("Generators in selected area:") 
        unique(brushedPoints(brushed_pts, input$plot_brush_pb, xvar = "PC1", yvar = "PC2")[,1])
    })
    
    #PCA Plot for BandAvavil
    output$PCA_ba <- renderPlot({ 
        range <- limits(max(abs(b$x[data(),1])), max(abs(b$x[data(),2]))) # Range and domain for the PCA plot 
        
        par(mar=c(5, 4, 4, 8), xpd = TRUE) # Set aside area for the legend
        
        # PCA Plot
        plot(b$x[data(),1], b$x[data(),2],
             xlab = paste("PC 1 (", round(sb$importance[2]*100,1), "%)", sep = ""), 
             ylab = paste("PC 2 (", round(sb$importance[5]*100,1), "%)", sep = ""), 
             xlim = ranges$x, ylim = ranges$y, 
             main = "PCA Plot for chosen Generator(s)",
             col="black",
             pch = pch.group[data()],
             bg = col.group[data()]
        )
        
        # Add grid lines
        abline(v=0, lty=2, col="grey50")
        abline(h=0, lty=2, col="grey50")
        
        # Get co-ordinates of variables (loadings) and multiply by a scalar for the arrows 
        l.x <- b$rotation[,1]*(range + 1)
        l.y <- b$rotation[,2]*(range + 1)
        
        # Draw arrows
        arrows(x0=0, x1=l.x, y0=0, y1=l.y, col="red", length=0.15, lwd=1.5)
        
        # Label position of arrows 
        l.pos <- l.y # Creates a vector of y axis coordinates 
        lo <- which(l.y < 0) # Get the variables on the bottom half of the plot
        hi <- which(l.y > 0) # Get variables on the top half of the plot 
        
        # Replace values in the vector
        l.pos <- replace(l.pos, lo, "1")
        l.pos <- replace(l.pos, hi, "3")
        
        # Variable labels
        text(l.x, l.y, labels= substr(row.names(b$rotation), 6, 11), col="red", pos=l.pos)
        
        # Legend for the Years (colours)
        legend("topright", 
               inset=c(-0.2,0),
               legend=c(unique(bid_merged$Year)), 
               col="black", 
               pt.bg=c(colVec), 
               pch=c(rep(21,times = 12)), pt.cex=1.5)
        
    }) 
    
    observeEvent(input$zoom_ba, {
        brush <- input$plot_brush_ba
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- c(-limits(max(abs(b$x[data(),1])), max(abs(b$x[data(),2]))),
                          limits(max(abs(b$x[data(),1])), max(abs(b$x[data(),2]))))
            ranges$y <- c(-limits(max(abs(b$x[data(),1])), max(abs(b$x[data(),2]))),
                          limits(max(abs(b$x[data(),1])), max(abs(b$x[data(),2]))))
        }
    })
    
    # Table output for BandAvail
    output$info_ba <- renderDataTable({ 
        brushed_pts <- bid_merged_ba[data(), ] # so the output is only bandavail and doesn't include pricebands (incl DUID, etc...)
        brushedPoints(brushed_pts, input$plot_brush_ba, xvar = "PC1", yvar = "PC2")
    }) 
    
    # Quantity Histogram
    output$hist_ba <- renderPlot({ 
        brushed_pts <- bid_merged_ba[data(), ]
        brushed_table <-  brushedPoints(brushed_pts, input$plot_brush_ba, xvar = "PC1", yvar = "PC2") 
        
        # point <- brushed_table[1,]
        point_all <- all_points()
        # point_all <- distinct(filter(bid_merged, INTERVAL_DATETIME %in% brushed_table[,12]))
        
        bands <-  c(rep('BA1', 3), rep('BA2', 3), rep('BA3', 3), rep('BA4', 3), rep('BA5', 3),
                    rep('BA6', 3), rep('BA7', 3), rep('BA8', 3), rep('BA9', 3), rep('BA10', 3))
        
        legend <- c(rep(c('avg quant selected obs', 'avg quant plotted obs', 'avg quant all obs'), 10))
        
        value <- c(mean(brushed_table$BANDAVAIL1), mean(point_all$BANDAVAIL1), mean(bid_merged$BANDAVAIL1),
                   mean(brushed_table$BANDAVAIL2), mean(point_all$BANDAVAIL2), mean(bid_merged$BANDAVAIL2),
                   mean(brushed_table$BANDAVAIL3), mean(point_all$BANDAVAIL3), mean(bid_merged$BANDAVAIL3),
                   mean(brushed_table$BANDAVAIL4), mean(point_all$BANDAVAIL4), mean(bid_merged$BANDAVAIL4),
                   mean(brushed_table$BANDAVAIL5), mean(point_all$BANDAVAIL5), mean(bid_merged$BANDAVAIL5),
                   mean(brushed_table$BANDAVAIL6), mean(point_all$BANDAVAIL6), mean(bid_merged$BANDAVAIL6),
                   mean(brushed_table$BANDAVAIL7), mean(point_all$BANDAVAIL7), mean(bid_merged$BANDAVAIL7),
                   mean(brushed_table$BANDAVAIL8), mean(point_all$BANDAVAIL8), mean(bid_merged$BANDAVAIL8),
                   mean(brushed_table$BANDAVAIL9), mean(point_all$BANDAVAIL9), mean(bid_merged$BANDAVAIL9),
                   mean(brushed_table$BANDAVAIL10), mean(point_all$BANDAVAIL10), mean(bid_merged$BANDAVAIL10))
        
        hist_data <- data.frame(bands, legend, value)
        
        ggplot(hist_data, aes(fill=legend, y=value, x= reorder(bands, seq.int(nrow(hist_data))))) + 
            geom_bar(position="dodge", stat="identity") +
            labs(y= "Quantity (mwh)", x = "Band Available") +
            ggtitle("Quantity Plot for chosen observation") + 
            theme(plot.title=element_text(size=14,face="bold")) +
            theme(legend.position="bottom")
    }) 
    
    # List of DUIDs in brushed area
    output$unique_DUID_ba <- renderText({
        brushed_pts <- bid_merged_ba[data(),] # selects just DUID only
        paste("Generators in selected area:") 
        unique(brushedPoints(brushed_pts, input$plot_brush_ba, xvar = "PC1", yvar = "PC2")[,1])
    })
    
    # Conditionally updates the choices for 'Fuel_Type' given Region_ID8 
    observe({updatePickerInput(session, 
                               inputId="Fuel_Type", 
                               choices=unique(bid_merged[bid_merged$Region %in% input$Region,"Fuel_Type"])
    )
    }) 
    
    # Conditionally updates the choices for 'Generator' 
    observe({updatePickerInput(session,
                               inputId="Generator", 
                               choices=unique(bid_merged[bid_merged$Region %in% input$Region &
                                                             bid_merged$Fuel_Type %in% input$Fuel_Type,"DUID"])
    )
    })  
    
    # Conditionally updates the choices for 'Year' 
    observe({updatePickerInput(session,
                               inputId="Year", 
                               choices=unique(bid_merged[bid_merged$Region %in% input$Region &
                                                             bid_merged$Fuel_Type %in% input$Fuel_Type &
                                                             bid_merged$DUID %in% input$Generator,"Year"])
    )
    })  
    
    # Conditionally updates the choices for 'Month_name' 
    observe({updatePickerInput(session, 
                               inputId="Month_name", 
                               choices=unique(bid_merged[bid_merged$Region %in% input$Region &
                                                             bid_merged$Fuel_Type %in% input$Fuel_Type &
                                                             bid_merged$DUID %in% input$Generator &
                                                             bid_merged$Year %in% input$Year,"Month_name"])
    )
    }) 
    
    # Conditionally updates the choices for 'Day_name' 
    observe({updatePickerInput(session, # Conditionally updates the choices for 'Day_name' dropdown options given the 'Generator' + 'Year' + 'Month_name'
                               inputId="Day_name", 
                               choices=unique(bid_merged[bid_merged$Region %in% input$Region &
                                                             bid_merged$Fuel_Type %in% input$Fuel_Type &
                                                             bid_merged$DUID %in% input$Generator &
                                                             bid_merged$Year %in% input$Year & 
                                                             bid_merged$Month_name %in% input$Month_name,"Day_name"])
    )
    }) 
    
    # Conditionally updates the choices for 'Hour' 
    observe({updatePickerInput(session, 
                               inputId="Hour", 
                               choices=unique(bid_merged[bid_merged$Region %in% input$Region &
                                                             bid_merged$Fuel_Type %in% input$Fuel_Type &
                                                             bid_merged$DUID %in% input$Generator &
                                                             bid_merged$Year %in% input$Year & 
                                                             bid_merged$Month_name %in% input$Month_name &
                                                             bid_merged$Day_name %in% input$Day_name,"Hour"])
    )
    }) 
}

# Run the application 
shinyApp(ui = ui, server = server)

