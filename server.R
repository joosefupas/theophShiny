# server.R
library(shiny)
library(deSolve)
library(ggplot2)
library(plyr)

server <- function(input, output) {
  simulatePK <- eventReactive(input$simulate, {
    # Parameters
    ka <- 1.51
    CL <- input$CL
    V <- input$V
    dose <- input$dose
    
    kel <- CL / V
    
    # Time sequence
    times <- seq(0, 24, by = 0.1)
    
    # Initial conditions
    init <- c(Ad = dose, Ac = 0) # Starting dose in Ad compartment
    
    # Differential equations
    pkModel <- function(time, state, parameters) {
      with(as.list(c(state, parameters)), {
        dAd <- -ka * Ad
        dAc <- ka * Ad - kel * Ac
        list(c(dAd, dAc))
      })
    }
    
    # Fixed effects
    parameters <- c(ka = ka, kel = kel)
    
    # Solve ODE without IIV
    noIIV <- ode(y = init, times = times, func = pkModel, parms = parameters)
    
    if (input$includeIIV) {
      # Include inter-individual variability (IIV)
      omega_CL <- 0.26
      omega_V <- 0.12
      
      CL_i <- rlnorm(input$n, meanlog = log(CL), sdlog = omega_CL)
      V_i <- rlnorm(input$n, meanlog = log(V), sdlog = omega_V)
      
      profiles <- lapply(1:input$n, function(i) {
        kel_i <- CL_i[i] / V_i[i]
        params_i <- c(ka = ka, kel = kel_i)
        ode(y = init, times = times, func = pkModel, parms = params_i)
      })
      
      # Calculate median and confidence intervals
      Ac_values <- do.call(cbind, lapply(profiles, function(x) x[, "Ac"] / V))
      stats <- data.frame(
        time = times,
        median = apply(Ac_values, 1, median),
        low = apply(Ac_values, 1, quantile, probs = 0.025),
        high = apply(Ac_values, 1, quantile, probs = 0.975)
      )
      
      return(list(noIIV = noIIV, stats = stats))
    }
    
    return(list(noIIV = noIIV))
  })
  
  output$pkPlot <- renderPlot({
    result <- simulatePK()
    times <- result$noIIV[, "time"]
    
    plot(result$noIIV[, "time"], result$noIIV[, "Ac"] / input$V, type = "l",
         xlab = "Time (h)", ylab = "Concentration (mg/L)", col = "blue", lwd = 2,
         main = "Theophylline PK Profiles")
    legend("topright", legend = c("Typical"), col = c("blue"), lwd = 2)
    
    if (input$includeIIV) {
      stats <- result$stats
      lines(stats$time, stats$median, col = "red", lwd = 2)
      polygon(c(stats$time, rev(stats$time)), c(stats$low, rev(stats$high)),
              col = rgb(1, 0, 0, 0.2), border = NA)
      legend("topright", legend = c("Typical", "Median (IIV)", "95% CI (IIV)"),
             col = c("blue", "red", rgb(1, 0, 0, 0.2)), lwd = c(2, 2, NA), pch = c(NA, NA, 15))
    }
  })
}

shinyApp(ui = ui, server = server)
